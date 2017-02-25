package me.scarlet.discordbot.commands

import java.io.File

import scala.util.control.NonFatal

import com.mashape.unirest.http.async.Callback
import com.mashape.unirest.http.exceptions.UnirestException
import com.mashape.unirest.http.{HttpResponse, JsonNode, Unirest}
import me.scarlet.discordbot.DiscordBot.{config, log}
import me.scarlet.discordbot.util.BotUtil._
import net.dv8tion.jda.core.entities.Message

object ImgurCommand extends Command {

    private val API = "https://api.imgur.com/3/image"
    private val LOCAL_FILE = new File("imgurupload")

    override def name = "imgur"

    override def desc = "Upload an image to imgur and get a direct link."

    override def extDesc = "Use this command in a message containing an image attachment."

    override def syntax = Array("**imgur**")

    private def apiToken: Option[String] = {
        val token = config.apis("imgur")
        if (token == null || token.isEmpty) None
        else Option(token)
    }

    private def cleanup(): Unit = {
        try if (LOCAL_FILE.exists) LOCAL_FILE.delete() catch {
            case NonFatal(e) ⇒ {
                log.warn(s"Failed to cleanup imgur upload")
                log.warn(stacktraceAsString(e))
            }
        }
    }

    override def run(context: CommandContext): Unit = {
        val token = apiToken
        val msg = context.msg
        val args = context.args
        if (token.isEmpty) throw CommandException("Configuration file does not define an Imgur API token.")

        try {
            val img = if (args.isEmpty) msg.msg.getAttachments.get(0) else args.head
            val att = img.isInstanceOf[Message.Attachment]

            if (att) {
                val attach = img.asInstanceOf[Message.Attachment]
                if (!attach.isImage) throw CommandException("Attachment must be an image file.")
                if (!attach.download(LOCAL_FILE)) throw CommandException("Failed to temporarily store image file locally for uploading.")
            }

            msg.respond("Uploading ...").complete()
            log.info("Imgur command is uploading a new image ...")

            val req = Unirest.post(API).header("Authorization", s"Client-ID ${token.get}")
            if (att) req.field("image", LOCAL_FILE) else req.field("image", img.toString)
            req.asJsonAsync(new Callback[JsonNode]() {

                def error(m: String): Unit = msg.error(m).queue()

                override def completed(response: HttpResponse[JsonNode]): Unit = {
                    response.getStatus match {
                        case 200 ⇒ msg.respond(response.getBody.getObject.getJSONObject("data").getString("link")).queue()
                        case 400 ⇒ error("Upload failed, Imgur didn't know what to do with your \"image.\" (400)")
                        case 401 ⇒ error("The Imgur API token in your configuration was not accepted. (401)")
                        case 403 ⇒ error("The Imgur API didn't accept our request. (403)")
                        case 404 ⇒ error("Imgur didn't know what we wanted. Probably Xemiru's fault, stab him. (404)")
                        case 429 ⇒ error("Imgur says we're poking it to do things too much. Slow down. (429)")
                        case 500 ⇒ error("Imgur screwed up on their side. They said sorry. (500)")
                        case status ⇒ error(s"Imgur told us something, but we have no idea what it is. ($status)")
                    }

                    log.info(s"Imgur responded with ${response.getStatus}: ${response.getStatusText}")
                    cleanup()
                }

                override def cancelled(): Unit = {
                    error("Upload was cancelled.")
                    cleanup()
                }

                override def failed(e: UnirestException): Unit = {
                    log.warn("Imgur upload failed.")
                    log.warn(stacktraceAsString(e))
                    error(s"Upload failed: ${e.getMessage}")
                    cleanup()
                }
            })
        } catch {
            case _: IndexOutOfBoundsException ⇒ throw CommandException("Pass a link to an image, or an image attachment to the command.")
            case NonFatal(e) ⇒ {
                // shut up intellij
                cleanup() // make sure this happens
                throw e
            }
        }
    }
}
