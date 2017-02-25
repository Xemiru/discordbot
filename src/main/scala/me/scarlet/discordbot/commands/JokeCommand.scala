package me.scarlet.discordbot.commands

import com.mashape.unirest.http.async.Callback
import com.mashape.unirest.http.exceptions.UnirestException
import com.mashape.unirest.http.{HttpResponse, Unirest}
import me.scarlet.discordbot.DiscordBot.log
import me.scarlet.discordbot.util.BotUtil._

object JokeCommand extends Command {

    private val API = "https://icanhazdadjoke.com/"

    override def name = "joke"

    override def desc = "Tell a random joke."

    override def extDesc = "Jokes provided by [icanhazdadjoke.com](https://icanhazdadjoke.com/about)."

    override def syntax = Array("**joke**")

    override def preserved = true

    override def run(context: CommandContext): Unit = {
        val msg = context.msg
        Unirest.get(API).header("Accept", "text/plain").asStringAsync(new Callback[String]() {
            override def completed(response: HttpResponse[String]): Unit = {
                if (response.getStatus != 200) msg.error(s"Server responded unsuccessfully: ${response.getStatusText}").queue()
                else msg.respond(s"${response.getBody} :rofl:").queue()
            }

            override def cancelled(): Unit = {
                msg.error("Request was cancelled.").queue()
            }

            override def failed(e: UnirestException): Unit = {
                log.warn(s"Request to $API failed: ${e.getMessage}\n${stacktraceAsString(e)}")
                msg.error(s"Request failed: ${e.getMessage}").queue()
            }
        })
    }

}
