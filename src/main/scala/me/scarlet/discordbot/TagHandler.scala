package me.scarlet.discordbot

import java.io.{File, FileOutputStream}
import java.util.regex.{Matcher, Pattern}

import scala.collection.mutable.{Map ⇒ MMap, Set ⇒ MSet}
import scala.io.Source
import scala.util.control.NonFatal

import io.circe.parser.decode
import io.circe.syntax._
import me.scarlet.discordbot.util.BotUtil._
import me.scarlet.discordbot.util.Messager
import me.scarlet.discordbot.util.Messager._
import me.scarlet.discordbot.DiscordBot.log
import me.scarlet.discordbot.commands.{Command, CommandContext, CommandHandler, ParentCommand}
import net.dv8tion.jda.core.MessageBuilder
import net.dv8tion.jda.core.entities.{Message, MessageChannel, User}
import net.dv8tion.jda.core.events.message.{MessageReceivedEvent, MessageUpdateEvent}
import net.dv8tion.jda.core.hooks.ListenerAdapter

/**
  * Handles the text-replacement system for the bot.
  */
object TagHandler extends ListenerAdapter {

    // tag data management
    private var tags: MMap[String, String] = null
    private val tagFile = new File("tags.json")
    private val tagReg = "[^\\\\]?(\\{(.+?[^\\\\])\\})".r
    // https://gist.github.com/dperini/729294 -- slightly modified so we dont find already-escaped links
    private val urlReg = "(?<!\\<)(?i)(?:(?:https?|ftp)://)(?:\\S+(?::\\S*)?@)?(?:(?!(?:10|127)(?:\\.\\d{1,3}){3})(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3})" +
        "{2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\." +
        "(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)(?:\\.(?:[a-z\\u00a1-\\uffff0-9]-*)" +
        "*[a-z\\u00a1-\\uffff0-9]+)*(?:\\.(?:[a-z\\u00a1-\\uffff]{2,}))\\.?)(?::\\d{2,5})?(?:[/?#]\\S*)?(?!\\>)"

    def getTag(key: String): Option[String] = tags.get(key)

    def setTag(key: String, tag: Option[String]): Option[String] = {
        if (tag.isDefined) tags.put(key, tag.get) else tags.remove(key)
    }

    def saveTags(): Unit = {
        try {
            log.info(s"Saving tags file to ${tagFile.getName}")
            val writer = new FileOutputStream(tagFile)
            writer.write(tags.toMap.asJson.noSpaces.getBytes("UTF-16"))
            writer.close()
        } catch {
            case NonFatal(e) ⇒ {
                log.warn("Couldn't save tags file")
                log.warn(stacktraceAsString(e))
            }
        }
    }

    def loadTags(): Unit = {
        if (tagFile.exists()) {
            log.info(s"Loading tags file from ${tagFile.getName}")
            try {
                decode[Map[String, String]](Source.fromFile(tagFile, "UTF-16").getLines().mkString) match {
                    case Right(c) ⇒ {
                        tags = MMap[String, String](c.toSeq: _*)
                        log.info(s"${tags.size} tag(s) loaded")
                    }
                    case Left(e) ⇒ throw e
                }
            } catch {
                case NonFatal(_) ⇒ {
                    val format = "tags-%d.broken"

                    // generate our broken tag filename
                    var i = 0
                    var file = new File(format.format(i))
                    while (file.exists()) {
                        i += 1
                        file = new File(format.format(i))
                    }

                    tagFile.renameTo(file)
                    log.warn(s"Tags file is corrupted! Creating new tags file and storing old one in file ${file.getName}")
                    tags = MMap[String, String]()
                    saveTags()
                }
            }
        } else {
            tags = MMap[String, String]()
            saveTags()
        }
    }

    // actual tag handling

    private val edited = MSet[String]()

    override def onMessageReceived(event: MessageReceivedEvent): Unit = applyTags(event.getMessage)

    override def onMessageUpdate(event: MessageUpdateEvent): Unit = applyTags(event.getMessage)

    def applyTags(msg: String): (Int, String) = {
        var i = 0
        var content = msg
        tagReg.findAllMatchIn(content).map(_.group(2)).toSeq.distinct.foreach(m ⇒ {
            getTag(m).foreach(value ⇒ {
                content = content.replaceFirst(Pattern.quote(s"{$m}"), Matcher.quoteReplacement(value))
                i += 1
            })
        })

        (i, content)
    }

    def applyTags(msg: Message): Unit = {
        // command handler'll process tags itself
        if (!CommandHandler.hasInvoker(msg.getRawContent)) {
            if (edited.contains(msg.getId)) {
                edited.remove(msg.getId)
            } else if (msg.getAuthor == DiscordBot.jda.getSelfUser) {
                val content = applyTags(msg.getRawContent)
                val msgr = Messager(msg)
                if (content._1 > 0) {
                    if (msg.getRawContent != content._2) {
                        edited.add(msg.getId) // prevent reprocessing
                        if (content._2.length > 2000) {
                            log.warn(s"Tag replacement failed in last message due to exceeding character limit")
                            msgr.error("couldn't replace tags: resulting message was over 2,000 characters").queue()
                        } else {
                            log.info(s"Replaced ${content._1} unique tag(s) in last message")
                            msgr.content = content._2
                            msgr.update.foreach(_.queue())
                        }
                    }
                }
            }
        }
    }

    // command

    object TagCommand extends ParentCommand(
        DeleteCommand,
        ReloadCommand,
        ListCommand,
        SetCommand
    ) {
        override def name = "tag"

        override def desc = "Tag management."

        override def syntax = Array("**tag**")
    }

    object ListCommand extends Command {
        override def name = "list"

        override def desc = "List currently loaded tags."

        override def syntax = Array("**tag list**")

        override def run(context: CommandContext): Unit = {
            context.msg.embed(responseTemplate(tags.keys.mkString(", ")).setTitle("List of known tags", null).build).queue()
        }
    }

    object ReloadCommand extends Command {
        override def name = "reload"

        override def desc = "Reload tags from disk."

        override def syntax = Array("**tag reload**")

        override def run(context: CommandContext): Unit = {
            loadTags()
            context.msg.respond("Tags reloaded.").queue()
        }
    }

    object SetCommand extends Command {
        override def name = "set"

        override def desc = "Set a tag."

        override def rawargs = true

        override def syntax = Array("**tag set** <key> <value>")

        override def run(context: CommandContext): Unit = {
            val msg = context.msg

            try {
                val tag = context.args.head
                val content = context.args.drop(1).mkString(" ").trim
                if (content.isEmpty) {
                    msg.error("Tag content cannot be empty.").queue()
                } else {
                    msg.content = msg.content.replaceAll(urlReg, "<$0>")
                    setTag(tag, Option(content)) match {
                        case Some(e) ⇒ msg.respond(s"replaced tag $tag (previously `$e`)").queue()
                        case None ⇒ msg.respond(s"set tag $tag").queue()
                    }

                    saveTags()
                }
            } catch {
                case _: ArrayIndexOutOfBoundsException ⇒ msg.error("Usage: tag set <id> <content>").queue()
            }
        }
    }

    object DeleteCommand extends Command {
        override def name = "delete"

        override def desc = "Delete a tag. Or a few."

        override def syntax = Array("**tag delete** [space-separated tags]")

        override def run(context: CommandContext): Unit = {
            val msg = context.msg
            val args = context.args
            if (args.length < 1) msg.error("Usage: tag delete <tag names>").queue() else {
                val notfound = MSet[String]()
                args.foreach(tag ⇒ {
                    if (getTag(tag).isDefined) {
                        setTag(tag, None)
                    } else {
                        notfound.add(tag)
                    }
                })

                msg.respond(if (notfound.size <= 0) ":ok_hand:"
                    else s"Some tags didn't exist (${notfound.mkString(", ")}), but the ones that did were deleted.").queue()
                saveTags()
            }
        }
    }

}
