package me.scarlet.discordbot.commands

import java.util.concurrent.{ExecutorService, Executors}

import scala.util.control.NonFatal

import me.scarlet.discordbot.DiscordBot.{config, log}
import me.scarlet.discordbot.util.{BotUtil, Messager}
import me.scarlet.discordbot.{DiscordBot, TagHandler}
import net.dv8tion.jda.core.events.message.MessageReceivedEvent
import net.dv8tion.jda.core.hooks.ListenerAdapter

/**
  * Manages bot commands.
  */
object CommandHandler extends ListenerAdapter {

    /**
      * Root command, containing all main commands of the bot.
      */
    object RootCommand extends ParentCommand(
        // command registration
        TagHandler.TagCommand,

        IconCommand,
        ImgurCommand,
        JokeCommand,
        OverstatsCommand,
        ReloadCommand,
        ReverseCommand,
        SayCommand,
        ServerInfoCommand,
        SetGameCommand,
        SpaceCommand
    ) {
        // satisfy command impl
        override def name = "root"

        override def desc = "You shouldn't be seeing this."

        override def syntax = Array("Use an invoker.")

        // dont pop default help message if they don't do anything
        override def noSub(context: CommandContext): Unit = {}

        override def noArgs(context: CommandContext): Unit = {}
    }

    private val pool: ExecutorService = Executors.newCachedThreadPool()
    private var lastCmd: Option[CommandContext] = None

    /**
      * Returns whether or not the given string would activate processing by this [[me.scarlet.discordbot.commands.CommandHandler]].
      *
      * @param str the string to check
      *
      * @return if `str` starts with a command invoker
      */
    def hasInvoker(str: String): Boolean = config.invoker.exists(str.startsWith)

    /**
      * Listener handling commands.
      */
    override def onMessageReceived(event: MessageReceivedEvent): Unit = {
        if (event.getAuthor == DiscordBot.jda.getSelfUser || !config.selfbot) {
            val usr = event.getAuthor
            val msg = event.getMessage
            val content = TagHandler.applyTags(msg.getRawContent)
            val invokers = config.invoker.filter(e ⇒ content._2.toLowerCase.startsWith(e.toLowerCase))
            if (invokers.nonEmpty) {
                val args = content._2.substring(invokers.head.length).split(" ")
                val cmd = RootCommand.getCommand(config.aliases.getOrElse(args.head, args.head))

                // run the command if its there
                cmd.foreach(cmd ⇒ {
                    val cmdName = s"${cmd.name}:${cmd.getClass.getSimpleName}"
                    val context = CommandContext(usr, Messager(msg), args.drop(1): _*)
                    context.msg.content = content._2

                    // clean up the previous command
                    lastCmd.foreach(_.msg.clear().queue())
                    if (cmd.preserved) lastCmd = None else lastCmd = Option(context)

                    pool.execute(() ⇒ {
                        log.info(s"User ${usr.getName}#${usr.getDiscriminator} running command $cmdName")
                        if(content._1 > 0) log.info(s"Replaced ${content._1} unique tag(s) in command string")

                        try {
                            cmd.run(context)
                            context.msg.update.foreach(_.queue())
                        } catch {
                            case e: CommandException ⇒ {
                                log.warn(s"Command $cmdName failed: ${e.getMessage}")
                                context.msg.error(e.getMessage).queue()
                            }
                            case NonFatal(e) ⇒ {
                                log.warn(s"Command $cmdName failed exceptionally:")
                                log.warn(BotUtil.stacktraceAsString(e))

                                context.msg.error("The command ran into a bad error.").queue()
                            }
                        }
                    })
                })
            }
        }
    }
}
