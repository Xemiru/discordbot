package me.scarlet.discordbot.commands

import scala.util.control.NonFatal

import me.scarlet.discordbot.DiscordBot
import me.scarlet.discordbot.DiscordBot.log
import me.scarlet.discordbot.util.BotUtil._

object ReloadCommand extends Command {

    override def name = "reload"

    override def desc = "Reloads the configuration file of the bot."

    override def syntax = Array("**reload**")

    override def run(context: CommandContext): Unit = {
        try {
            DiscordBot.reloadConfig()
            context.msg.respond("Successfully reloaded configuration!").queue()
        } catch {
            case NonFatal(e) ⇒ {
                log.warn("Reload command failed to reload configuration")
                log.warn(stacktraceAsString(e))

                throw CommandException("Failed to load configuration, keeping old config.")
            }
        }
    }
}
