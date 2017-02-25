package me.scarlet.discordbot.commands

import me.scarlet.discordbot.DiscordBot.config
import me.scarlet.discordbot.util.Messager._

object IconCommand extends Command {

    override def name = "icon"

    override def desc = "Pastes the icon of the bot."

    override def syntax = Array("**icon**")

    override def preserved: Boolean = true

    override def run(context: CommandContext): Unit = {
        context.msg.embed(responseTemplate.setImage(config.icon).build).queue()
    }

}
