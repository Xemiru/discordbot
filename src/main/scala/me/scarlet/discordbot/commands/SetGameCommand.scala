package me.scarlet.discordbot.commands

import me.scarlet.discordbot.DiscordBot
import me.scarlet.discordbot.DiscordBot.log
import net.dv8tion.jda.core.entities._

object SetGameCommand extends Command {

    override def name = "setgame"

    override def desc = "Sets your currently-playing game."

    override def syntax = Array("**setgame** [game=clear]")

    override def rawargs = true

    override def run(context: CommandContext): Unit = {
        val game = context.args.mkString(" ")
        val pres = DiscordBot.jda.getPresence
        var desc = ""

        if (game.isEmpty) {
            pres.setGame(null)
            desc = "set to not play anything"
        } else {
            pres.setGame(Game.of(game))
            desc = s"set to 'Playing **$game**'"
        }

        log.info(s"Game status $desc")
        context.msg.respond(desc).queue()
    }
}
