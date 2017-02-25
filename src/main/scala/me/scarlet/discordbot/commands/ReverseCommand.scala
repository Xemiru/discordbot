package me.scarlet.discordbot.commands

object ReverseCommand extends Command {

    override def name = "reverse"

    override def desc = "Reverses text."

    override def extDesc = "Kinda cheats a little by using the RIGHT-TO-LEFT override character. Which means you'll also get (detide)."

    override def syntax = Array("**reverse** <text>")

    override def rawargs = true

    override def run(context: CommandContext): Unit = {
        val msg = context.msg
        msg.content = s"\u202e${context.args.mkString(" ")}"
        msg.update.foreach(_.queue())
    }

}
