package me.scarlet.discordbot.commands

object SpaceCommand extends Command {

    override def name = "space"

    override def desc = "Space out characters in your message."

    override def syntax = Array("**space** <text>")

    override def rawargs = true

    override def run(context: CommandContext): Unit = {
        val str = context.args.mkString(" ").split("").mkString(" ")
        val msg = context.msg
        if (str.length > 2000) {
            msg.content = str.substring(0, 2000)
            msg.respond("Message was capped at 2,000 characters.").queue()
        } else {
            msg.content = str
            msg.update.foreach(_.queue())
        }
    }

}
