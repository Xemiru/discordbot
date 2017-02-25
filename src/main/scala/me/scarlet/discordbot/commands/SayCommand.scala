package me.scarlet.discordbot.commands

object SayCommand extends Command {

    override def name = "say"

    override def desc = "Ping the bot with something to say."

    override def syntax = Array("**icon** <text>")

    override def rawargs: Boolean = true

    override def preserved: Boolean = true

    override def run(context: CommandContext): Unit = {
        val msg = context.msg
        val start = System.currentTimeMillis
        val pong = context.args.mkString(" ")
        msg.respond(pong).complete()

        val time = System.currentTimeMillis - start
        msg.respond(s"$pong (${time}ms)").queue()
    }
}
