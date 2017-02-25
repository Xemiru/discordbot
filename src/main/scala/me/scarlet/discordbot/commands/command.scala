package me.scarlet.discordbot.commands

import me.scarlet.discordbot.util.BotUtil._
import me.scarlet.discordbot.util.Messager
import net.dv8tion.jda.core.entities.User

/**
  * An executable bot command.
  *
  * <p>Bot commands are always ran in their own thread.</p>
  */
trait Command {

    /**
      * Returns the name of this command.
      */
    def name: String

    /**
      * Returns the short description of this command, used in quick help messages.
      */
    def desc: String

    /**
      * Returns the extended description of this command, used in detailed help messages.
      *
      * <p>Defaults to an empty string.</p>
      */
    def extDesc: String = ""

    /**
      * Returns the possible valid expressions for usage of this command.
      *
      * <p>Defaults to an empty array.</p>
      */
    def syntax: Array[String] = Array()

    /**
      * Returns whether or not this command receives "raw arguments," that is, arguments that are tokens delimited by a space character <strong>without
      * considering quoted arguments.</strong>
      *
      * <pre>
      * // where % is the invoker and cmd is the command,
      * // resolves to "aaa", "bbb", "\"ccc", "ddd\"" as arguments
      * %cmd aaa bbb "ccc ddd"
      * </pre>
      *
      * <p>Defaults to false.</p>
      */
    def rawargs: Boolean = false

    /**
      * Returns whether or not this command is "preserved."
      *
      * <p>Preserved commands do not have their response embeds removed when a new command is invoked.</p>
      *
      * @return
      */
    def preserved: Boolean = false

    /**
      * Runs this command with the provided context.
      *
      * @param context the context of the command's execution
      */
    def run(context: CommandContext): Unit
}

/**
  * Extended base implementation of a [[Command]], allowing grouping of multiple commands under one.
  *
  * <p>In each ParentCommand, a `help` command is installed to display any registered subcommands and their help texts.</p>
  */
abstract class ParentCommand(private val _subs: Command*) extends Command {

    /**
      * Help command, automatically installed within every instance of [[ParentCommand]].
      *
      * <p>Includes pagination.</p>
      */
    private[ParentCommand] object HelpCommand extends Command {

        import me.scarlet.discordbot.util.Messager._

        override def name = "help"

        override def desc = "List registered commands."

        override def syntax = Array("**help** [page or command=1]")

        override def run(context: CommandContext): Unit = {
            val args = context.args
            val arg = if (args.isEmpty) "0" else args.head
            val target = subs.get(arg)

            if (target.isEmpty) {
                val cmd = new StringBuilder()
                val help = new StringBuilder()
                val page = paginate(subs.values.toList, try arg.toInt catch {
                    case _: NumberFormatException ⇒ 1
                })

                page._1.foreach(c ⇒ {
                    cmd ++= {if (c.preserved) s"**${c.name}**" else c.name} + "\n"
                    help ++= c.desc + "\n"
                })

                context.msg.embed(responseTemplate(s"Page ${page._2} of ${page._3}")
                    .addField("Command", cmd.mkString, true)
                    .addField("Help", help.mkString, true).build)
                    .complete()
            } else {
                val desc = if (target.get.extDesc.trim.nonEmpty) {
                    target.get.desc + "\n\n" + target.get.extDesc
                } else target.get.desc

                context.msg.embed(responseTemplate(desc)
                    .addField("Syntax", target.get.syntax.mkString("• ", "\n• ", ""), false).build)
                    .complete()
            }
        }
    }

    private val subs = Map[String, Command](_subs.toArray.+:(HelpCommand).map(cmd ⇒ (cmd.name, cmd)): _*)

    /**
      * Returns a named subcommand.
      *
      * @param name the name of a subcommand
      *
      * @return a subcommand?
      */
    def getCommand(name: String): Option[Command] = subs.get(name)

    /**
      * @inheritdoc
      *
      * <p>Implementing default behavior of a [[ParentCommand]] should usually be done by overriding its
      * [[ParentCommand#noArgs]] function.</p>
      *
      * @param context the context of the command's execution
      */
    override def run(context: CommandContext): Unit = {
        val args = context.args
        if (args.isEmpty) noArgs(context) else {
            val sub = getCommand(args.head.toLowerCase)
            if (sub.isDefined) sub.get.run(CommandContext(context.invoker, context.msg, args.drop(1): _*))
            else noSub(context)
        }
    }

    /**
      * Executes the behavior of this [[me.scarlet.discordbot.commands.ParentCommand]] when passed a subcommand that doesn't exist. The arguments stored by the
      * context contain the name of the non-existant subcommand as its first member.
      *
      * <p>Defaults to calling this ParentCommand's help command.</p>
      *
      * @param context the context of the command's execution
      */
    protected def noSub(context: CommandContext): Unit = HelpCommand.run(context)

    /**
      * Executes the behavior of this [[me.scarlet.discordbot.commands.ParentCommand]] when passed no parameters.
      *
      * <p>Defaults to calling this ParentCommand's help command.</p>
      *
      * @param context the context of the command's execution, assumed to have empty parameters
      */
    protected def noArgs(context: CommandContext): Unit = HelpCommand.run(context)

}

/**
  * Thrown when a command fails during its execution.
  *
  * <p>This can range from simple misusage to internal errors. Fatal errors should not be wrapped by this exception.</p>
  *
  * @param msg a short description of the failure
  * @param cause the exception causing the failure, if any
  */
case class CommandException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

/**
  * Context informing a command about its execution.
  *
  * <p>Contexts can also be used to send responses to the user in a standardized form.</p>
  *
  * @param invoker the [[net.dv8tion.jda.core.entities.User]] who invoked the command
  * @param msg the [[net.dv8tion.jda.core.entities.Message]] invoking the command
  */
case class CommandContext(invoker: User, msg: Messager, args: String*)
