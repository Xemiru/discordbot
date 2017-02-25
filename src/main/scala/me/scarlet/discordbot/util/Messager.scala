package me.scarlet.discordbot.util

import java.awt.Color

import me.scarlet.discordbot.DiscordBot.config
import net.dv8tion.jda.core.entities.{Message, MessageEmbed}
import net.dv8tion.jda.core.requests.RestAction
import net.dv8tion.jda.core.{EmbedBuilder, MessageBuilder}

object Messager {

    // we aren't a case class but bye new keyword
    def apply(msg: Message): Messager = new Messager(msg)

    /**
      * The color used to denote embeds describing errors.
      */
    val ERROR_COLOR: Color = Color.decode("#FF6666")

    /**
      * Generates a template for informing the user.
      */
    def responseTemplate: EmbedBuilder = responseTemplate()

    /**
      * Generates a template for informing the user with a message.
      *
      * @param msg the message
      */
    def responseTemplate(msg: String = null): EmbedBuilder =
        new EmbedBuilder().setFooter(config.name, config.icon).setColor(Color.decode(config.color)).setDescription(msg)

    /**
      * Generates a template for informing the user of an error.
      */
    def errorTemplate: EmbedBuilder = errorTemplate()

    /**
      * Generates a template for informing the user of an error with a message.
      *
      * @param msg the message
      */
    def errorTemplate(msg: String = null): EmbedBuilder =
        new EmbedBuilder().setFooter("There might be a full error in the console.", config.icon).setColor(ERROR_COLOR).setDescription(msg)
}

/**
  * Wrapper around a [[net.dv8tion.jda.core.entities.Message]] that provides convenience editing functions.
  *
  * @param msg the Message
  */
class Messager(val msg: Message) {

    import Messager._

    /**
      * The content of the message in its current state.
      */
    private var ccontent: String = msg.getRawContent

    /**
      * The content of the message.
      *
      * <p>This variable reflects the current content of the message. The edit is reflected in the message when an action modifying the message takes place
      * (editing with an embed), or if [[me.scarlet.discordbot.util.Messager#update]] is called.</p>
      */
    var content: String = msg.getRawContent

    /**
      * Edit the message with an embed.
      *
      * @param message the message to respond with
      *
      * @return a RestAction to edit the invoking message with
      */
    def respond(message: String): RestAction[Message] = update(responseTemplate(message).build).get

    /**
      * Edit the message with an embed indicating errors.
      *
      * @param message the message to respond with
      *
      * @return a RestAction to edit the invoking message with
      */
    def error(message: String): RestAction[Message] = update(errorTemplate(message).build).get

    /**
      * Edit the message's embed.
      *
      * <p>It is recommended to use templates in [[me.scarlet.discordbot.util.Messager]] as a base.</p>
      *
      * @param embed the embed to respond with
      *
      * @return a RestAction to edit the invoking message with
      */
    def embed(embed: MessageEmbed): RestAction[Message] = update(embed).get

    /**
      * Clears the current embed on the invoking message.
      *
      * @return a RestAction to edit the invoking message with
      */
    def clear(): RestAction[Message] = update(new EmbedBuilder().setDescription("").build).get

    /**
      * Attempts to update the invoking message.
      *
      * <p>This will not return None if the message content was previously changed using [[me.scarlet.discordbot.util.Messager#content]].</p>
      *
      * @return a RestAction to edit the invoking message with if it can be updated, otherwise None
      */
    def update: Option[RestAction[Message]] = update()

    /**
      * Attempts to update the invoking message.
      *
      * <p>This will not return None if an embed is passed or the message content was changed using
      * [[me.scarlet.discordbot.util.Messager#content]].</p>
      *
      * @param embed an embed to update the message with
      *
      * @return a RestAction to edit the invoking message with if it can be updated, otherwise None
      */
    private def update(embed: MessageEmbed = null): Option[RestAction[Message]] = {
        if (embed != null || ccontent.trim != content.trim) {
            val mb = new MessageBuilder()
            if (embed != null) mb.setEmbed(embed)
            if (ccontent.trim != content.trim) {
                ccontent = content.trim
            }

            mb.append(content)
            Option(msg.editMessage(mb.build))
        } else {
            None
        }
    }

}
