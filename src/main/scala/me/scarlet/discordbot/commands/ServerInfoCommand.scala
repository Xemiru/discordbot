package me.scarlet.discordbot.commands

import java.time.format.DateTimeFormatter

import scala.collection.JavaConverters._

import me.scarlet.discordbot.DiscordBot
import me.scarlet.discordbot.util.Messager._
import net.dv8tion.jda.core.entities.Channel

object ServerInfoCommand extends Command {

    private val dateFormat = DateTimeFormatter.RFC_1123_DATE_TIME

    override def name = "serverinfo"

    override def desc = "View info about a server."

    override def syntax = Array("**serverinfo** [server id=current]")

    override def run(context: CommandContext): Unit = {
        val args = context.args
        val msg = context.msg
        val ch = msg.msg.getChannel
        val serverID = {
            if (args.nonEmpty) args.head
            else ch match {
                case ch: Channel ⇒ ch.getGuild.getId
                case _ ⇒ ""
            }
        }

        if (serverID.isEmpty) throw CommandException("this is a private channel idiot")
        else {
            val guild = Option(DiscordBot.jda.getGuildById(serverID))
            if (guild.isEmpty) throw CommandException("Couldn't grab guild info.")
            else {
                val owner = guild.get.getOwner.getUser
                val roleList = guild.get.getRoles.asScala.filter(!_.isManaged).map(_.getName).mkString(", ")
                val emoteList = guild.get.getEmotes.asScala.map(_.getName).mkString(", ")
                msg.embed(responseTemplate
                    .setTitle(s"Server info of ${guild.get.getName}", null)
                    .setThumbnail(guild.get.getIconUrl)
                    .addField("Owner", s"${owner.getName}#${owner.getDiscriminator}", true)
                    .addField("Home Channel", s"#${guild.get.getPublicChannel.getName}", true)
                    .addField("Region", s"${guild.get.getRegion.toString}", true)
                    .addField("User Count", s"${guild.get.getMembers.size}", true)
                    .addField("Default Notif. Level", s"${guild.get.getDefaultNotificationLevel.toString}", true)
                    .addField("Verification Level", s"${guild.get.getVerificationLevel.toString}", true)
                    .addField("User Roles List", s"```${roleList.substring(0, Math.min(roleList.length, 2042))}```", false)
                    .addField("Emotes List", s"```${if (emoteList.isEmpty) "no emotes" else emoteList.substring(0, Math.min(emoteList.length, 2042))}```",
                        false)
                    .addField("Date Created", s"${guild.get.getCreationTime.format(dateFormat)}", true)
                    .build).queue()
            }
        }
    }

}
