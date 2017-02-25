package me.scarlet.discordbot

import java.io.{File, PrintWriter}

import scala.io.Source

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import me.scarlet.discordbot.commands.CommandHandler
import net.dv8tion.jda.core.events.ReadyEvent
import net.dv8tion.jda.core.hooks.ListenerAdapter
import net.dv8tion.jda.core.utils.SimpleLog
import net.dv8tion.jda.core.{AccountType, JDA, JDABuilder}

/**
  * The main entrypoint class for the Discord bot.
  */
object DiscordBot extends ListenerAdapter {

    private var _config: Config = Config.default
    private var _eventThread: Thread = _
    private var _jda: JDA = _

    val configFile = new File("config.json")
    val log: SimpleLog = SimpleLog.getLog("Selfbot")

    def eventThread: Thread = _eventThread

    def config: Config = _config

    def jda: JDA = _jda

    //
    // configuration
    //

    /**
      * Reloads the configuration from disk, or loads a default one if none is found.
      */
    def reloadConfig(): Unit = {
        if (configFile.exists) {
            log.info("Loading config file.")

            // read config and load it as a case class
            decode[Config ⇒ Config](Source.fromFile(configFile).getLines.mkString) match {
                case Right(cfg) ⇒ {
                    val config = cfg(_config)
                    config.verify().foreach(throw _)
                    _config = config
                }
                case Left(e) ⇒ throw e
            }

            log.info("Rewriting changes to config file.")
            writeConfig()
        } else {
            // write the default configuration
            log.info("Writing default config file. You'll need to set the \"token\" value to your token, then re-run the bot.")
            writeConfig()
            System.exit(0)
        }
    }

    /**
      * Writes the current configuration to disk.
      */
    def writeConfig(): Unit = {
        val write = new PrintWriter(configFile)
        write.append(_config.asJson.toString)
        write.close()
    }

    //
    // main bot
    //

    def main(args: Array[String]): Unit = {
        reloadConfig()
        val typee = if (config.selfbot) AccountType.CLIENT else AccountType.BOT
        log.info(s"Launching bot as a ${if (typee == AccountType.CLIENT) "selfbot" else "server bot"}")
        new JDABuilder(typee).setToken(config.token).addListener(this).buildBlocking()
    }

    override def onReady(event: ReadyEvent): Unit = {
        _jda = event.getJDA
        _eventThread = Thread.currentThread()

        // prepare any managers
        TagHandler.loadTags()

        // register listeners
        jda.addEventListener(TagHandler, CommandHandler)
        log.info(s"${config.name} is ready!")
    }

}
