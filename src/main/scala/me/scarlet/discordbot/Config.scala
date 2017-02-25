package me.scarlet.discordbot

import java.awt.Color

import scala.collection.mutable.{ArrayBuffer, Seq ⇒ MSeq}

object Config {
    val default = Config()
}

case class Config(
    token: String                = "",
    selfbot: Boolean             = true,
    invoker: ArrayBuffer[String] = ArrayBuffer("$"),
    color: String                = "#FFFFFF",
    name: String                 = "Generic Selfbot",
    icon: String                 = "",
    aliases: Map[String, String] = Map(),
    apis: Map[String, String]    = Map(("imgur", ""))) {

        def verify() : Option[IllegalStateException] = {
            val errs = ArrayBuffer[String]()
            if(token.isEmpty) {
                errs += "token cannot be empty"
            }

            if(invoker.length < 1) {
                errs += "must have at least one command invoker"
            }

            invoker.foreach(s ⇒ invoker.foreach(o ⇒ {
                if(s != o && s.startsWith(o)) errs += s"invokers conflict ${Array(s, o).sortBy(a ⇒ a.head).mkString("(\"", "\", \"", "\")")}"
            }))

            if(invoker.exists(_.trim.isEmpty)) errs += "cannot have an empty string as an invoker"

            try {
                Color.decode(color)
            } catch {
                case _ : NumberFormatException ⇒ errs += s"invalid color $color"
            }

            if(errs.isEmpty) {
                None
            } else {
                Option(new IllegalStateException(s"Cannot continue with invalid configuration: ${errs.mkString("\n- ", "\n- ", "")}"))
            }
        }
    }
