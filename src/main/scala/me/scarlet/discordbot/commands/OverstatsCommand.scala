package me.scarlet.discordbot.commands

import scala.collection.mutable.{TreeMap ⇒ MMap}
import scala.util.control.NonFatal

import com.mashape.unirest.http.async.Callback
import com.mashape.unirest.http.exceptions.UnirestException
import com.mashape.unirest.http.{HttpResponse, Unirest}
import com.mashape.unirest.request.HttpRequest
import io.circe.Json
import io.circe.parser._
import me.scarlet.discordbot.DiscordBot.log
import me.scarlet.discordbot.util.BotUtil._
import me.scarlet.discordbot.util.Messager._
import net.dv8tion.jda.core.EmbedBuilder

object OverstatsCommand extends Command {

    object LootboxAPI {
        val API = "https://api.lootbox.eu"

        val PATCH_NOTES = API + "/patch_notes"

        private val PARAMS = "/{platform}/{region}/{tag}"
        private val PARAMS_2 = PARAMS + "/{mode}"
        val PROFILE = API + PARAMS + "/profile"
        val PLAYTIMES = API + PARAMS_2 + "/heroes"
        val STATS_GENERIC = API + PARAMS_2 + "/allHeroes/"
        val STATS_HERO = API + PARAMS_2 + "/hero/{hero}/"

    }

    override def name = "overstats"

    override def desc = "Grab someone's Overwatch stats."

    override def extDesc =
        """This command caches data to not spam the API server. Stats taken for a user after the first try since the bot's launch may not be up-to-date.
          |Caching keeps data up to 16 players for 30 minutes. You'll know its grabbing new data if the command takes awhile to complete again.
          |
          |Battletag is the person's battletag. Case-sensitive, includes ID.
          | example: Xemiru#11803
          | fight me about my stats
          |
          |Category defines which part of their stats you get. Possible categories below.
          | - **profile** their user stats
          | - **playtimes** their playtime for each hero
          | - **stats** their general play statistics
          |
          |If you specify a hero instead, you'll get the play statistics for that specific hero.
          |Spaces in hero names, if any, are removed. Names are case-insensitive. Names with special characters are modified, see below.
          | - Torbjörn    = Torbjoern
          | - Lúcio       = Lucio
          | - Soldier: 76 = Soldier76
          | - D.Va        = DVa
          |
          |If a category or hero has more than one page, specify the page using a hash / number sign (#).
          | - third page of achievements: achieves#3
          | - second page of a Mercy stats list: mercy#2
          |
          |Syntax shown below. For optional parameters (marked by []) with multiple options, the first listed is the default.
        """.stripMargin


    override def syntax: Array[String] =
        Array("**overstats** <battletag> [category or hero=stats] [mode=quickplay|competitive] [region=us|eu|kr|cn|global] [platform=pc|psn|xbl]",
            "**overstats #patchnotes** [page=1]")

    private val cache = MMap[String, (Json, Long)]()
    private val cacheTime = 1000 * 60 * 30
    // 30 minutes
    private val maxSize = 16
    private val heroes = Array(
        "Genji", "Mccree", "Reaper", "Soldier76",
        "Sombra", "Tracer", "Bastion", "Hanzo",
        "Junkrat", "Mei", "Torbjoern", "Widowmaker",
        "DVa", "Reinhardt", "Roadhog", "Winston",
        "Zarya", "Ana", "Lucio", "Mercy",
        "Symmetra", "Zenyatta", "Pharah")
    private var running = false

    // process and cache responses
    private def processResponse(api: HttpRequest, success: Json ⇒ Unit, failure: String ⇒ Unit): Unit = {
        val response = cache.get(api.getUrl)

        def successFunc(json: Json): Unit = {
            try {
                success.apply(json)
            } catch {
                case NonFatal(e) ⇒ {
                    failure.apply("Response processing ran into errors.")
                    log.warn("Overstats response processing failed")
                    log.warn(stacktraceAsString(e))
                }
            }
        }

        if (response.isDefined) {
            if (System.currentTimeMillis - response.get._2 > cacheTime) cache.remove(api.getUrl)
            successFunc(response.get._1)
        } else {
            log.info(s"Overstats processing request ${api.getUrl}")
            api.asStringAsync(new Callback[String]() {
                override def completed(response: HttpResponse[String]): Unit = {
                    if (response.getStatus != 200) failure.apply("Server responded unsuccessfully: " + response.getStatusText)
                    else parse(response.getBody) match {
                        case Left(_) ⇒ failure.apply("Request failed: received bad JSON response")
                        case Right(json) ⇒ {
                            if (cache.size > maxSize) cache.remove(cache.firstKey)
                            cache.put(api.getUrl, (json, System.currentTimeMillis))
                            successFunc(json)
                        }
                    }
                }

                override def cancelled(): Unit = failure.apply("Request to server was cancelled")

                override def failed(e: UnirestException): Unit = {
                    log.warn("Request for Overwatch stats failed:\n" + stacktraceAsString(e))
                    failure.apply("Request failed: " + e.getMessage)
                }
            })
        }
    }

    override def run(context: CommandContext): Unit = {
        if (running) throw CommandException("This command is already running. Try again when it's done.")
        running = true

        val args = context.args
        val msg = context.msg

        try {
            val tag = {try args.head.split("#") catch {case _: ArrayIndexOutOfBoundsException ⇒ throw CommandException("Provide a battletag.")}}
            val coh = {try args(1) catch {case _: ArrayIndexOutOfBoundsException ⇒ "stats"}}.toLowerCase
            val mode = {try args(2) catch {case _: ArrayIndexOutOfBoundsException ⇒ "quickplay"}}.toLowerCase
            val region = {try args(3) catch {case _: ArrayIndexOutOfBoundsException ⇒ "us"}}.toLowerCase
            val platform = {try args(4) catch {case _: ArrayIndexOutOfBoundsException ⇒ "pc"}}.toLowerCase

            // get the correct capitalized hero name
            def heroName(s: String): Option[String] = {
                try {
                    Option(heroes.filter(n ⇒ s.equalsIgnoreCase(n))(0))
                } catch {case _: ArrayIndexOutOfBoundsException ⇒ None}
            }

            def fillParams(api: String) = {
                // automagically fill our urls with their route parameters
                val req = Unirest.get(api)
                    .header("Content-Type", "application/json")
                    .routeParam("platform", platform)
                    .routeParam("region", region)
                    .routeParam("tag", tag.mkString("-"))
                if (api.contains("{mode}")) req.routeParam("mode", mode) else req
            }

            // organize general/hero stats alphabetically
            def statOrg(stats: Json): List[(String, String)] = {
                stats.asObject.get.toMap.map(a ⇒ (a._1, a._2.asString.get)).toList.sortWith((a, b) ⇒ {
                    a._1.compare(b._1) < 0
                })
            }

            // build a paginated embed table from a list
            def table(title: String, key: String, value: String, list: List[(String, String)], _page: Int): EmbedBuilder = {
                val keys = new StringBuilder()
                val values = new StringBuilder()

                var page = _page - 1
                val pageSize = 8
                val maxPages = Math.ceil(list.size / pageSize.toDouble).toInt
                if (page < 0 || page > maxPages) page = 0

                list.slice(page * pageSize, (page + 1) * pageSize).foreach(j ⇒ {
                    keys ++= j._1 + "\n"
                    values ++= j._2 + "\n"
                })

                (keys.mkString.trim, values.mkString.trim)
                responseTemplate(s"Page ${page + 1} of $maxPages")
                    .setTitle(title, null)
                    .addField(key, keys.mkString.trim, true)
                    .addField(value, values.mkString.trim, true)
            }

            val responseFailure: String ⇒ Unit = fail ⇒ {
                msg.error(fail).queue()
                running = false
            }

            // param validation
            if (tag.length < 2) throw CommandException("Invalid battletag.")
            try tag(1).toInt catch {case _: NumberFormatException ⇒ throw CommandException("Invalid battletag.")}

            mode match {
                case "quickplay" | "competitive" ⇒ {}
                case unknown ⇒ throw CommandException(s"Unknown mode $unknown")
            }
            region match {
                case "us" | "eu" | "kr" | "cn" | "global" ⇒ {}
                case unknown ⇒ throw CommandException(s"Unknown region $unknown")
            }
            platform match {
                case "pc" | "psn" | "xbl" ⇒ {}
                case unknown ⇒ throw CommandException(s"Unknown platform $unknown")
            }

            val opt = coh.split("#")
            val fullTag = tag.mkString("#")
            val page = {try opt(1).toInt catch {case _@(_: ArrayIndexOutOfBoundsException | _: NumberFormatException) ⇒ 1}}

            // lootbox api takes awhile
            // make sure user knows we're doing _something_
            msg.respond("Asking server ...").queue()

            opt(0) match {
                case "stats" ⇒ {
                    processResponse(fillParams(LootboxAPI.STATS_GENERIC), response ⇒ {
                        msg.embed(table(s"Overall Play Statistics for $fullTag", "Statistic", "Value", statOrg(response), page).build).queue()
                        running = false
                    }, responseFailure)
                }
                case "profile" ⇒ {
                    processResponse(fillParams(LootboxAPI.PROFILE), response ⇒ {
                        // circe json why so many god damn gets
                        val data = response.asObject.get.apply("data").get.asObject.get
                        val ptimes = data.apply("playtime").get.asObject.get
                        val games = data.apply("games").get.asObject.get
                        val cgames = games.apply("competitive").get.asObject.get
                        val rlevel = data.apply("level").get.asNumber.get.toInt.get
                        val stars = Math.floor(rlevel / 100).toInt

                        val level = (rlevel % 100) + ("★" * stars)
                        // just like in-game :^)
                        val avatar = data.apply("avatar").get.asString.get
                        val rank = data.apply("competitive").get.asObject.get.apply("rank").get.asString.get.toInt
                        val qptime = ptimes.apply("quick").get.asString.get
                        val cptime = ptimes.apply("competitive").get.asString.get
                        val qwins = games.apply("quick").get.asObject.get.apply("wins").get.asString.get
                        val cwins = cgames.apply("wins").get.asString.get.toInt
                        val ctotal = cgames.apply("lost").get.asNumber.get.toInt.get + cwins

                        // get label
                        // can't do Top 500, api doesnt throw that at us
                        val rankLabel = rank match {
                            case i if 1 to 1499 contains i ⇒ "Bronze"
                            case i if 1500 to 1999 contains i ⇒ "Silver"
                            case i if 2000 to 2499 contains i ⇒ "Gold"
                            case i if 2500 to 2999 contains i ⇒ "Platinum"
                            case i if 3000 to 3499 contains i ⇒ "Diamond"
                            case i if 3500 to 3999 contains i ⇒ "Master"
                            case _ ⇒ "Grandmaster"
                        }

                        msg.embed(responseTemplate
                            .setTitle(s"Profile of $fullTag", null)
                            .setThumbnail(avatar)
                            .addField("Level", level, true)
                            .addField("Competitive Rank", s"$rank ($rankLabel)", true)
                            .addField("Quick Play Wins", qwins, true)
                            .addField("Competitive Win Rate", s"$cwins / $ctotal (${Math.round(cwins / ctotal.toDouble * 100).toInt}%)", true)
                            .addField("Quick Play Time", qptime, true)
                            .addField("Competitive Play Time", cptime, true).build).queue()
                        running = false
                    }, responseFailure)
                }
                case "playtimes" ⇒ {
                    processResponse(fillParams(LootboxAPI.PLAYTIMES), response ⇒ {
                        var main: String = null
                        val eb = table(s"Hero Playtimes for $fullTag", "Hero", "Playtime", response.asArray.get.sortWith((a, b) ⇒ {
                            // sort by playtime
                            try {
                                def convertTime(str: String) = {
                                    val arr = str.split(" ")
                                    val num = arr(0).toInt
                                    arr(1) match {
                                        case e if e.startsWith("hour") ⇒ num * 60 * 60
                                        case e if e.startsWith("minute") ⇒ num * 60
                                        case _ ⇒ num
                                    }
                                }

                                val pa = a.asObject.get.apply("playtime").get.asString.get
                                val pb = b.asObject.get.apply("playtime").get.asString.get

                                // make sure blank values end up last
                                if (pa == "--") false
                                else if (pb == "--") true
                                else convertTime(pa) > convertTime(pb)
                            } catch {case NonFatal(_) ⇒ true}
                        }).toList.map(b ⇒ {
                            val obj = b.asObject.get

                            // get the first hero and use their image as the thumbnail
                            if (main == null) main = obj.apply("image").get.asString.get
                            (obj.apply("name").get.asString.get, obj.apply("playtime").get.asString.get)
                        }), page)

                        msg.embed(eb.setThumbnail(main).build).queue()
                        running = false
                    }, responseFailure)
                }
                case h ⇒ {
                    // hero name
                    val hero = heroName(h)
                    if (hero.isEmpty) throw CommandException(s"Unknown hero $h")
                    processResponse(fillParams(LootboxAPI.STATS_HERO).routeParam("hero", hero.get), response ⇒ {
                        msg.embed(table(s"Play Statistics for $fullTag's ${hero.get}", "Statistic", "Value",
                            statOrg(response.asObject.get(hero.get).get), page).build).queue()
                        running = false
                    }, responseFailure)
                }
            }
        } catch {
            // i dont think nonfatal matters here
            // but shut up anyway intellij
            case NonFatal(e) ⇒ {
                // make sure we mark ourselves as no longer running
                running = false
                throw e
            }
        }
    }

}
