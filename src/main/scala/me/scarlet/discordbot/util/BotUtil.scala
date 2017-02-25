package me.scarlet.discordbot.util

import java.io.{PrintWriter, StringWriter}

/**
  * Utility object holding utility things.
  */
object BotUtil {

    /**
      * Paginates the given list and returns the contents of the given page index.
      *
      * <p>If an invalid page index is given (the index was out of range), the page index defaults to 1.</p>
      *
      * <p>The list is returned with the page index used and the maximum possible pages, in the case such information needs to be displayed.</p>
      *
      * @param list the list to paginate
      * @param _page the page to get the contents of, 1-based
      * @param pageSize the maximum count of elements in a page, defaults to 8
      *
      * @return a tuple containing the list page, the page index used, and the maximum possible pages
      */
    def paginate[A](list: List[A], _page: Int, pageSize: Int = 8): (List[A], Int, Int) = {
        val maxPage = Math.ceil(list.size / pageSize.toDouble).toInt
        val page = if (_page < 1 || _page > maxPage) 0 else _page - 1
        (list.slice(page * pageSize, (page + 1) * pageSize), page + 1, maxPage)
    }

    /**
      * Returns the stacktrace of the given Throwable as a string.
      *
      * @param e the Throwable
      *
      * @return its stacktrace
      */
    def stacktraceAsString(e: Throwable): String = {
        val writer = new StringWriter()
        e.printStackTrace(new PrintWriter(writer))
        writer.toString.trim
    }

}
