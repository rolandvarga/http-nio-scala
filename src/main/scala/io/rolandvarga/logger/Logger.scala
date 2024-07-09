package io.rolandvarga.logger

import com.github.nscala_time.time.Imports._


sealed abstract class logLevel(val level: Int, val name: String, val color: String)

object logLevel {
  case object DEBUG extends logLevel(1, "DEBUG", Console.CYAN)
  case object INFO extends logLevel(2, "INFO", Console.GREEN)
  case object WARN extends logLevel(3, "WARN", Console.YELLOW)
  case object ERROR extends logLevel(4, "ERROR", Console.RED)
  case object FATAL extends logLevel(5, "FATAL", Console.RED)

  case class UnknownLogLevel(override val level: Int, override val name: String = "Unknown LogLevel", override val color: String = Console.RED) extends logLevel(level, name, color)

  val values = Set(
    DEBUG,
    INFO,
    WARN,
    ERROR,
    FATAL,
  )

  private val codeMap = values.map(attr => attr.level -> attr).toMap

  def lookup(code: Int): Option[logLevel] = codeMap.get(code)
}

object Logger {
  val dateFormat = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
  def prefix(level: String, color: String): String = {
    s"[${dateFormat.print(DateTime.now)}] - [${color}${level}${Console.WHITE}]"
  }

  def log(level: logLevel, message: String) = {
    val p = prefix(level.name, level.color)
    println(s"${p}: $message")
  }

  def info(message: String) = log(logLevel.INFO, message)
  def debug(message: String) = log(logLevel.DEBUG, message)
  def warn(message: String) = log(logLevel.WARN, message)
  def error(message: String) = log(logLevel.ERROR, message)
  def fatal(message: String) = log(logLevel.FATAL, message)
}
