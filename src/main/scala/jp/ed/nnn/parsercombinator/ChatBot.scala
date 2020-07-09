package jp.ed.nnn.parsercombinator

import scala.util.Random
import scala.util.matching.Regex

case class ChatBot(commands: List[Command])

sealed trait Command {
  def exec(input :String): Boolean
}

case class ReplyCommand(regex: Regex, replies: List[String]) extends Command {
  override def exec(input :String): Boolean = {
    regex.findFirstIn(input) match {
      case Some(_) =>
        println(Random.shuffle(replies).head)
        true
      case None => false
    }
  }
}