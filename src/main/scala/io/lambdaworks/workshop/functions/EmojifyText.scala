package io.lambdaworks.workshop.functions

import com.lightbend.emoji.ShortCodes.Defaults._
import com.lightbend.emoji.ShortCodes.Implicits._

import scala.util.Try

object EmojifyText {

  def emojify(sentence: String): String = {
    val words = sentence.split(" +").toList
    val emojiWords = words.map(word => emojiOrWord(word.filter(chr => isLetter(chr))))
    emojiWords.map(word => word + " ").reduce(_ + _).trim
  }

  private def emojiOrWord(word: String): String =
    Try(word.toLowerCase.emoji.toString).getOrElse(word)

  private def isLetter(char: Char) = char.isLetter

}
