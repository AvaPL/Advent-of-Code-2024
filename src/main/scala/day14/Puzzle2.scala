package io.github.avapl
package day14

import day14.Puzzle2Terminal.Key

import org.jline.keymap.{BindingReader, KeyMap}
import org.jline.terminal.{Attributes, Terminal, TerminalBuilder}
import org.jline.utils.InfoCmp.Capability

import java.io.Closeable
import scala.util.Using

/** This is not a real solution but rather an interactive simulation of the robots' movement. While it doesn't give the
  * answer directly, it was a fun exercise to implement :)
  *
  * To run it, use:
  * {{{
  *   sbt -error "runMain io.github.avapl.day14.puzzle2"
  * }}}
  * It may not work in the IDE terminal or some terminals of different operating systems (I tested it only in the native
  * macOS terminal). It might be also required to decrease the font size to see the entire space (Command-Minus in the
  * macOS terminal).
  *
  * The solution for my input is 8087.
  */
@main def puzzle2(): Unit = {
  val robots = PuzzleInputParser.parsedInput
  val maxSeconds = 10_000

  Using(Puzzle2Terminal(maxSeconds)) { terminal =>
    var quit = false
    var currentSecond = 0

    while (!quit) {
      val robotPositions = robots.map(calculatePositionAfterSeconds(_, currentSecond))
      terminal.printScreen(robotPositions, currentSecond)

      terminal.readKey() match {
        case Key.ArrowRight =>
          currentSecond = (currentSecond + 1).clamp(0, maxSeconds)
        case Key.ArrowUp =>
          currentSecond = (currentSecond + 100).clamp(0, maxSeconds)
        case Key.ArrowLeft =>
          currentSecond = (currentSecond - 1).clamp(0, maxSeconds)
        case Key.ArrowDown =>
          currentSecond = (currentSecond - 100).clamp(0, maxSeconds)
        case Key.Quit =>
          quit = true
      }
    }
  }
}

class Puzzle2Terminal(maxSeconds: Int) extends Closeable {

  private val (terminal, previousTerminalAttributes) = createTerminal()

  private def createTerminal(): (Terminal, Attributes) = {
    val terminal = TerminalBuilder.terminal()
    val normalModeAttributes = terminal.enterRawMode()
    terminal.puts(Capability.keypad_xmit)
    (terminal, normalModeAttributes)
  }

  private val keyMap: KeyMap[Key] = {
    val keyMap = new KeyMap[Key]()
    keyMap.bind(Key.ArrowUp, KeyMap.key(terminal, Capability.key_up))
    keyMap.bind(Key.ArrowRight, KeyMap.key(terminal, Capability.key_right))
    keyMap.bind(Key.ArrowDown, KeyMap.key(terminal, Capability.key_down))
    keyMap.bind(Key.ArrowLeft, KeyMap.key(terminal, Capability.key_left))
    keyMap.bind(Key.Quit, 'q'.toString)
    keyMap
  }

  private val bindingReader = new BindingReader(terminal.reader())

  def readKey(): Key =
    bindingReader.readBinding(keyMap)

  def printScreen(robotPositions: List[Position], currentSecond: Int): Unit = {
    clearScreen()
    printRobotsPositions(robotPositions)
    printProgress(currentSecond)
    printControls()
  }

  private def clearScreen(): Unit =
    terminal.puts(Capability.clear_screen)

  private def printRobotsPositions(positions: List[Position]): Unit = {
    val space = Array.fill(spaceHeight, spaceWidth)(' ')
    positions.foreach { position =>
      space(position.row)(position.column) = '#'
    }
    space.map(_.mkString).foreach(terminal.writer().println)
    terminal.writer().println()
    terminal.flush()
  }

  private def printProgress(currentSecond: Int): Unit = {
    val progress = currentSecond.toDouble / maxSeconds
    val progressBarLength = spaceWidth - 24
    val progressBar = "#" * (progress * progressBarLength).toInt
    val spaces = " " * (progressBarLength - progressBar.length)
    terminal.writer.println(f"[$progressBar$spaces] second $currentSecond%5d of $maxSeconds%5d")
    terminal.flush()
  }

  private def printControls(): Unit = {
    terminal.writer.println(
      "[→] advance by 1s   [↑] advance by 100s   [←] rewind by 1s   [↓] rewind by 100s              [q] quit"
    )
    terminal.flush()
  }

  override def close(): Unit = {
    terminal.puts(Capability.keypad_local)
    terminal.setAttributes(previousTerminalAttributes)
    terminal.close()
  }
}

object Puzzle2Terminal {
  enum Key {
    case ArrowUp, ArrowRight, ArrowDown, ArrowLeft, Quit
  }
}

extension (i: Int) {
  def clamp(min: Int, max: Int): Int =
    i.max(min).min(max)
}
