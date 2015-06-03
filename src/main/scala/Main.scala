object LogParser {

  sealed trait MessageType
  case object Info extends MessageType
  case object Warning extends MessageType
  case class Error(code: Int) extends MessageType

  type Timestamp = Int

  sealed trait LogMessage
  case class LogMessageM(`type`: MessageType, timestamp: Timestamp, message: String) extends LogMessage
  case class Unknown(string: String) extends LogMessage

  // Binary Search Tree
  // http://en.wikipedia.org/wiki/Binary_search_tree
  sealed trait MessageTree
  case object Leaf extends MessageTree
  case class Node(left: MessageTree, log: LogMessage, right: MessageTree) extends MessageTree

  def getTimestamp(l: LogMessage): Option[Int] = {
    l match {
      case LogMessageM(_, t, _) => Some(t)
      case Unknown(_) => None
    }
  }

  def getString(l: LogMessage): String = {
    l match {
      case LogMessageM(_, _, s) => s
      case Unknown(s) => s
    }
  }

  def parseMessage(s: String): LogMessage = {
    s.split(" ") match {
      case Array("I", x, xs @ _*) => LogMessageM(Info, x.toInt, xs.mkString(" "))
      case Array("W", x, xs @ _*) => LogMessageM(Warning, x.toInt, xs.mkString(" "))
      case Array("E", x, y, xs @ _*) => LogMessageM(Error(x.toInt), y.toInt, xs.mkString(" "))
      case xs => Unknown(xs.mkString(" "))
    }
  }

  def parse(s: String): List[LogMessage] = {
    s.split("\n").map(parseMessage).toList
  }

  def insert(l: LogMessage, mt: MessageTree): MessageTree = {
    (l, mt) match {
      case (Unknown(_), tree) => tree
      case (msgToInsert, Leaf) => Node(Leaf, msgToInsert, Leaf)
      case (msgToInsert, Node(left, rootMsg, right)) =>
        if (getTimestamp(msgToInsert).get <= getTimestamp(rootMsg).get)
          Node(insert(msgToInsert, left), rootMsg, right)
        else
          Node(left, rootMsg, insert(msgToInsert, right))
    }
  }

  def build(ls: List[LogMessage]): MessageTree = {
    ls.foldRight(Leaf: MessageTree)(insert)
  }

  def inOrder(mt: MessageTree): List[LogMessage] = {
    mt match {
      case Leaf => List()
      case Node(left, logMsg, right) => inOrder(left) ++ List(logMsg) ++ inOrder(right)
    }
  }

  def isBigError(l: LogMessage): Boolean = {
    // big errors are errors with code > 50
    l match {
      case LogMessageM(Error(c), _, _) => if (c > 50) true else false
      case _ => false
    }
  }

  def whatWentWrong(ls: List[LogMessage]): List[String] = {
    inOrder(build(ls)).filter(isBigError).map(getString)
  }

}

object Main {

  import scala.io.Source

  import LogParser._

  def readFile(): String = {
    Source.fromURL(getClass.getResource("/error.log")).mkString
  }

  def main(args: Array[String]) {
    val whatWentWrongList = whatWentWrong(parse(readFile()))
    println(whatWentWrongList.mkString("\n"))
  }

}
