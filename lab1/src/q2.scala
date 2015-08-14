
abstract class Element {
  def contents : Array[String] // no body : abstract
  val height = contents.length
  val width = if (height == 0) 0 else contents(0).length
}

object Element {
  def elem(contents: Array[String]): Element = new ArrayElement(contents)
  def elem(line: String): Element = new LineElement(line)
}

class ArrayElement(conts : Array[String]) extends Element {
  def contents : Array[String] = conts
}

class LineElement(s: String) extends ArrayElement(Array(s)) {
  override val width = s.length
  override val height = 1
}

//class UniformClass(c: Char, row: Int, col: Int) extends Element {
//  def contents: Array[String] = c
//}