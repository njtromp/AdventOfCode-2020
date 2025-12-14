package nl.njtromp.adventofcode

import scala.util.boundary, boundary.break
import scala.collection.mutable

class Day12 extends Puzzle[Long] {
  private type Region = Array[Array[Int]]
  private type Pos = (Int, Int) // (Y, X)
  private val SHAP_SIZE = 3

  extension (region: Region)
    private def fits(p: Package, offset: Pos): Boolean =
      (offset._1 + p.shape.length <= region.length) &&
        (offset._2 + p.shape.length <= region.head.length) &&
        p.shape.indices.forall(y => p.shape(y).indices.forall(x => region(y + offset._1)(x + offset._2) + p.shape(y)(x) <= 1))
    private def place(p: Package, offset: Pos): Unit = {
      p.shape.indices.foreach(y =>
        p.shape(y).indices.foreach(x => region(y + offset._1)(x + offset._2) += p.shape(y)(x))
      )
    }
    private def remove(p: Package, offset: Pos): Unit =
      p.shape.indices.foreach(y =>
        p.shape(y).indices.foreach(x => region(y + offset._1)(x + offset._2) -= p.shape(y)(x))
      )

  case class Package(index: Int, shape: Region) {
    def squares: Int = shape.map(_.count(_ == 1)).sum
    lazy val variants: List[Package] =
      rotations.flatMap(_.flips).distinct
    private def rotations: List[Package] =
      val v1 = rotateCW
      val v2 = v1.rotateCW
      val v3 = v2.rotateCW
      Set(this, v1, v2, v3).toList
    private def flips: List[Package] =
      val vf = flipVertical
      val hf = flipHorizontal
      val hvf = hf.flipVertical
      val vhf = vf.flipHorizontal
      Set(this, vf, hf, vhf, hvf).toList
    private def flipVertical: Package = Package(index, shape.map(_.reverse))
    private def flipHorizontal: Package = Package(index, Array(shape.last, shape(1), shape.head))
    private def rotateCW: Package = Package(index, Array(
      Array(shape.last.head, shape(1).head, shape.head.head),
      Array(shape.last(1), shape(1)(1), shape.head(1)),
      Array(shape.last.last, shape(1).last, shape.head.last)
    ))
    // Dirty trick to use the toString method for hashCode and equals
    // I tell my student to NEVER do this, but here it is a nice shortcut
    override def hashCode(): Int = 13 * index + toString.hashCode()
    override def equals(obj: Any): Boolean =
      obj match {
        case Package(i, _) => index == i && toString.equals(obj.toString)
        case _ => false
      }
    override def toString: String = shape.map(_.map(d => if d == 1 then '#' else '.').mkString).mkString("\n")
  }
  private object Package {
    def parse(lines: List[String]): Package =
      Package(lines.head.split(':').head.toInt, lines.tail.map(_.map(p => if p == '#' then 1 else 0).toArray).toArray)
  }

  private case class Tree(width: Int, height: Int, packages: Array[Int]) {
    def size: Int = width * height

    override def toString: String = s"Tree(${width}x$height: ${packages.mkString(" ")})"
  }
  private object Tree {
    def parse(line: String): Tree =
      val parts = line.split(':')
      parts.head match { case s"${w}x$l" => Tree(w.toInt, l.toInt, parts.last.trim.split(' ').map(_.toInt))}
  }

  private def fitPackages(tree: Tree, packages: Array[Package]): Boolean =
    val region = Array.ofDim[Int](tree.height, tree.width)
    def placeAll(packages: List[Package], limits: (Int, Int)): Boolean =
      if packages.isEmpty then
        true
      else
        boundary:
          (0 to limits._1).foreach(y =>
            (0 to limits._2).foreach(x =>
              packages.head.variants.foreach(p =>
                val pos = (y, x)
                if region.fits(p, pos) then
                  region.place(p, pos)
                  if placeAll(packages.tail, (tree.height - SHAP_SIZE + 1, tree.width - SHAP_SIZE + 1)) then
                    break(true)
                  region.remove(p, pos)
              )
            )
          )
          false
    placeAll(tree.packages.indices.flatMap(i => (1 to tree.packages(i)).map(_ => packages(i))).toList, ((tree.height / 2) + 1, (tree.width / 2) + 1))

  override def exampleAnswerPart1: Long = 2
  override def solvePart1(lines: List[String]): Long =
    val packages = lines.splitOnEmptyLines.reverse.tail.reverse.map(Package.parse).toArray
    val trees = lines.splitOnEmptyLines.last.map(Tree.parse)
    val candidates = trees.filter(r => r.packages.zip(packages).map((n, p) => n * p.squares).sum <= r.width * r.height)
    if candidates.size > 3 then
      println(s"Just filtering out the trees that have to many packages already results in the answer: ${candidates.size}")
    candidates.count(fitPackages(_, packages))

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day12 extends App {
  new Day12().solvePuzzles()
}
