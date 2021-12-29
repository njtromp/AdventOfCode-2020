package nl.njtromp.adventofcode

import nl.njtromp.adventofcode.Matrix.{rotation, xRotation, yRotation, zRotation}

case class Matrix(elems: List[List[Double]]) {
  def *(m: Matrix): Matrix = {
    if (elems.head.size != m.elems.size)
      throw new IllegalArgumentException(s"Can't multiply a ${elems.size}x${elems.head.size} and a ${m.elems.size}x${m.elems.head.size} matrix")
    Matrix(elems.map(r => m.rowsToColums.elems.map(_.zip(r).map(n => n._1 * n._2).sum)))
  }
  def -(m: Matrix): Matrix = {
    if (elems.size != m.elems.size || elems.head.size != m.elems.head.size) {
      throw new IllegalArgumentException(s"Can't substract a ${m.elems.size}x${m.elems.head.size} from a ${elems.size}x${elems.head.size} matrix")
    }
    Matrix(elems.zip(m.elems).map(r => r._1.zip(r._2).map(n => n._1 - n._2)))
  }
  def +(m: Matrix): Matrix = {
    if (elems.size != m.elems.size || elems.head.size != m.elems.head.size) {
      throw new IllegalArgumentException(s"Can't add a ${elems.size}x${elems.head.size} to a ${m.elems.size}x${m.elems.head.size} matrix")
    }
    Matrix(elems.zip(m.elems).map(r => r._1.zip(r._2).map(n => n._1 + n._2)))
  }
  def rotateX(angle: Double): Matrix = xRotation(angle) * this
  def rotateY(angle: Double): Matrix = yRotation(angle) * this
  def rotateZ(angle: Double): Matrix = zRotation(angle) * this
  def rotate(angleX: Double, angleY: Double, angelZ: Double): Matrix = rotation(angleX, angleY, angelZ) * this
  def round: Matrix = Matrix(elems.map(_.map(Math.round(_).toDouble)))
  def clean: Matrix = Matrix(elems.map(_.map(n => if (n == -0.0) 0 else if (Math.abs(n) < 1.0E-10) 0 else n)))
  def toTuple: (Int, Int, Int) = (elems.head.head.toInt, elems(1).head.toInt, elems(2).head.toInt)
  def rowsToColums: Matrix = Matrix(elems.head.indices.map(i => elems.map(_(i))).toList)
}

object Matrix extends App {
  def xRotation(angle: Double) = {
    val a = Math.toRadians(angle)
    Matrix(List(
      List(1, 0, 0),
      List(0, Math.cos(a), -Math.sin(a)),
      List(0, Math.sin(a), Math.cos(a))
    ))
  }

  def yRotation(angle: Double) = {
    val a = Math.toRadians(angle)
    Matrix(List(
      List(Math.cos(a), 0, Math.sin(a)),
      List(0, 1, 0),
      List(-Math.sin(a), 0, Math.cos(a))
    ))
  }

  def zRotation(angle: Double) = {
    val a = Math.toRadians(angle)
    Matrix(List(
      List(Math.cos(a), -Math.sin(a), 0),
      List(Math.sin(a), Math.cos(a),0),
      List(0, 0, 1)
    ))
  }

  def rotation(angleX: Double, angleY: Double, angelZ: Double): Matrix = {
    val z = Math.toRadians(angleX)
    val b = Math.toRadians(angleY)
    val a = Math.toRadians(angelZ)
    Matrix(List(
      List(Math.cos(a) * Math.cos(b),
        Math.cos(a) * Math.sin(b) * Math.sin(z) - Math.sin(a) * Math.cos(z),
        Math.cos(a) * Math.sin(b) * Math.cos(z) + Math.sin(a) * Math.sin(z)),
      List(Math.sin(a) * Math.cos(b),
        Math.sin(a) * Math.sin(b) * Math.sin(z) + Math.cos(a) * Math.cos(z),
        Math.sin(a) * Math.sin(b) * Math.cos(z) - Math.cos(a) * Math.sin(z)),
      List(-Math.sin(b),
        Math.cos(b) * Math.sin(z),
        Math.cos(b) * Math.cos(z))
    ))
  }

  def toIntLists(m: Matrix): List[List[Int]] = m.elems.map(_.map(_.toInt))

  def permutations: List[Matrix] = (0 to 3).flatMap(x => (0 to 3)
    .flatMap(y => (0 to 3)
      .map(z => rotation(x * 90, y * 90, z * 90))))
    .toList.distinct

  def perpendicularPermutations: List[Matrix] = permutations.map(_.clean.round).distinct

}
