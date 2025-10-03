package nl.njtromp.adventofcode

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector, LUDecomposition}

trait LinearAlgebra {

  def solve(coefficients: Array[Array[Double]], vector: Array[Double]): Array[Double] =
    val c = new Array2DRowRealMatrix(coefficients)
    val v = new ArrayRealVector(vector, false)
    new LUDecomposition(c).getSolver.solve(v).toArray

  def solve(coefficients: Array[Array[Long]], vector: Array[Long]): Option[Array[Long]] =
    def isValidSolution(solution: Array[Long]): Boolean =
      coefficients.map(_.zip(solution).map(_ * _).sum) sameElements vector
    val s = solve(coefficients.map(_.map(_.toDouble)), vector.map(_.toDouble))
    val solution = s.map(Math.round)
    Some(solution).filter(isValidSolution)

}
