package nl.njtromp.adventofcode

import org.apache.commons.math3.linear._
import org.apache.commons.math3.geometry.euclidean.threed._

object LineIntersectionExample {

  def main(args: Array[String]): Unit = {
    // Example lines represented in vector form: r(t) = v + t * d
    val line1 = Line(new Vector3D(1.0, 2.0, 3.0), new Vector3D(2.0, 1.0, -1.0))
    val line2 = Line(new Vector3D(0.0, 1.0, 2.0), new Vector3D(1.0, -1.0, 3.0))
    val line3 = Line(new Vector3D(-1.0, 0.0, 1.0), new Vector3D(3.0, 2.0, -1.0))

    // Create a list of lines
    val lines = List(line1, line2, line3)

    // Solve the system of equations
    val solution = solveSystemOfEquations(lines)

    // Print the results
    if (solution.isDefined) {
      println("Common Line:")
      println(s"Position: ${solution.get._1}")
      println(s"Direction: ${solution.get._2}")
    } else {
      println("No common line found.")
    }
  }

  // Case class representing a line in vector form: r(t) = v + t * d
  case class Line(v: Vector3D, d: Vector3D)

  // Solve the system of equations for each pair of lines using Apache Commons Math
  def solveSystemOfEquations(lines: List[Line]): Option[(Vector3D, Vector3D)] = {
    // Create a matrix to store coefficients
    val coefficientsMatrix = new Array2DRowRealMatrix(lines.length * (lines.length - 1), lines.length)

    // Create a vector to store constants
    val constantsVector = new ArrayRealVector(lines.length * (lines.length - 1))

    // Populate the coefficients matrix and constants vector
    var row = 0
    for {
      i <- lines.indices
      j <- 0 until i
    } {
//      coefficientsMatrix.setRowVector(row, lines(i).d.toArray)
//      coefficientsMatrix.setEntry(row, i, -lines(i).d.getEntry(j))
//      coefficientsMatrix.setEntry(row, j, lines(i).d.getEntry(j))
//      constantsVector.setEntry(row, lines(j).v.getEntry(j) - lines(i).v.getEntry(j))
      row += 1
    }

    // Solve the system of equations
    val solver = new SingularValueDecomposition(coefficientsMatrix).getSolver
    val solution = solver.solve(constantsVector)

    // Check if the solution is valid
    if (!solution.isNaN) {
      // Extract position and direction vectors from the solution
      val positionVector = new Vector3D(solution.getEntry(0), solution.getEntry(1), solution.getEntry(2))
      val directionVector = new Vector3D(solution.getEntry(3), solution.getEntry(4), solution.getEntry(5))

      Some((positionVector, directionVector))
    } else {
      None
    }
  }
}
