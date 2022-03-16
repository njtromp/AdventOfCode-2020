package nl.njtromp.adventofcode_2020

import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.Map
import scala.util.matching.Regex

object Day04 extends App {
  private val requiredFields: Set[String] = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  private val optionalFields: Set[String] = Set("cid")
  private var passportData: mutable.Map[String, String] = mutable.Map.empty
  private var passports: List[mutable.Map[String, String]] = List.empty
  private val height: Regex = "([0-9]{2,3})([a-z]{2})".r
  private val hairColor: Regex = "#([a-zA-Z0-9]{6})".r

  private def isValidPassport: Boolean = {
    requiredFields subsetOf (passportData.keySet diff optionalFields)
  }

  private def fieldChecks(p: Map[String, String]): Boolean = {
    p.forall(f => {
      f._1 match {
        case "byr" => f._2.toInt >= 1920 && f._2.toInt <= 2002
        case "iyr" => f._2.toInt >= 2010 && f._2.toInt <= 2020
        case "eyr" => f._2.toInt >= 2020 && f._2.toInt <= 2030
        case "hgt" => {
          f._2 match {
            case height(value, unit) => {
              if ("cm" equals unit)
                value.toInt >= 150 && value.toInt <= 193
              else if ("in"equals(unit))
                value.toInt >= 59 && value.toInt <= 76
              else
                false
            }
            case _ => false
          }
        }
        case "hcl" => f._2 match {
          case hairColor(_) => true
          case _ => false
        }
        case "ecl" => Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth") contains f._2
        case "pid" =>f._2.length == 9 && f._2.toInt > 0
        case "cid" => true
        case _ => false
      }
    })
  }

  for (line <- Source.fromInputStream(getClass.getResourceAsStream("/2020/input-puzzle04.txt")).getLines) {
    if (line.trim.isEmpty) {
      if (isValidPassport) {
        passports = passportData +: passports
      }
      passportData = Map.empty
    } else {
      for (data <- line.split(" ")) {
        val key: String = data.split(":")(0)
        val value: String = data.split(":")(1)
        passportData += (key -> value)
      }
    }
  }

  println(s"Answer part 1: ${passports.size}")
  println(s"Answer part 2: ${passports.filter(p => fieldChecks(p)).size}")
}
