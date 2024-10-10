package nl.njtromp.codingcontest

import org.scalatest.flatspec.AnyFlatSpec

class BakeryTest extends AnyFlatSpec {
  "John" should "be naughty on day 2 at level 1" in {
    assert(new Bakery().naughtyJohn("F 1 200 F 2 170 B 1 200 B 2 100") === 2 :: List.empty)
  }

  "John" should "be naughty on day 2 at level 3" in {
    assert(new Bakery().naughtyJohn("F 1 200 F 2 170 B 1 100 B 2 80 B 2 15 B 2 100 B 3 70") === 2 :: List.empty)
  }

}
