package nl.njtromp.adventofcode

trait Optimization {

  def maximize[A](time: Long, maxTime: Long, current: A, neighbors: A => List[A], costs: (A, A) => Long, gain: (A, Long) => Long): Long =
    var maximum = 0L
    def maximize(time: Long, current: A, profit: Long, visited: Set[A]): Unit = {
      val activatables = neighbors(current).filter(!visited.contains(_))
      if (activatables.isEmpty)
        maximum = Math.max(maximum, profit)
      else if (maximum < profit + activatables.map(gain(_, time)).sum)
        activatables.foreach(n => {
          val openedTime = time + costs(current, n) + 1
          if (openedTime >= maxTime)
            profit
          else {
            val newProfit = profit + gain(n, openedTime)
            maximum = Math.max(maximum, newProfit)
            maximize(openedTime, n, newProfit, visited + n)
          }
        })
    }
    maximize(time, current, 0L, Set.empty[A])
    maximum

}
