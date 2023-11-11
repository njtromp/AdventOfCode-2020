package nl.njtromp.adventofcode

trait Optimization {

  def maximize[A](maxTime: Long, worker: A, neighbors: A => List[A], costs: (A, A) => Long, gain: (A, Long) => Long): Long =
    var maximum = 0L
    def maximize(time: Long, worker: A, profit: Long, visited: Set[A]): Unit =
      val activatables = neighbors(worker).filter(!visited.contains(_))
      if (activatables.nonEmpty)
        if (maximum < profit + activatables.map(gain(_, time)).sum)
        activatables.foreach(n =>
          val actionTime = time + costs(worker, n) + 1
          if (actionTime >= maxTime)
            profit
          else
            val newProfit = profit + gain(n, actionTime)
            maximum = Math.max(maximum, newProfit)
            maximize(actionTime, n, newProfit, visited + n)
        )
    maximize(0L, worker, 0L, Set.empty[A])
    maximum

  def maximize[A](maxTime: Long, workers: List[A], neighbors: A => List[A], costs: (A, A) => Long, gain: (A, Long) => Long): Long =
    var maximum = 0L
    def maximize(time: Long, workers: List[(Long, A)], profit: Long, visited: Set[A]): Unit =
      val worker = workers.head._2
      val activatables = neighbors(worker).filter(!visited.contains(_))
      if (activatables.nonEmpty)
        if (maximum < profit + activatables.map(gain(_, time)).sum)
        activatables.foreach(n =>
          val actionTime = time + costs(worker, n) + 1
          if (actionTime >= maxTime)
            profit
          else
            val newProfit = profit + gain(n, actionTime)
            maximum = Math.max(maximum, newProfit)
            val newWorkers = ((actionTime, n) :: workers.tail).sortBy(_._1)
            maximize(newWorkers.head._1, newWorkers, newProfit, visited + n)
        )
    maximize(0L, workers.map((0L, _)), 0L, Set.empty[A])
    maximum


}
