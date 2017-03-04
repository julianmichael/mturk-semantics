package mts.experiments.expH

object WordStats {
  def newStats(validIndices: Set[Int]): WordStats =
    validIndices.iterator.map(i => i -> WordStat.empty(i)).toMap

  def add(stats: WordStats, stat: WordStat): WordStats =
    stats.updated(
      stat.index,
      stats.get(stat.index) match {
        case None => // this case actually should never happen
          System.err.println("adding word stat to word that shouldn't have it")
          stat
        case Some(ws) => WordStat(
          stat.index,
          ws.numPrompted + stat.numPrompted,
          ws.numQAPairs + stat.numQAPairs,
          ws.numQAPairsContaining + stat.numQAPairsContaining)
      })

  def merge(s1: WordStats, s2: WordStats) =
    (s1.keySet ++ s2.keySet).iterator.map { i =>
      i -> WordStat(
        i,
        s1(i).numPrompted + s2(i).numPrompted,
        s1(i).numQAPairs + s2(i).numQAPairs,
        s1(i).numQAPairsContaining + s2(i).numQAPairsContaining)
    }.toMap

  def indicesByPriority(stats: WordStats): List[Int] = {
    stats.keys.toList // TODO
  }
}

object WordStat {
  def empty(i: Int) = WordStat(i, 0, 0, 0)
}

case class WordStat(
  index: Int,
  numPrompted: Int,
  numQAPairs: Int,
  numQAPairsContaining: Int)
