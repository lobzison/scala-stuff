object ToSmallest {

  def smallest(n: Long): Array[Long] = {
    val num = n.toString.toArray.zipWithIndex.map(x => (x._1.getNumericValue, x._2))

    def aux(m: Array[(Int, Int)]): Option[(Int, Int)] = {
      m match {
        case Array() => None
        case _ =>
          val h = m.head
          val t = m.tail
          val index =
            t.foldLeft(None: Option[(Int, Int)]){
              case (end, (x, ind)) if x < h._1 && x <= end.map(_._1).getOrElse(9) =>
                Some((x, ind))
              case (end, _) => end
            }
          index match {
            case None => aux(t)
            case Some((_, ind)) => Some(h._2, ind)
          }
      }
    }
    aux(num) match {
      case None => Array(n, 0L, 0L)
      case Some((from, to)) =>
        val nStr = n.toString
        val toNum = nStr(to)
        val fromNum = nStr(from)
        val newNum =
          nStr
          .updated(from, toNum)
          .updated(to, fromNum)
        Array(newNum.toLong, to, from)

    }
  }
}


ToSmallest.smallest(6453155895013300L)
//Testing 6453155895013300
//Actual --> 453155895013306, 15, 0
//Expect --> 645315589501330, 14, 0
//           645315589513300