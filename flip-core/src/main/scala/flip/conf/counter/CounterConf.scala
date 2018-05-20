package flip.conf.counter

trait CounterConf {
  val size: Int
  val no: Int
}

object CounterConf {

  case class HCounterConfImpl(size: Int, no: Int) extends CounterConf

  def apply(size: Int, no: Int): CounterConf = HCounterConfImpl(size, no)

}
