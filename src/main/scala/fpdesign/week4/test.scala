package fpdesign.week4

object test {
  def main(args: Array[String]) = {
    object sim extends Circuits with Paramters
    import sim._
    val in1, in2, sum, carry = new Wire
    halfAdder(in1, in2, sum, carry)
    probe("sum", sum)

    in1 setSignal true
    run()

    in2 setSignal true
    run()

    in1 setSignal false
    run()
  }
}
