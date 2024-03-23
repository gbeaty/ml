package ml

class NandTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Nand gate") {
    import NandGate._
    assert(x1.state === None)
    assert(x2.state === None)
    assert(out.state === None)

    x1.set(0)
    assert(x1.state === Some(0))
    assert(x2.state === None)
    assert(out.state === None)

    x2.set(0)
    assert(x1.state === Some(0))
    assert(x2.state === Some(0))
    assert(out.state === Some(1))
  }

  test("Nand adder") {
    import NandAdder._

    assert(center.weights === Map(x1 -> -2, x2 -> -2))
    assert(center.outs === Set(top, bottom, carry))

    assert(top.weights === Map(x1 -> -2, center -> -2))
    assert(top.outs === Set(sum))

    x1.set(0)
    x2.set(0)
    assert(center.state === Some(1))
    assert(top.ins === Map(x1 -> 0, center -> -2))
    assert(top.state === Some(1))
    assert(bottom.state === Some(1))
    assert(sum.state === Some(0))
    assert(carry.state === Some(0))

    x1.set(0)
    x2.set(1)
    assert(sum.state === Some(1))
    assert(carry.state === Some(0))

    x1.set(1)
    x2.set(0)
    assert(sum.state === Some(1))
    assert(carry.state === Some(0))

    x1.set(1)
    x2.set(1)
    assert(sum.state === Some(0))
    assert(carry.state === Some(1))
  }
}