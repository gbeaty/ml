package ml

class Perceptron(var bias: Double) extends NeuronBehavior {
  def computeOutput(neuron: NeuronWithInputs, inputs: Map[NeuronWithConnections,Double]) = if(inputs.values.sum + bias > 0) 1.0 else 0.0
}
object Perceptron {
  def input = new InputNeuron
  def hidden(bias: Double) = new HiddenNeuron(new Perceptron(bias))
  def output(bias: Double) = new OutputNeuron(new Perceptron(bias))
}
object Nand {
  def input = Perceptron.input
  def hidden = Perceptron.hidden(3)
  def output = Perceptron.output(3)
}

object NandGate {
  import Nand._

  val x1 = input
  val x2 = input
  val out = output

  x1.connect(out, -2)
  x2.connect(out, -2)
}

object NandAdder {
  import Nand._

  val x1 = input
  val x2 = input
  val center = hidden
  val top = hidden
  val bottom = hidden
  val sum = output
  val carry = output

  x1.connect(center, -2)
  x1.connect(top, -2)

  x2.connect(center, -2)
  x2.connect(bottom, -2)
  
  center.connect(top, -2)
  center.connect(bottom, -2)
  center.connect(carry, -4)

  top.connect(sum, -2)

  bottom.connect(sum, -2)
}