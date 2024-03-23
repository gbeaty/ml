package ml

import scala.util.Random

trait NeuronBehavior {
  def computeOutput(neuron: NeuronWithInputs, inputs: Map[NeuronWithConnections,Double]): Double
}

trait Neuron {
  protected var value: Option[Double] = None

  def isComplete = value.isDefined

  def processOutput(out: Double)

  def state = value
}
trait NeuronWithConnections extends Neuron {
  var outputs = Set[NeuronWithInputs]()

  def outs = outputs

  def connect(to: NeuronWithInputs, weight: Double) {
    outputs += to
    to.connectFrom(this, weight)
  }

  def processOutput(out: Double) = outputs.foreach(_.input(this, out))
}
trait NeuronWithInputs extends Neuron {
  var inputs = Map[NeuronWithConnections,Double]()
  var cache = Map[NeuronWithConnections,Double]()
  def weights = inputs
  def ins = cache

  val behavior: NeuronBehavior

  def connectFrom(from: NeuronWithConnections, weight: Double) {
    inputs = inputs + (from -> weight)
  }
  def input(from: NeuronWithConnections, in: Double) {
    inputs.get(from).foreach { w =>
      cache = cache + (from -> w * in)

      if(cache.size == inputs.size) {
        val out = behavior.computeOutput(this, cache)
        value = Some(out)
        processOutput(out)
      }
    }
  }
}
class InputNeuron extends NeuronWithConnections {
  def set(out: Double) {
    if(Some(out) != value) {
      value = Some(out)
      processOutput(out)
    }
  }
}
class HiddenNeuron(val behavior: NeuronBehavior) extends NeuronWithInputs with NeuronWithConnections
class OutputNeuron(val behavior: NeuronBehavior) extends NeuronWithInputs {
  def processOutput(out: Double) = {}
}

class Network(val sizes: Seq[Int]) {
  val numInputs = sizes(0)
  val numOutputs = sizes(sizes.length - 1)
  val numLayers = sizes.length

  val hiddenSizes = sizes.tail.dropRight(1)

  val biases: Seq[Seq[Double]] = for (s <- sizes.tail)
    yield (for (n <- Range(0,s)) yield Random.nextDouble)
}