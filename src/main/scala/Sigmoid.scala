package ml

import scala.math._

class Sigmoid(var bias: Double) extends NeuronBehavior {
  def computeOutput(neuron: NeuronWithInputs, inputs: Map[NeuronWithConnections,Double]) =
    1 / (1 + exp(-inputs.values.sum - bias))
}

object Sigmoid {
  def input = new InputNeuron
  def hidden(bias: Double = 1.0) = new HiddenNeuron(new Sigmoid(bias))
  def output(bias: Double = 1.0) = new OutputNeuron(new Sigmoid(bias))
}

class Digits(hiddenN: Int) {

  val hidden = Range(0,hiddenN).toArray.map(_ => Sigmoid.hidden())

  val x = 28
  val y = 28
  val digits = 10

  var inputs = Seq[Seq[InputNeuron]]()

  Range(0, x+1).foreach { x =>
    var row = Seq[InputNeuron]()    
    Range(0, y+1).foreach { y =>
      val neuron = new InputNeuron
      hidden.foreach { h =>
        neuron.connect(h, 1.0)
      }
      row = row :+ neuron
    }
    inputs = inputs :+ row
  }

  var outputs = Seq[OutputNeuron]()
  Range(0, digits).foreach { o =>
    val out = Sigmoid.output(1.0)
    outputs = outputs :+ out
    hidden.foreach { h =>
      h.connect(out, 1.0)
    }
  }

  val noMatch = Range(0, digits).map(_ => 0.0).toSeq

  // def ideal(in: Int) = (nMatch(in) = 1)
}