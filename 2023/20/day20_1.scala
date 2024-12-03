@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

//  val sampleLines = List(
//    "broadcaster -> a, b, c",
//    "%a -> b",
//    "%b -> c",
//    "%c -> inv",
//    "&inv -> a"
//  )

  val sampleLines = List(
    "broadcaster -> a",
    "%a -> inv, con",
    "&inv -> b",
    "%b -> con",
    "&con -> output"
  )

  val inputs: List[InputToOutput] = sampleLines.map { line =>
    line.split("->") match {
      case Array(input, output) =>
        InputToOutput(inputToModule(input.trim), output.split(",").toList.map(_.trim))
    }
  }

  val conjunctions = inputs.filter {
      case InputToOutput(m: Conjunction, _) => true
      case _ => false
    }
    .map {
      case i @ InputToOutput(c @ Conjunction(mod, _), output) =>
        val conjunctionInputs = inputs.collect {
          case InputToOutput(module, output) if output.contains(mod) => module
        }

        i.copy(module = c.copy(inputs = conjunctionInputs.map((_, Low))))
    }

  val nonConjunctions = inputs.filter {
    case InputToOutput(m: Conjunction, _) => false
    case _ => true
  }

  val inputsRejoined = InputToOutput(Button, List("broadcaster")) :: InputToOutput(Output, List.empty) :: (conjunctions ++ nonConjunctions)

  val (pulseCount, state) = iterateSignalsNew(List((Low, Button, "broadcaster")), inputsRejoined, (0, 0))

//  inputsRejoined.foreach(println)
//
//  inputsRejoined.zipWithIndex.find {
//    case (InputToOutput(module, _), index) =>
//      module.name == "con"
//  }

  println(pulseCount)

}

sealed trait Pulse
case object High extends Pulse
case object Low extends Pulse

sealed trait Module {
  def nextPulse(incomingPulse: Pulse, module: Module): (Module, Option[Pulse])
  val name: String
}

case object Output extends Module {
  override def nextPulse(incomingPulse: Pulse, module: Module) = (this, None)
  val name = "output"
}

case object Button extends Module {
  override def nextPulse(incomingPulse: Pulse, module: Module) = (this, Some(Low))
  val name = "button"
}

case object Broadcaster extends Module {
  override def nextPulse(incomingPulse: Pulse, module: Module) = (this, Some(Low))
  val name = "broadcaster"
}

case class FlipFlop(mod: String, on: Boolean) extends Module {
  override def nextPulse(incomingPulse: Pulse, module: Module) =
    if (incomingPulse == High) (this, None)
    else {
      if (on) (this.copy(on = false), Some(Low))
      else (this.copy(on = true), Some(High))
    }

  val name = mod
}

case class Conjunction(mod: String, inputs: List[(Module, Pulse)]) extends Module {
  override def nextPulse(incomingPulse: Pulse, module: Module) = {
    val updatedInputs = inputs.zipWithIndex.find {
      case ((mod, _), _) => mod.name == module.name
    }.map {
      case ((mod, pulse), index) =>
        if (pulse == incomingPulse) inputs
        else inputs.updated(index, (mod, incomingPulse))
    }.getOrElse(inputs)

    val pulseToSend =
      if (updatedInputs.map(_._2).forall(_ == High)) Some(Low)
      else if (updatedInputs.map(_._2).forall(_ == Low)) Some(High)
      else None

    (this.copy(inputs = updatedInputs), pulseToSend)
  }

  val name = mod
}

case class InputToOutput(module: Module, output: List[String])

def inputToModule(input: String) =
  input.head match {
    case 'b' => Broadcaster
    case '%' => FlipFlop(input.drop(1), false)
    case '&' => Conjunction(input.drop(1), List.empty)
  }

def updateConjunctions(ls: List[InputToOutput], signal: Pulse, module: Module): List[InputToOutput] =
  ls.filter {
    case InputToOutput(module @ Conjunction(_, _), output) =>
      output.contains(module.name)
  }
    .map {
      case InputToOutput(c @ Conjunction(mod, inputs), output) =>
        val newMemoryIn = inputs.indexOf(_._1.name == module.name)

        InputToOutput(c.copy(mod, inputs.updated(newMemoryIn, (module, signal))), output)
    }

def iterateSignalsNew(ls: List[(Pulse, Module, String)], conjunctions: List[InputToOutput], moduleStates: List[InputToOutput], acc: (Int, Int)): ((Int, Int), List[InputToOutput]) = {
  println(ls)

  (ls.headOption, acc) match {
    case (Some((pulse, incomingModule, moduleName)), (lowPulseAcc, highPulseAcc)) =>

      val newAcc = pulse match {
        case Low => (lowPulseAcc + 1, highPulseAcc)
        case High => (lowPulseAcc, highPulseAcc + 1)
      }

      val (module, index) = moduleStates.zipWithIndex.find {
        case (InputToOutput(module, _), index) => module.name == moduleName
      }.get

      val (newModuleState, signalOpt) = module.module.nextPulse(pulse, incomingModule)

      val newModuleStates = moduleStates.updated(index, module.copy(module = newModuleState))

      val newPulseList = signalOpt.map { signal =>
        module.output.map { name =>
          (signal, newModuleState, newModuleStates.find(_.module.name == name).get.module.name)
        }
      }.getOrElse(List.empty)

      iterateSignalsNew(ls.drop(1) ++ newPulseList, newModuleStates, newAcc)
    case _ => (acc, moduleStates)
  }
}