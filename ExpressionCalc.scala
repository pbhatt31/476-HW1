import scala.collection.mutable
import ExpressionCalc.ExpOperation.:=

// language & class definitions for PBJ
object PBJ {
  // define types
  type Value = Any
  type Environment = mutable.Map[String, Value]
  type Stack = mutable.Stack[Environment]

  // class definition
  case class Class(
      name: String,
      vars: List[ClassVar] = List(),
      methods: List[Method] = List(),
      superClass: Option[Class] = None
  )

  // define class variable, variable type, method, and parameter
  case class ClassVar(name: String, varType: VarType)
  case class VarType(name: String)
  case class Method(name: String, parameters: List[Parameter], body: ExpOperation)
  case class Parameter(name: String, paramType: VarType)

  given StackVal: Stack = mutable.Stack(mutable.Map[String, Value]())

  // create new instance of a class
  def CreateNew(cls: Class): Instance = {
    val instanceEnv = mutable.Map[String, Value]()
    populateInheritedVars(cls, instanceEnv)
    new Instance(cls, instanceEnv)
  }

  // populate inherited variables
  private def populateInheritedVars(cls: Class, env: Environment): Unit = {
    // recursion
    cls.superClass.foreach(sc => populateInheritedVars(sc, env))
    cls.vars.foreach(v => env.put(v.name, defaultForType(v.varType)))
  }

  // give default values for variable types
  private def defaultForType(varType: VarType): Value = varType.name match {
    case "int"    => 0
    case "string" => ""
    case _        => null
  }
}

// class instance
class Instance(val cls: PBJ.Class, val env: PBJ.Environment) {
  import PBJ._

  def InvokeMethod(methodName: String, args: List[(String, Value)]): ExpOperation = {
    val method = cls.methods
      .find(_.name == methodName)
      .orElse(cls.superClass.flatMap(_.methods.find(_.name == methodName)))
      .getOrElse(throw new Exception(s"Method $methodName not found in class ${cls.name}"))

    // create new environment
    val methodEnv = mutable.Map[String, Value]()
    method.parameters.zip(args).foreach { case (param, (_, value)) =>
      methodEnv(param.name) = value
    }

    summon[Stack].push(methodEnv)
    val result = ExpressionCalc.partialEval(method.body)
    summon[Stack].pop()
    result
  }
}

// expression operations
enum ExpOperation {
  case Value(v: PBJ.Value)
  case Variable(name: String)
  case Assign(name: String, expr: ExpOperation)
  case Add(p1: ExpOperation, p2: ExpOperation)
  case Mult(p1: ExpOperation, p2: ExpOperation)
  case IFTRUE(cond: ExpOperation, thenBranch: ExpOperation, elseBranch: ExpOperation)
  case Scope(body: ExpOperation)
  case GreaterEqual(p1: ExpOperation, p2: ExpOperation)
}

import PBJ._
import ExpOperation._

// evaluator for operations
object ExpressionCalc {
  // eval() for evaluating expression & returning the value
  def eval(exp: ExpOperation): Value = exp match {
    case Value(v) => v
    case Variable(name) =>
      summon[Stack].find(_.contains(name)).map(_.apply(name)).getOrElse(throw new Exception(s"Variable $name not found"))

    case Assign(name, expr) =>
      val value = eval(expr)
      summon[Stack].top(name) = value
      value

    case Add(p1, p2) =>
      (eval(p1), eval(p2)) match {
        case (a: Int, b: Int) => a + b
        case _                => throw new Exception("Invalid types for addition")
      }

    case Mult(p1, p2) =>
      (eval(p1), eval(p2)) match {
        case (a: Int, b: Int) => a * b
        case _                => throw new Exception("Invalid types for multiplication")
      }

    case GreaterEqual(p1, p2) =>
      (eval(p1), eval(p2)) match {
        case (a: Int, b: Int)     => a >= b
        case (a: Double, b: Double) => a >= b
        case _                    => throw new Exception("Invalid types for comparison")
      }

    case IFTRUE(cond, thenBranch, elseBranch) =>
      eval(cond) match {
        case true  => eval(thenBranch)
        case false => eval(elseBranch)
        case _     => throw new Exception("Condition must be boolean")
      }

    case Scope(body) =>
      summon[Stack].push(mutable.Map[String, Value]())
      val result = eval(body)
      summon[Stack].pop()
      result
  }

  def partialEval(exp: ExpOperation): ExpOperation = exp match {
    case GreaterEqual(Value(a: Int), Value(b: Int)) => Value(a >= b)
    case GreaterEqual(p1, p2) => GreaterEqual(partialEval(p1), partialEval(p2))
    case Add(Value(a: Int), Value(b: Int)) => Value(a + b)
    case Mult(Value(a: Int), Value(b: Int)) => Value(a * b)
    case Mult(Value(a: Int), Mult(Value(b: Int), rest)) => Mult(Value(a * b), partialEval(rest))
    case Add(p1, p2) => Add(partialEval(p1), partialEval(p2))
    case Mult(p1, p2) => Mult(partialEval(p1), partialEval(p2))
    case IFTRUE(cond, thenBranch, elseBranch) =>
      IFTRUE(partialEval(cond), partialEval(thenBranch), partialEval(elseBranch))
    case _ => exp
  }
}

// test cases for partial evaluation
object TestCases {
  import org.scalatest.flatspec.AnyFlatSpec
  import org.scalatest.matchers.should.Matchers

  class PartialEvalTests extends AnyFlatSpec with Matchers {
    behavior of "Partial Evaluation"

    it should "partially evaluate arithmetic expressions" in {
      val expr = Mult(Value(3), Mult(Add(Value(5), Value(1)), Variable("x")))
      val result = ExpressionCalc.partialEval(expr)
      val expected = Mult(Value(3), Mult(Value(6), Variable("x")))
      result.toString shouldBe expected.toString
    }

    it should "partially evaluate conditionals" in {
      val cond = IFTRUE(
        GreaterEqual(Mult(Value(15), Variable("x")), Add(Value(2), Variable("y"))),
        Assign("lhs", Add(Variable("x"), Value(3))),
        Assign("lhs", Value(0))
      )
      val result = ExpressionCalc.partialEval(cond)
      val expected = IFTRUE(
        GreaterEqual(Mult(Value(15), Variable("x")), Add(Value(2), Variable("y"))),
        Assign("lhs", Add(Variable("x"), Value(3))),
        Assign("lhs", Value(0))
      )
      result.toString shouldBe expected.toString
    }
  }
}