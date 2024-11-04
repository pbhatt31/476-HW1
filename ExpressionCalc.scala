import scala.collection.mutable
import ExpressionCalc.ExpOperation.:=

// new language
object PBJ {
  // define the types for variables and values
  type Value = Any
  type Environment = mutable.Map[String, Value]
  type Stack = mutable.Stack[Environment]

  // create type for class
  case class Class(name: String,
                   vars: List[ClassVar] = List(),
                   methods: List[Method] = List(),
                   superClass: Option[Class] = None)

  // class variable with name and type
  case class ClassVar(name: String, varType: VarType)

  // class variable type with name
  case class VarType(name: String)

  // class for method which has parameters and the expression
  case class Method(name: String, 
                    parameters: List[Parameter], 
                    body: ExpOperation)

  // class for method parameter with name and type
  case class Parameter(name: String, paramType: VarType)

  given StackVal: Stack = mutable.Stack(mutable.Map[String, Value]()) // initialize env w/ empty

  // instance of class
  def CreateNew(cls: Class): Instance = {
    // create new env for instance and populate the variables
    val instanceEnv = mutable.Map[String, Value]()
    populateInheritedVars(cls, instanceEnv)
    new Instance(cls, instanceEnv) // return instance
  }

  // populate the variables from the superclasses
  private def populateInheritedVars(cls: Class, env: Environment): Unit = {
    cls.superClass.foreach(sc => populateInheritedVars(sc, env))
    cls.vars.foreach(v => env.put(v.name, defaultForType(v.varType)))
  }

  private def defaultForType(varType: VarType): Value = varType.name match {
    case "int" => 0
    case "string" => ""
    case _ => null
  }
}

// instance of class
class Instance(val cls: PBJ.Class, val env: PBJ.Environment) {
  import PBJ._

  // method example
  def InvokeMethod(methodName: String, args: List[(String, Value)]): Value = {

    // find method in class/superclass
    val method = cls.methods.find(_.name == methodName)
      .orElse(cls.superClass.flatMap(_.methods.find(_.name == methodName)))
      .getOrElse(throw new Exception(s"Method $methodName not found in class ${cls.name}"))

    // method call
    val methodEnv = mutable.Map[String, Value]()

    // method execution
    method.parameters.zip(args).foreach { case (param, (name, value)) =>
      methodEnv(param.name) = value
    }

    // push method env onto stack
    summon[Stack].push(methodEnv)

    // evaluate method, pop env after and then return result of execution
    val result = ExpressionCalc.eval(method.body)
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
  case Union(p1: ExpOperation, p2: ExpOperation)
  case Intersect(p1: ExpOperation, p2: ExpOperation)
  case AndVal(p1: ExpOperation, p2: ExpOperation)
  case OrVal(p1: ExpOperation, p2: ExpOperation)
  case NotVal(p: ExpOperation)
  case XorVal(p1: ExpOperation, p2: ExpOperation)
  case Scope(body: ExpOperation)

  case Scope(body: ExpOperation) // nested scope for scoping rules
}

import PBJ._
import ExpOperation._

// evaluating expressions in methods
object ExpressionCalc {
  def eval(exp: ExpOperation): Value = exp match {
    case Value(v) => v

    case Variable(name) =>
      summon[Stack].find(_.contains(name))
        .map(_.apply(name))
        .getOrElse(throw new Exception(s"Variable $name not found"))

    case Assign(name, expr) =>
      val value = eval(expr)
      summon[Stack].top(name) = value
      value

    case Add(p1, p2) =>
      (eval(p1), eval(p2)) match {
        case (a: Int, b: Int) => a + b
        case (a: Double, b: Double) => a + b
        case _ => throw new Exception("Incompatible types for addition")
      }

    case Mult(p1, p2) =>
      (eval(p1), eval(p2)) match {
        case (a: Int, b: Int) => a * b
        case (a: Double, b: Double) => a * b
        case _ => throw new Exception("Incompatible types for multiplication")
      }

    case Union(p1, p2) =>
      (eval(p1), eval(p2)) match {
        case (a: Set[Any], b: Set[Any]) => a union b
        case _ => throw new Exception("Union operation requires sets")
      }

    case Intersect(p1, p2) =>
      (eval(p1), eval(p2)) match {
        case (a: Set[Any], b: Set[Any]) => a intersect b
        case _ => throw new Exception("Intersection operation requires sets")
      }

    case AndVal(p1, p2) =>
      (eval(p1), eval(p2)) match {
        case (a: Boolean, b: Boolean) => a && b
        case (a: Int, b: Int) => a & b
        case _ => throw new Exception("AndVal operation requires booleans or integers")
      }

    case OrVal(p1, p2) =>
      (eval(p1), eval(p2)) match {
        case (a: Boolean, b: Boolean) => a || b
        case (a: Int, b: Int) => a | b
        case _ => throw new Exception("OrVal operation requires booleans or integers")
      }

    case NotVal(p) =>
      eval(p) match {
        case a: Boolean => !a
        case a: Int => ~a
        case _ => throw new Exception("NotVal operation requires a boolean or integer")
      }

    case XorVal(p1, p2) =>
      (eval(p1), eval(p2)) match {
        case (a: Boolean, b: Boolean) => a ^ b
        case (a: Int, b: Int) => a ^ b
        case _ => throw new Exception("XorVal operation requires booleans or integers")
      }

    case Scope(body) =>
      val stackVal = summon[Stack]

      // push new env for scope, evaluate the expression and then pop the env
      stackVal.push(mutable.Map[String, Value]())
      val result = eval(body)
      stackVal.pop()
      result
  }
}

// test cases
object TestCases {
  import org.scalatest.flatspec.AnyFlatSpec
  import org.scalatest.matchers.should.Matchers

  class OOLangTest extends AnyFlatSpec with Matchers {
    behavior of "PBJ Language with OOP features"

    it should "create instances and handle inheritance" in {
      val baseClass = Class("Base", List(ClassVar("x", VarType("int"))))
      val derivedClass = Class("Derived", List(ClassVar("y", VarType("int"))), superClass = Some(baseClass))

      val baseInstance = CreateNew(baseClass)
      baseInstance.env("x") shouldBe 0

      val derivedInstance = CreateNew(derivedClass)
      derivedInstance.env("x") shouldBe 0
      derivedInstance.env("y") shouldBe 0
    }

    it should "invoke methods with dynamic dispatch" in {
      val baseClass = Class("Base", methods = List(Method("foo", List(Parameter("a", VarType("int"))), Add(Value(1), Variable("a")))))
      val derivedClass = Class("Derived", methods = List(Method("foo", List(Parameter("a", VarType("int"))), Add(Value(2), Variable("a")))), superClass = Some(baseClass))

      val derivedInstance = CreateNew(derivedClass)
      derivedInstance.InvokeMethod("foo", List(("a", 3))) shouldBe 5 
    }
  }
}