import ExpressionCalc.ExpOperation.:=

import scala.collection.mutable

// new language
object PBJ:
  type Value = Double
  type Element = String
  type Set = mutable.Map[Element, Value]

  // for scope
  type Environment = mutable.Map[String, Value]
  type Stack = mutable.Stack[Environment]
  type Context = Stack ?=> Value
  given StackVal: Stack = mutable.Stack(mutable.Map[String, Value]())

object ExpressionCalc:
  import PBJ.*
//  type EnvironmentTable = mutable.Map[String, Int]
//  type EnvironmentTableContext = EnvironmentTable ?=> Int
//  given envTable: EnvironmentTable = mutable.Map("evenNumber" -> 2)
//  given envTable1: EnvironmentTable = mutable.Map("evenNumber" -> 2)

  def myScopeBorder(sc: => Int) :Int = sc

  myScopeBorder {
    myScopeBorder{
      val x = 5
      x
    }
    val x = 3
    x
  }

  def f(v: Int) = v+2
  def ff(v: Int) = ???//g+2

  def outerScope(param: String) = {
    var vasu = 476
    println(s"outer = $param")

    def innerScope(param:String) = {
      def vasuScope(param:String) = {
        val vasu = Map("c"->5)
        println(vasu)
      }
      vasu = 1
//      val vasu = "441"
      println(s"inner = $param")
      println(vasu)
      vasuScope("fff")
    }
    innerScope(param + "6")

  }

  // fuzzy set

  enum ExpOperation:
    case Value(v: Value)
    case Variable(name: String)

    case AssignVal(name: String, expr: ExpOperation)
    case Union(p1: ExpOperation, p2: ExpOperation)
    case Intersect(p1: ExpOperation, p2: ExpOperation)
    case AndVal(p1: ExpOperation, p2: ExpOperation)
    case OrVal(p1: ExpOperation, p2: ExpOperation)
    case NotVal(p: ExpOperation)
    case XorVal(p1: ExpOperation, p2: ExpOperation)
    case Scope(body: ExpOperation)

    case Add(p1: ExpOperation, p2: ExpOperation)
    case Mult(p1: ExpOperation, p2: ExpOperation)
//    case Sub(p1: ExpOperation, p2: ExpOperation)
    case :=(variable: Variable, v: Value)
    
//    case PBJSet()
//    case Xyz()

  import ExpOperation.*

  def eval(exp: ExpOperation): Context = exp match
    case Value(v) => v
    
    case Variable(name) =>
      summon[Stack].find(_.contains(name)) match
        case Some(a) => a(name)
        case None => throw new Exception(s"$name not found")

    case AssignVal(name, expr) =>
      val newVal = eval(expr)
      summon[Stack].top.update(name, newVal)
      newVal

    case Union(p1, p2) =>
      Math.max(eval(p1), eval(p2))

    case Intersect(p1, p2) =>
      Math.min(eval(p1), eval(p2))
      
      // case for and val
    case AndVal(p1, p2) =>
      eval(p1) * eval(p2)
      
      
      // case for or val
    case OrVal(p1, p2) =>
      Math.max(eval(p1), eval(p2))

    case NotVal(p) =>
      1.0 - eval(p)

    case XorVal(p1, p2) =>
      Math.abs(eval(p1) - eval(p2))

    case Scope(body) =>
      val stackVal = summon[Stack]
      stackVal.push(mutable.Map[String, Value]())
      val result = eval(body)
      stackVal.pop()
      result

    case Add(p1, p2) =>
      Math.min(1.0, eval(p1) + eval(p2))
  
    case Mult(p1, p2) =>
      eval(p1) * eval(p2)
      
    // test cases
    object TestCases:
      import org.scalatest.flatspec.AnyFlatSpec
      import org.scalatest.matchers.should.Matchers
      
      class Test1 extends AnyFlatSpec with matchers {
        behavior of "PBJ language"
        
        it should "test union" in {
          val result1 = eval(Union(Value(0.2), Value(0.6)))
          result1 shouldBe 0.6
        }
        
        // test case for intersection
        it should "test intersection" in {
          val result2 = eval(Intersect(Value(0.5), Value(0.6)))
          result2 shouldBe 0.5
        }
        
        // test case for and
        it should "test and" in {
          val result3 = eval(AndVal(Value(0.5), Value(0.6)))
          result3 shouldBe 0.30
        }
        
        // test case for or
      }

    

    
//
//  def eval(exp: ExpOperation): EnvironmentTableContext = exp match
//    case ExpOperation.Value(i) => i
//    case ExpOperation.Variable(s) => summon[EnvironmentTable].getOrElse(s, throw new Exception("not found"))
//    case ExpOperation.Add(p1, p2) =>
//      while(true) do {}
//      eval(p1) + eval(p2)
//    case ExpOperation.Mult(p1, p2) => eval(p1) * eval(p2)
//    case ExpOperation.Sub(p1, p2) => eval(p1) - eval(p2)
//    case :=(v, i) =>
//      summon[EnvironmentTable] ++= mutable.Map(v.s -> i.i)
//      i.i

//  def main(args: Array[String]): Unit = {
//    import ExpOperation.*
//    outerScope("5")
//
//    println(
//      eval(Add(Value(2),Value(7)))
//    )
//    println {
//      val x = Add(Value(3),Value(5))
//      Add(Mult(Value(2),x),Add(Value(5),Value(7)))
//    }
//    println(
//      eval(:=(Variable("x"), Value(5)))
//    )
//    println(
//      eval(Add(Mult(Variable("x"), Add(Value(3), Value(5))), Sub(Value(5), Value(7))))
//    )
//  }