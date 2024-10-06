# 476-HW1
CS 476 HW 1

Documentation: 
To compile the program, use "sbt run" and to test the program, use "sbt test". 

Explanation of code:
- First I created PBJ which takes from the Fuzzy language. I set up the types for values, elements, and sets and also everything needed to edit the scope.
- Then I needed to create cases for all the different operations that the langauage can do like assigning values, union, intersection, etc. These were done like so: "case AssignVal(name: String, expr: ExpOperation)" for each respective operation.
- Then I needed to create cases for how these operations would actually be implemented. As an example for assigning values:
  case AssignVal(name, expr) =>
      val newVal = eval(expr)
      summon[Stack].top.update(name, newVal)
      newVal
- I then had scala test cases to test each operation so I could tell if each operation was functioning as it should. For example, here's the test case I wrote to test the union operation:
  it should "test union" in {
          val result1 = eval(Union(Value(0.2), Value(0.6)))
          result1 shouldBe 0.6
        }

Semantics:
- The fuzzy language and the PBJ language deals with fuzzy sets, variables and operations. The set operations include union, intersection, xor, not, addition and multiplication.
- Like we learned in class and from the book, this program uses scoping so logic gates and variables can be used for different things depending on the scope its declared in.

Limitations:
- Other than the given set operations, there's nothing else the languages can really be used for.
- Fuzzy and PBJ can't tell the difference between the sets and the independent variables.
