# Feedback for Homework 2

## Score: 8.7/15

1. -1%: No instructions in the README on how to install and run the program.
2. -3%: Project does not build; compilation error.
3. -0.7%: Insufficient documentation for design and implementation of object-orientation.
4. -0.4%: DSL does not support nested classes.
5. -1.2%: <5 unit/integration tests

**Please keep your source code clean and well-documented so that it's easy to parse.**

---

# Feedback for Homework 1

## Score: 10.6/15

1. -0.3%: One use of `var`.
2. -1.5%: Missing the `TestGate` construct.
3. -1.5%: Compilation (build) failed; incorrect project structure.
4. -0.4%: <5 unit/integration tests.
5. -0.7%: Logic gates are not *reusable* with different inputs.

```scala
// Example of gate reuse; assume this in your syntax
Assign(LogicGate("exampleGate"), ADD(MULTIPLY(v1, v2)))
Eval("exampleGate", Inputs(i1, i2))
Eval("exampleGate", Inputs(i3, i4)) // exampleGate reused
```

Is this possible in your DSL?

**Please keep your source code clean and well-documented so that it's easy to parse.**

---