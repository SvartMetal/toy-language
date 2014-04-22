package ru.spbau.montsev.hw03.main.scala

import Language._

class Error(msg: String) extends Exception

class ExpectedBooleanError(cond: Expr, condValue: Data)
  extends Error(s"Condition value must be boolean value. Condition: $cond, Value evaluated: $condValue. ")

class InvalidFunctionCallError(expr: Expr, args: List[Expr])
  extends Error(s"Invalid function call: $expr. With args: $args. ")

class InvalidRangeError(range: Expr, rangeValue: Data)
  extends Error(s"Invalid range: $range, Value evaluated: $rangeValue. ")

class ExpectedIntError(value: Data)
  extends Error(s"Expected integer, found: $value. ")

class ExpectedVarDefError(value: Expr)
  extends Error(s"Expected variable definition, found: $value. ")

class FieldNotFoundError(name: Name)
  extends Error(s"Field with name: $name not found. ")

class InvalidInvocationError(expr: Expr, field: Name)
  extends Error(s"Invalid invocation on: $expr with field: $field. ")

class BinaryOpNotFoundError(op: Name)
  extends Error(s"Binary operation $op not found. ")

class UnaryOpNotFoundError(op: Name)
  extends Error(s"Unary opeation $op not found. ")

class UnknownExprError(expr: Expr)
  extends Error(s"Unknown AST expression: $expr")

class UndefinedVariableError(name: Name)
  extends Error(s"Undefined variable: $name. ")

class UpdateError(name: Name, value: Data)
  extends Error(s"Can't update variable: $name in empty context. With value: $value. ")

