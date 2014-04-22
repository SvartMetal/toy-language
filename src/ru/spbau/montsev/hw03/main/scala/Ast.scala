package ru.spbau.montsev.hw03.main.scala

import Language._

trait Expr

case class IntLiteral(value: BigInt) extends Expr

case class DoubleLiteral(value: BigDecimal) extends Expr

case class StringLiteral(value: String) extends Expr

case class BooleanLiteral(value: Boolean) extends Expr

case class Assign(target: Expr, expr: Expr) extends Expr

case class Print(node: Expr) extends Expr

case class Println(node: Expr) extends Expr

case class Call(target: Expr, args: List[Expr]) extends Expr

case class Range(left: Expr, right: Expr) extends Expr

case class If(cond: Expr, thenBody: Block, elseBody: Option[Block]) extends Expr

case class While(cond: Expr, body: Block) extends Expr

case class For(variable: Var, range: Range, body: Block) extends Expr

case class Block(expressions: List[Expr]) extends Expr

case class Struct(body: List[Assign]) extends Expr

case class Lambda(args: Map[Name, Expr], body: Block) extends Expr

case class Var(name: Name) extends Expr

case class Invocation(target: Expr, field: Var) extends Expr

case class Binary(left: Expr, right: Expr, op: Name) extends Expr

case class Unary(arg: Expr, op: String) extends Expr

