package ru.spbau.montsev.hw03.main.scala

import scala.util.parsing.combinator.JavaTokenParsers
import ru.spbau.montsev.hw03.main.scala.Language.Name
import scala.annotation.tailrec

object Parser extends JavaTokenParsers {

  import Tokens._

  override type Elem = Char

  private def notWith[T](p: Parser[T], predicate: (T => Boolean)) = Parser {
    in =>
      p(in) match {
        case res@Success(s, m) =>
          if (predicate(s)) {
            Failure("Predicate failed. ", in)
          } else {
            res
          }
        case res@_ => res
      }
  }

  def intValue = wholeNumber ^^ {
    p => IntLiteral(BigInt(p))
  }

  def doubleValue =
    notWith(floatingPointNumber, (x: String) => x.matches( """-?\d+""")) ^^ {
      p => DoubleLiteral(BigDecimal(p))
    }

  def stringValue = stringLiteral ^^ {
    p => StringLiteral(p.stripPrefix("\"").stripSuffix("\""))
  }

  private def parenthesized[T](p: Parser[T]) = tPAR_BEGIN ~> p <~ tPAR_END

  def booleanValue = (tFALSE | tTRUE) ^^ {
    p => BooleanLiteral(p.toBoolean)
  }

  def value = doubleValue | intValue | booleanValue | stringValue

  def lambda = (argDefs <~ tARGS_END) ~ block ^^ {
    case defs ~ b => Lambda(defs.toMap, b)
  }

  def ifExpr: Parser[If] = tIF ~> parenthesized(expr) ~ block ~ (tELSE ~> block).? ^^ {
    case cond ~ body ~ optionElseBody => If(cond, body, optionElseBody)
  }

  def range: Parser[Range] = (expr <~ tRANGE_SEP) ~ expr ^^ {
    case l ~ r => Range(l, r)
  }

  def forExpr: Parser[For] = tFOR ~> parenthesized((variable <~ tGEN) ~ range) ~ block ^^ {
    case rangeVar ~ range ~ body => For(rangeVar, range, body)
  }

  def whileExpr: Parser[While] = tWHILE ~> parenthesized(expr) ~ block ^^ {
    case cond ~ body => While(cond, body)
  }

  def struct: Parser[Struct] = tSTRUCT_BEGIN ~> varDef.* <~ tSTRUCT_END ^^ Struct

  def expr: Parser[Expr] =
    factor ~ ((tPLUS | tMINUS | tAND | tOR | tXOR) ~ factor).* ^^ foldExpr

  private def foldExpr(e: ~[Expr, List[~[Name, Expr]]]): Expr = e match {
    case eLeft ~ rest => rest.foldLeft(eLeft)({
      case (left, op ~ right) => Binary(left, right, op)
    })
  }

  private def factor: Parser[Expr] =
    leaf ~ ((tMUL | tDIV | tREM | tEQ | tNEQ | tGEQ | tLEQ | tLE | tGE) ~ leaf).* ^^ foldExpr

  private def leaf: Parser[Expr] =
    printExpr |
      ifExpr |
      forExpr |
      whileExpr |
      lambda |
      assignment |
      callOrInvoke |
      struct |
      value |
      variable |
      block |
      parenthesized(expr)

  def assignment: Parser[Assign] = (callOrInvoke <~ tASSIGN) ~ expr ^^ {
    case t ~ v => Assign(t, v)
  }

  def varDef: Parser[Assign] = (notWith(ident, KEYWORDS.contains) <~ tASSIGN) ~ expr <~ tSTMT ^^ {
    case id ~ n =>
      Assign(Var(id), n)
  }

  private def callOn[T <: Expr](p: Parser[T]): Parser[Call] = p ~ callArgs.+ ^^ {
    case callee ~ argLists => transformCall(callee, argLists)
  }

  private def callArgs: Parser[List[Expr]] = parenthesized(repsep(expr, tCOMMA))

  private def transformCall(callee: Expr, argLists: List[List[Expr]]): Call = {
    argLists match {
      case Nil => throw new MatchError("Argument list is empty. ")
      case _ => argLists.tail.foldLeft(Call(callee, argLists.head))(Call(_, _))
    }
  }

  private def unwrapSimpleCall(call: Call): (Var, List[List[Expr]]) = {
    @tailrec
    def unwrapSimpleCall(call: Call, argLists: List[List[Expr]]): (Var, List[List[Expr]]) = {
      call match {
        case Call(x@Var(name), args) => (x, args :: argLists)
        case Call(c@Call(_, _), args) => unwrapSimpleCall(c, args :: argLists)
        case _ => throw new MatchError("Expected call or variable. ")
      }
    }
    unwrapSimpleCall(call, Nil)
  }

  private def transformCallOrInvoke(e: Expr, fields: List[Expr]): Expr = {
    fields.foldLeft(e) {
      case (struct, x: Var) => Invocation(struct, x)
      case (struct, c: Call) =>
        val (x, argLists) = unwrapSimpleCall(c)
        transformCall(Invocation(struct, x), argLists)
      case (_, _) =>
        throw new MatchError("Expected call or variable. ")
    }
  }

  def simpleCall: Parser[Call] = callOn(variable)

  def simpleCallOrVariable: Parser[Expr] = simpleCall | variable

  def arg: Parser[(Name, Expr)] = variable ~ (tASSIGN ~> expr).? ^^ {
    case n ~ optionDefaultValue => (n.name, optionDefaultValue match {
      case Some(x) => x
      case _ => Undefined
    })
  }

  def argDefs: Parser[List[(Name, Expr)]] = parenthesized(repsep(arg, tCOMMA))

  def callOrInvoke: Parser[Expr] = {
    (simpleCallOrVariable |
      parenthesized(assignment) |
      callOn(struct) |
      callOn(lambda) |
      callOn(parenthesized(callOrInvoke)) |
      parenthesized(callOrInvoke)) ~
      (tINVOKE ~> simpleCallOrVariable).* ^^ {
      case v ~ fields => transformCallOrInvoke(v, fields)
    }
  }

  def printExpr: Parser[Expr] =
    (tPRINTLN ~> tPAR_BEGIN ~> expr <~ tPAR_END ^^ Println) |
      (tPRINT ~> tPAR_BEGIN ~> expr <~ tPAR_END) ^^ Print

  def variable: Parser[Var] = (notWith(ident, KEYWORDS.contains) ^^ Var) | parenthesized(variable)

  def statement: Parser[Expr] =
    varDef | ifExpr | forExpr | whileExpr | expr <~ tSTMT | block

  def block: Parser[Block] = tBLOCK_BEGIN ~> statement.* <~ tBLOCK_END ^^ Block

  def tree: Parser[Expr] = statement.* ^^ Block

  def parse(s: String): Expr = {

    parseAll(tree, s) match {
      case Success(v, _) => v
      case _ => throw new IllegalArgumentException(s"Parse error with input: $s")
    }
  }

}
