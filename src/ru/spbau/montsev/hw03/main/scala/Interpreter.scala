package ru.spbau.montsev.hw03.main.scala

import Language._
import Interpreter._
import Tokens._

trait Evaluator {
  def eval(expr: Expr) {
    val env = Environment()
    evalExpr(expr, env)
  }

  def computeVarRef(arg: Data)(env: Environment): Data = {
    arg match {
      case VarRef(Some(o), name) =>
        o.vars(name)
      case VarRef(None, name) =>
        env.lookup(name)
      case data => data
    }
  }

  def printHandler(expr: Expr, printFunc: Any => Unit)(env: Environment) = {
    val (exprData, _) = evalExpr(expr, env)
    val computedData = computeVarRef(exprData)(env)
    printFunc(computedData)
    (Undefined, env)
  }

  def evalExpr(expression: Expr, env: Environment): (Data, Environment) = expression match {
    case IntLiteral(value) =>
      (value, env)

    case DoubleLiteral(value) =>
      (value, env)

    case StringLiteral(value) =>
      (value, env)

    case BooleanLiteral(value) =>
      (value, env)

    case Assign(target, value) =>
      val (computedTarget, newTargetEnv) = evalExpr(target, env)
      val (valueData, newValEnv) = evalExpr(value, newTargetEnv)
      val computedData = computeVarRef(valueData)(env)
      computedTarget match {
        case VarRef(Some(o), name) =>
          o.vars += name -> computedData
        case VarRef(None, name) =>
          newValEnv.update(name, computedData)
      }
      (computedData, newValEnv)

    case Var(name) =>
      (VarRef(None, name), env)

    case Print(expr) =>
      printHandler(expr, print)(env)

    case Println(expr) =>
      printHandler(expr, println)(env)

    case Call(expr, args) =>
      val (calleeData, _) = evalExpr(expr, env)
      val computedCallee = computeVarRef(calleeData)(env)
      computedCallee match {
        case Fun(f) =>
          val computedArgs = args.map {
            case arg =>
              val (argData, _) = evalExpr(arg, env)
              argData
          }.map {
            computeVarRef(_)(env)
          }
          (f(computedArgs), env)
        case _ =>
          throw new InvalidFunctionCallError(expr, args)
      }

    case Range(left, right) =>
      val (leftData, _) = evalExpr(left, env)
      val computedLeft = computeVarRef(leftData)(env)
      val (rightData, _) = evalExpr(right, env)
      val computedRight = computeVarRef(rightData)(env)
      ((computedLeft, computedRight), env)

    case If(cond, thenBody, elseBody) =>
      val (condData, _) = evalExpr(cond, env)
      val condValue = computeVarRef(condData)(env)
      condValue match {
        case true =>
          evalExpr(thenBody, env)
        case false =>
          elseBody match {
            case Some(body) =>
              evalExpr(body, env)
            case None =>
              (Undefined, env)
          }
        case _ =>
          throw new ExpectedBooleanError(cond, condValue)
      }

    case While(cond, body) =>
      var whileFlag = true
      while (whileFlag) {
        val (condData, _) = evalExpr(cond, env)
        val condValue = computeVarRef(condData)(env)
        whileFlag = condValue match {
          case true =>
            evalExpr(body, env)
            true
          case false =>
            false
          case _ =>
            throw new ExpectedBooleanError(cond, condValue)
        }
      }
      (Undefined, env)

    case For(v, range, body) =>
      val name = v.name
      val newEnv = env.extend(name, Undefined)
      val (rangeValue, _) = evalExpr(range, env)
      rangeValue match {
        case (left: BigInt, right: BigInt) =>
          val extEnv = newEnv.extend(name, left)
          var counterData = extEnv.lookup(name)
          counterData match {
            case counterValue: BigInt =>
              var counter = counterValue
              while (counter >= left && counter <= right) {
                val (_, e) = evalExpr(body, extEnv)
                counterData = e.lookup(name)
                counterData match {
                  case t: BigInt =>
                    counter = t + 1
                    counterData = counter
                }
                e.update(name, counterData)
              }
          }
          (Undefined, env)
        case _ =>
          throw new InvalidRangeError(range, rangeValue)
      }

    case Block(exprs) =>
      val lastExpr = exprs.foldLeft((Undefined, env): (Data, Environment)) {
        (accDataAndEnv, expr) =>
          val (_, accEnv) = accDataAndEnv
          evalExpr(expr, accEnv)
      }
      val (lastExprData, lastExprEnv) = lastExpr
      val resultData = computeVarRef(lastExprData)(lastExprEnv)
      (resultData, env)

    case Struct(body) =>
      val (vars, _) = body.foldLeft((Map[Name, Data](), env)) {
        (accArgMapAndEnv, expr) =>
          expr match {
            case Assign(Var(name), value) =>
              val (argMap, accEnv) = accArgMapAndEnv
              val (data, newEnv) = evalExpr(value, accEnv)
              val extendedEnv = newEnv.extend(name, data)
              (argMap + (name -> data), extendedEnv)
            case _ =>
              throw new ExpectedVarDefError(expr)
          }
      }
      (new Object(vars), env)

    case Lambda(args, body) =>
      (Fun {
        lst =>
          val newEnv = args.foldLeft(env) {
            (accEnv, variable) =>
              val (name, expr) = variable
              val (data, _) = evalExpr(expr, env)
              accEnv.extend(name, data)
          }
          val argList = args.toList
          val funEnv = argList.zip(0 until argList.length).foldLeft(newEnv) {
            (accEnv, argIndexPair) =>
              val ((name, _), index) = argIndexPair
              if (index < lst.length) {
                accEnv.update(name, lst(index))
              }
              accEnv
          }
          val (resultData, _) = evalExpr(body, funEnv)
          resultData
      }, env)

    case Invocation(target, field) =>
      val fieldName = field.name
      val (targetData, _) = evalExpr(target, env)
      val computedTarget = computeVarRef(targetData)(env)
      val result = computedTarget match {
        case o: Object =>
          if (o.vars.contains(fieldName)) {
            VarRef(Some(o), fieldName)
          } else {
            throw new FieldNotFoundError(fieldName)
          }
        case _ =>
          throw new InvalidInvocationError(target, fieldName)
      }
      (result, env)

    case Binary(left, right, op) =>
      val (leftData, _) = evalExpr(left, env)
      val (rightData, _) = evalExpr(right, env)
      val computedLeft = computeVarRef(leftData)(env)
      val computedRight = computeVarRef(rightData)(env)
      env.lookup(op) match {
        case Fun(f) =>
          (f(List(computedLeft, computedRight)), env)
        case _ =>
          throw new BinaryOpNotFoundError(op)
      }

    case Unary(arg, op) =>
      val (argData, _) = evalExpr(arg, env)
      val computedArg = computeVarRef(argData)(env)
      env.lookup(op) match {
        case Fun(f) =>
          (f(List(computedArg)), env)
        case _ =>
          throw new UnaryOpNotFoundError(op)
      }

    case Undefined =>
      (Undefined, env)

    case _ =>
      throw new UnknownExprError(expression)

  }
}

case class Fun(f: List[Data] => Data) {
  override def toString = rLAMBDA
}

class Object(var vars: Map[Name, Data]) {
  override def toString = {
    val repr = vars.map(p => s"(name = ${p._1}, value = ${p._2});").reduce(_ + _)
    s"$tSTRUCT_BEGIN$repr$tSTRUCT_END"
  }
}

case class VarRef(owner: Option[Object], name: Name) {
  override def toString = name
}

case object Undefined extends Expr {
  override def toString = rUNDEFINED
}

object Evaluator {
  def apply() = new Evaluator {}
}

trait Environment {
  def lookup(name: Name): Data

  def contains(name: Name): Boolean

  def update(name: Name, value: Data)

  def vars: Map[Name, Data]

  def extend(name: Name, value: Data) = new Environment {
    var locals = Environment.this.vars + (name -> value)

    def vars = locals

    def lookup(name: Name): Data =
      if (vars.contains(name)) vars(name) else Environment.this.lookup(name)

    def contains(name: Name): Boolean =
      if (vars.contains(name)) true else Environment.this.contains(name)

    def update(name: Name, value: Data) {
      if (!contains(name)) {
        locals += name -> value
      } else {
        if (vars.contains(name)) {
          locals += name -> value
        } else {
          Environment.this.update(name, value)
        }
      }
    }
  }
}

object EmptyEnvironment extends Environment {
  def lookup(name: Name): Data = throw new UndefinedVariableError(name)

  def contains(name: Name) = false

  def update(name: Name, value: Data) {
    throw new UpdateError(name, value)
  }

  def vars = Map.empty
}

object Environment {
  def apply() = EmptyEnvironment
    .extend(Tokens.tPLUS,
    binaryPattern3(_ + _, _ + _, _ + _))
    .extend(Tokens.tMINUS,
    Fun {
      case List(arg: BigInt) =>
        -arg
      case List(arg: BigDecimal) =>
        -arg
      case List(left: BigInt, right: BigInt) =>
        left - right
      case List(left: BigDecimal, right: BigDecimal) =>
        left - right
      case List(left: BigDecimal, right: BigInt) =>
        left - BigDecimal(right)
      case List(left: BigInt, right: BigDecimal) =>
        BigDecimal(left) - right
    })
    .extend(Tokens.tMUL,
    binaryPattern2(_ * _, _ * _))
    .extend(Tokens.tDIV,
    binaryPattern2(_ / _, _ / _))
    .extend(Tokens.tREM,
    Fun {
      case List(left: BigInt, right: BigInt) =>
        left % right
    })
    .extend(Tokens.tEQ,
    binaryPattern3(_ == _, _ == _, _ == _))
    .extend(Tokens.tNEQ,
    binaryPattern3(_ != _, _ != _, _ != _))
    .extend(Tokens.tGEQ,
    binaryPattern3(_ >= _, _ >= _, _ >= _))
    .extend(Tokens.tLEQ,
    binaryPattern3(_ <= _, _ <= _, _ <= _))
    .extend(Tokens.tLE,
    binaryPattern3(_ < _, _ < _, _ < _))
    .extend(Tokens.tGE,
    binaryPattern3(_ > _, _ > _, _ > _))
    .extend(Tokens.tOR,
    Fun {
      case List(left: Boolean, right: Boolean) =>
        left || right
    })
    .extend(Tokens.tAND,
    Fun {
      case List(left: Boolean, right: Boolean) =>
        left && right
    })
    .extend(Tokens.tXOR,
    Fun {
      case List(left: Boolean, right: Boolean) =>
        left ^ right
    })
}

object Interpreter {

  def binaryPattern3(op1: (BigInt, BigInt) => Data,
                     op2: (BigDecimal, BigDecimal) => Data,
                     op3: (String, String) => Data): Fun = {
    Fun {
      case List(left: BigInt, right: BigInt) =>
        op1(left, right)
      case List(left: BigInt, right: BigDecimal) =>
        op2(BigDecimal(left), right)
      case List(left: BigDecimal, right: BigDecimal) =>
        op2(left, right)
      case List(left: BigDecimal, right: BigInt) =>
        op2(left, BigDecimal(right))
      case List(left: String, right: String) =>
        op3(left, right)
      case List(left: BigInt, right: String) =>
        op3(left.toString(), right)
      case List(left: String, right: BigInt) =>
        op3(left, right.toString())
      case List(left: BigDecimal, right: String) =>
        op3(left.toString(), right)
      case List(left: String, right: BigDecimal) =>
        op3(left, right.toString())
    }
  }

  def binaryPattern2(op1: (BigInt, BigInt) => Data,
                     op2: (BigDecimal, BigDecimal) => Data): Fun = {
    Fun {
      case List(left: BigInt, right: BigInt) =>
        op1(left, right)
      case List(left: BigInt, right: BigDecimal) =>
        op2(BigDecimal(left), right)
      case List(left: BigDecimal, right: BigDecimal) =>
        op2(left, right)
      case List(left: BigDecimal, right: BigInt) =>
        op2(left, BigDecimal(right))
    }
  }

  def unaryPattern2(op1: BigInt => Data,
                    op2: BigDecimal => Data): Fun = {
    Fun {
      case List(arg: BigInt) =>
        op1(arg)
      case List(arg: BigDecimal) =>
        op2(arg)
    }
  }

}

