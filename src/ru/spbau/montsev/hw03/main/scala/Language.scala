package ru.spbau.montsev.hw03.main.scala

object Tokens {
  // operations
  val tASSIGN = "="
  val tPLUS = "+"
  val tMUL = "*"
  val tDIV = "/"
  val tREM = "%"
  val tMINUS = "-"
  val tEQ = "=="
  val tNEQ = "!="
  val tLEQ = "<="
  val tGEQ = ">="
  val tLE = "<"
  val tGE = ">"
  val tOR = "||"
  val tAND = "&&"
  val tXOR = "^"
  val tINVOKE = "."


  // separators
  val tBLOCK_BEGIN = "{"
  val tBLOCK_END = "}"
  val tCOMMA = ","
  val tPAR_BEGIN = "("
  val tPAR_END = ")"
  val tSTRUCT_BEGIN = "["
  val tSTRUCT_END = "]"
  val tSTMT = ";"
  val tGEN = "<-"
  val tARGS_END = "=>"
  val tRANGE_SEP = "to"
  val tCOMMENT = "//"

  // keywords
  val tIF = "if"
  val tELSE = "else"
  val tWHILE = "while"
  val tFOR = "for"
  val tPRINT = "print"
  val tPRINTLN = "println"
  val tTRUE = "true"
  val tFALSE = "false"

  // representation
  val rSTRUCT = "Struct"
  val rLAMBDA = "Lambda"
  val rUNDEFINED = "Null"

  val KEYWORDS = List(tPRINT, tPRINTLN, tTRUE, tFALSE)
}

object Language {
  type Data = Any
  type Name = String
}
