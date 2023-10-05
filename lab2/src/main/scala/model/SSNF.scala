package model

sealed trait SSNF

case class SSNFSymbol(value: Char) extends SSNF
case class SSNFOr(left: SSNF, right: SSNF) extends SSNF
case class SSNFConcat(left: SSNF, right: SSNF) extends SSNF
case class SSNFRepeat(term: SSNF) extends SSNF

object SSNF {
  def apply(term: Term): SSNF = {
    term match {
      case Symbol(value) => SSNFSymbol(value = value)
      case Or(left, right) => SSNFOr(SSNF(left), SSNF(right))
      case Concat(left, right) => SSNFConcat(SSNF(left), SSNF(right))
      case Repeat(term) => SSNFRepeat(applySS(term))
    }
  }

  def applySS(term: Term): SSNF = {
    term match {
      case Symbol(value) => SSNFSymbol(value = value)
      case Or(left, right) => SSNFOr(applySS(left), applySS(right))
      case Concat(left, right) => SSNFConcat(SSNF(left), SSNF(right))
      case Repeat(term) => applySS(term)
    }
  }
}
