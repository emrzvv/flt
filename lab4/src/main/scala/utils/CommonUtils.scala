package utils

object CommonUtils {
  val Eps = "&"

  implicit class StringOps(value: String) {
    def isEps: Boolean = value == "&"
  }

}
