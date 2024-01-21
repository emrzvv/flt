package utils

object CommonUtils {
  val Eps = "&"
  val EOL = "$" // end of line

  implicit class StringOps(value: String) {
    def isEps: Boolean = value == "&"
  }


}
