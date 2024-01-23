object Iterations {

  def whileChanging[T](f: T => T, initialValues: T): T = {
    val newValues = f(initialValues)
    if (newValues != initialValues) {
      whileChanging(f, newValues)
    } else {
      newValues
    }
  }

}
