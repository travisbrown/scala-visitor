package dev.travisbrown.visitors.classic

trait ExternalVisitor[A] {
  def onNull: A
  def onBoolean(value: Boolean): A
  def onNumber(value: BigDecimal): A
  def onString(value: String): A
  def onArray(value: Vector[Json]): A
  def onObject(value: Vector[(String, Json)]): A
}

trait InternalVisitor[A] {
  def onNull: A
  def onBoolean(value: Boolean): A
  def onNumber(value: BigDecimal): A
  def onString(value: String): A
  def onArray(value: Vector[A]): A
  def onObject(value: Vector[(String, A)]): A
}
