package dev.travisbrown.visitors.classic

import scala.util.control.TailCalls

sealed trait Json extends Product with Serializable {
  def accept[A](v: ExternalVisitor[A]): A
  def accept[A](v: InternalVisitor[A]): A
  final def acceptStackSafe[A](v: InternalVisitor[A]): A = acceptStackSafeInternal(v).result

  private[classic] def acceptStackSafeInternal[A](v: InternalVisitor[A]): TailCalls.TailRec[A]

  def fold[A](
    onNull: => A,
    onBoolean: Boolean => A,
    onNumber: BigDecimal => A,
    onString: String => A,
    onArray: Vector[Json] => A,
    onObject: Vector[(String, Json)] => A
  ): A
}

case object JsonNull extends Json {
  final def accept[A](v: ExternalVisitor[A]): A = v.onNull
  final def accept[A](v: InternalVisitor[A]): A = v.onNull
  private[classic] final def acceptStackSafeInternal[A](v: InternalVisitor[A]): TailCalls.TailRec[A] =
    TailCalls.done(v.onNull)
  final def fold[A](
    onNull: => A,
    onBoolean: Boolean => A,
    onNumber: BigDecimal => A,
    onString: String => A,
    onArray: Vector[Json] => A,
    onObject: Vector[(String, Json)] => A
  ): A = onNull
}
case class JsonBoolean(value: Boolean) extends Json {
  final def accept[A](v: ExternalVisitor[A]): A = v.onBoolean(value)
  final def accept[A](v: InternalVisitor[A]): A = v.onBoolean(value)
  private[classic] final def acceptStackSafeInternal[A](v: InternalVisitor[A]): TailCalls.TailRec[A] =
    TailCalls.done(v.onBoolean(value))
  final def fold[A](
    onNull: => A,
    onBoolean: Boolean => A,
    onNumber: BigDecimal => A,
    onString: String => A,
    onArray: Vector[Json] => A,
    onObject: Vector[(String, Json)] => A
  ): A = onBoolean(value)
}
case class JsonNumber(value: BigDecimal) extends Json {
  final def accept[A](v: ExternalVisitor[A]): A = v.onNumber(value)
  final def accept[A](v: InternalVisitor[A]): A = v.onNumber(value)
  private[classic] final def acceptStackSafeInternal[A](v: InternalVisitor[A]): TailCalls.TailRec[A] =
    TailCalls.done(v.onNumber(value))
  final def fold[A](
    onNull: => A,
    onBoolean: Boolean => A,
    onNumber: BigDecimal => A,
    onString: String => A,
    onArray: Vector[Json] => A,
    onObject: Vector[(String, Json)] => A
  ): A = onNumber(value)
}
case class JsonString(value: String) extends Json {
  final def accept[A](v: ExternalVisitor[A]): A = v.onString(value)
  final def accept[A](v: InternalVisitor[A]): A = v.onString(value)
  private[classic] final def acceptStackSafeInternal[A](v: InternalVisitor[A]): TailCalls.TailRec[A] =
    TailCalls.done(v.onString(value))
  final def fold[A](
    onNull: => A,
    onBoolean: Boolean => A,
    onNumber: BigDecimal => A,
    onString: String => A,
    onArray: Vector[Json] => A,
    onObject: Vector[(String, Json)] => A
  ): A = onString(value)
}
case class JsonArray(value: Vector[Json]) extends Json {
  final def accept[A](v: ExternalVisitor[A]): A = v.onArray(value)
  final def accept[A](v: InternalVisitor[A]): A = v.onArray(value.map(_.accept(v)))
  private[classic] final def acceptStackSafeInternal[A](v: InternalVisitor[A]): TailCalls.TailRec[A] =
    value
      .foldLeft(TailCalls.done(Vector.empty[A])) {
        case (acc, j) =>
          acc.flatMap { current =>
            TailCalls.tailcall(j.acceptStackSafeInternal(v)).map(current :+ _)
          }
      }
      .map(v.onArray)

  final def fold[A](
    onNull: => A,
    onBoolean: Boolean => A,
    onNumber: BigDecimal => A,
    onString: String => A,
    onArray: Vector[Json] => A,
    onObject: Vector[(String, Json)] => A
  ): A = onArray(value)
}
case class JsonObject(value: Vector[(String, Json)]) extends Json {
  final def accept[A](v: ExternalVisitor[A]): A = v.onObject(value)
  final def accept[A](v: InternalVisitor[A]): A = v.onObject(value.map { case (k, j) => (k, j.accept(v)) })
  private[classic] final def acceptStackSafeInternal[A](v: InternalVisitor[A]): TailCalls.TailRec[A] =
    value
      .foldLeft(TailCalls.done(Vector.empty[(String, A)])) {
        case (acc, (k, j)) =>
          acc.flatMap { current =>
            TailCalls.tailcall(j.acceptStackSafeInternal(v)).map(a => current :+ ((k, a)))
          }
      }
      .map(v.onObject)

  final def fold[A](
    onNull: => A,
    onBoolean: Boolean => A,
    onNumber: BigDecimal => A,
    onString: String => A,
    onArray: Vector[Json] => A,
    onObject: Vector[(String, Json)] => A
  ): A = onObject(value)
}
