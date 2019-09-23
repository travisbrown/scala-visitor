package dev.travisbrown.visitors

import dev.travisbrown.visitors.classic._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

/**
 * Compare the performance of various ways of folding JSON values.
 *
 * The following command will run the benchmarks with reasonable settings:
 *
 * > sbt "benchmark/jmh:run -i 10 -wi 10 -f 2 -t 1 dev.travisbrown.visitors.SizeBench"
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class SizeBench {
  val doc: Json = JsonObject(
    Vector(
      "numbers" -> JsonArray((0 to 100).map(i => JsonNumber(BigDecimal(i))).toVector),
      "booleans" -> JsonArray((0 to 100).map(i => JsonBoolean(i % 2 == 0)).toVector),
      "deep" -> (0 to 100).foldLeft(JsonObject(Vector.empty)) {
        case (acc, i) => acc.copy(value = Vector("d" -> JsonNumber(BigDecimal(i)), "v" -> acc))
      }
    )
  )

  @Benchmark
  def sizeWithExternalVisitor: Int = doc.accept(
    new ExternalVisitor[Int] {
      def onNull: Int = 0
      def onBoolean(value: Boolean): Int = if (value) 1 else 0
      def onNumber(value: BigDecimal): Int = value.toInt
      def onString(value: String): Int = value.length
      def onArray(value: Vector[Json]): Int = value.foldLeft(0) {
        case (acc, v) => acc + v.accept(this)
      }
      def onObject(value: Vector[(String, Json)]): Int = value.foldLeft(0) {
        case (acc, (_, v)) => acc + v.accept(this)
      }
    }
  )

  @Benchmark
  def sizeWithInternalVisitor: Int = doc.accept(
    new InternalVisitor[Int] {
      def onNull: Int = 0
      def onBoolean(value: Boolean): Int = if (value) 1 else 0
      def onNumber(value: BigDecimal): Int = value.toInt
      def onString(value: String): Int = value.length
      def onArray(value: Vector[Int]): Int = value.sum
      def onObject(value: Vector[(String, Int)]): Int = value.foldLeft(0) {
        case (acc, (_, v)) => acc + v
      }
    }
  )

  @Benchmark
  def sizeWithStackSafeInternalVisitor: Int = doc.acceptStackSafe(
    new InternalVisitor[Int] {
      def onNull: Int = 0
      def onBoolean(value: Boolean): Int = if (value) 1 else 0
      def onNumber(value: BigDecimal): Int = value.toInt
      def onString(value: String): Int = value.length
      def onArray(value: Vector[Int]): Int = value.sum
      def onObject(value: Vector[(String, Int)]): Int = value.foldLeft(0) {
        case (acc, (_, v)) => acc + v
      }
    }
  )

  @Benchmark
  def sizeWithFold: Int = {
    def size(json: Json): Int = json.fold(
      0,
      if (_) 1 else 0,
      _.toInt,
      _.length,
      _.foldLeft(0) {
        case (acc, v) => acc + size(v)
      },
      _.foldLeft(0) {
        case (acc, (_, v)) => acc + size(v)
      }
    )

    size(doc)
  }

  @Benchmark
  def sizeWithPatternMatch: Int = {
    def size(json: Json): Int = json match {
      case JsonNull           => 0
      case JsonBoolean(value) => if (value) 1 else 0
      case JsonNumber(value)  => value.toInt
      case JsonString(value)  => value.length
      case JsonArray(value) =>
        value.foldLeft(0) {
          case (acc, v) => acc + size(v)
        }
      case JsonObject(value) =>
        value.foldLeft(0) {
          case (acc, (_, v)) => acc + size(v)
        }
    }

    size(doc)
  }
}
