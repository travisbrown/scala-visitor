package dev.travisbrown.visitors.classic

import minitest._

object JsonSuite extends SimpleTestSuite {
	test("accept with an internal visitor should not overflow the stack") {
		val doc = (0 to 10000).foldLeft(JsonObject(Vector.empty)) {
			case (acc, i) => acc.copy(value = Vector("d" -> JsonNumber(BigDecimal(i)), "v" -> acc))
		}

		doc.acceptStackSafe(
			new InternalVisitor[Unit] {
				def onNull: Unit = ()
        def onBoolean(value: Boolean): Unit = ()
        def onNumber(value: BigDecimal): Unit = ()
        def onString(value: String): Unit = ()
        def onArray(value: Vector[Unit]): Unit = ()
        def onObject(value: Vector[(String, Unit)]): Unit = ()
			}
		)
	}
}
