import io.rolandvarga.http.{Handler, Routes, RoutingNode}
import io.rolandvarga.logger.Logger
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.mockito.MockitoSugar.mock


class RountingNodeSpec extends AnyFunSpec {
  describe("RoutingNode search") {
    val mockHandler = mock[Handler]

    it("returns None if node is not in tree") {
      val input = RoutingNode(
        segment = "",
        handler = mockHandler,
        children = collection.mutable.Map[String, RoutingNode](
          "GET" -> RoutingNode(
            segment = "GET",
            handler = mockHandler,
            children = collection.mutable.Map.empty[String, RoutingNode],
          )
        ),
      )

      val expected = None

      val actual = Routes.search(input, "GET /foo/bar")
      actual shouldBe (expected)
    }

    it("finds any node at lvl 2") {
      val input = RoutingNode(
        segment = "",
        handler = mockHandler,
        children = collection.mutable.Map[String, RoutingNode](
          "GET" -> RoutingNode(
            segment = "GET",
            handler = mockHandler,
            children = collection.mutable.Map.empty[String, RoutingNode],
          )
        ),
      )

      input.children("GET").children("foo") = RoutingNode(
        segment = "foo",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      )

      val expected = Some(RoutingNode(
        segment = "foo",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      ))

      val actual = Routes.search(input, "GET /foo")
      actual shouldBe (expected)
    }

    it("finds any node at lvl 3") {
      val input = RoutingNode(
        segment = "",
        handler = mockHandler,
        children = collection.mutable.Map[String, RoutingNode](
          "GET" -> RoutingNode(
            segment = "GET",
            handler = mockHandler,
            children = collection.mutable.Map.empty[String, RoutingNode],
          )
        ),
      )

      input.children("GET").children("foo") = RoutingNode(
        segment = "foo",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      )

      input.children("GET").children("foo").children("bar") = RoutingNode(
        segment = "bar",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      )

      val expected = Some(RoutingNode(
        segment = "bar",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      ))

      val actual = Routes.search(input, "GET /foo/bar")
      actual shouldBe (expected)
    }

    it("it's smart enough in finding nodes which have children, and doesn't recursively just return the child") {
      val input = RoutingNode(
        segment = "",
        handler = mockHandler,
        children = collection.mutable.Map[String, RoutingNode](
          "GET" -> RoutingNode(
            segment = "GET",
            handler = mockHandler,
            children = collection.mutable.Map.empty[String, RoutingNode],
          )
        ),
      )

      input.children("GET").children("foo") = RoutingNode(
        segment = "foo",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      )

      input.children("GET").children("foo").children("bar") = RoutingNode(
        segment = "bar",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      )

      val expected = Some(RoutingNode(
        segment = "foo",
        handler = mockHandler,
        children = collection.mutable.Map("bar" -> RoutingNode(
          segment = "bar",
          handler = mockHandler,
          children = collection.mutable.Map.empty[String, RoutingNode],
        ))
      ))

      val actual = Routes.search(input, "GET /foo")
      actual shouldBe (expected)
    }
  }

  describe("RoutingNode insert") {
    val mockHandler = mock[Handler]

    it("inserts nodes at lvl 2") {
      val input = RoutingNode(
        segment = "",
        handler = mockHandler,
        children = collection.mutable.Map[String, RoutingNode](
          "GET" -> RoutingNode(
            segment = "GET",
            handler = mockHandler,
            children = collection.mutable.Map.empty[String, RoutingNode],
          )
        ),
      )

      val expected = RoutingNode(
        segment = "",
        handler = mockHandler,
        children = collection.mutable.Map[String, RoutingNode](
          "GET" -> RoutingNode(
            segment = "GET",
            handler = mockHandler,
            children = collection.mutable.Map.empty[String, RoutingNode],
          )
        ),
      )

      expected.children("GET").children("foo") = RoutingNode(
        segment = "foo",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      )

      expected.children("GET").children("foo").children("bar") = RoutingNode(
        segment = "bar",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      )

      val actual = Routes.insert(input, "GET /foo/bar", mockHandler)
      actual shouldBe(Some(expected))
    }

    it("inserts nodes at lvl 3") {
      val input = RoutingNode(
        segment = "",
        handler = mockHandler,
        children = collection.mutable.Map[String, RoutingNode](
          "GET" -> RoutingNode(
            segment = "GET",
            handler = mockHandler,
            children = collection.mutable.Map.empty[String, RoutingNode],
          )
        ),
      )

      val expected = RoutingNode(
        segment = "",
        handler = mockHandler,
        children = collection.mutable.Map[String, RoutingNode](
          "GET" -> RoutingNode(
            segment = "GET",
            handler = mockHandler,
            children = collection.mutable.Map.empty[String, RoutingNode],
          )
        ),
      )

      expected.children("GET").children("foo") = RoutingNode(
        segment = "foo",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      )

      expected.children("GET").children("foo").children("bar") = RoutingNode(
        segment = "bar",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      )

      expected.children("GET").children("foo").children("bar").children("baz") = RoutingNode(
        segment = "baz",
        handler = mockHandler,
        children = collection.mutable.Map.empty[String, RoutingNode],
      )

      Routes.insert(input, "GET /foo/bar/baz", mockHandler)
      val actual = Some(input)
      actual shouldBe(Some(expected))
    }

    it("handles invalid input") {
      val input = RoutingNode(
        segment = "",
        handler = mockHandler,
        children = collection.mutable.Map[String, RoutingNode](
          "GET" -> RoutingNode(
            segment = "GET",
            handler = mockHandler,
            children = collection.mutable.Map.empty[String, RoutingNode],
          )
        ),
      )

      val expected = None
      val actual = Routes.insert(input, "some incohenerent input that we can't use", mockHandler)
      actual shouldBe(expected)
    }
  }
}
