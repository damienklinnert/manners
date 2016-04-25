package io.manners

import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull

class ProviderSpecificationTest {

    @Test fun testResultContainsDefaultInteractions() {
        val spec = specification()
        val result = spec.result()
        assertEquals(listOf<Interaction>(), result)
    }

    @Test fun testResultContainsInteractions() {
        val spec = specification().interaction(
                "some example",
                onRequest("GET", "/test/a"),
                respondWith(202)
        )
        val result = spec.result()
        assertEquals(listOf(
                Interaction(
                        "some example",
                        null,
                        Request("GET", "/test/a", null, null, null),
                        Response(202, null, null)
                )
        ), result)
    }

    @Test fun testResultContainsStatefulInteractions() {
        val spec = specification().statefulInteraction(
                "some example",
                state("test state"),
                onRequest("GET", "/test/a"),
                respondWith(202)
        )
        val result = spec.result()
        assertEquals(listOf(
                Interaction(
                        "some example",
                        "test state",
                        Request("GET", "/test/a", null, null, null),
                        Response(202, null, null)
                )
        ), result)
    }

}

class StateSpecificationTest {

    @Test fun testResultContainsDescription() {
        val spec = state("some description")
        assertEquals(spec.result(), "some description")
    }
}

class RequestSpecificationTest {

    @Test fun testResultContainsDefaultRequest() {
        val spec = onRequest("GET", "/test")
        val result = spec.result()
        assertEquals("GET", result.method)
        assertEquals("/test", result.path)
        assertNull(result.query)
        assertNull(result.headers)
        assertNull(result.body)
    }

    @Test fun testResultContainsRequestWithQuery() {
        val spec = onRequest("GET", "/test").queryParam("a", "1").queryParam("a", "2").queryParam("b", "1")
        val result = spec.result()
        assertEquals("GET", result.method)
        assertEquals("/test", result.path)
        assertEquals(result.query, mapOf("a" to listOf("1", "2"), "b" to listOf("1")))
        assertNull(result.headers)
        assertNull(result.body)
    }

    @Test fun testResultContainsRequestWithHeaders() {
        val spec = onRequest("GET", "/test").header("x-a", "a").header("x-b", "b")
        val result = spec.result()
        assertEquals("GET", result.method)
        assertEquals("/test", result.path)
        assertNull(result.query)
        assertEquals(mapOf("x-a" to "a", "x-b" to "b"), result.headers)
        assertNull(result.body)
    }

    @Test fun testResultContainsRequestWithBody() {
        val spec = onRequest("GET", "/test").body("[1,2,3]")
        val result = spec.result()
        assertEquals("GET", result.method)
        assertEquals("/test", result.path)
        assertNull(result.query)
        assertNull(result.headers)
        assertEquals("[1,2,3]", result.body)
    }

}

class ResponseSpecificationTest {

    @Test fun testResultContainsDefaultResponse() {
        val spec = respondWith(200)
        val result = spec.result()
        assertEquals(200, result.status)
        assertNull(result.headers)
        assertNull(result.body)
    }

    @Test fun testResultContainsResponseWithHeaders() {
        val spec = respondWith(200).header("x-a", "a").header("x-b", "b")
        val result = spec.result()
        assertEquals(200, result.status)
        assertEquals(mapOf("x-a" to "a", "x-b" to "b"), result.headers)
        assertNull(result.body)
    }

    @Test fun testResultContainsResponseWithBody() {
        val spec = respondWith(200).body("[1,2,3]")
        val result = spec.result()
        assertEquals(200, result.status)
        assertNull(result.headers)
        assertEquals("[1,2,3]", result.body)
    }

}