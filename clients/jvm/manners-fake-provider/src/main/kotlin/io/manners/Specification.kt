@file:JvmName("DSL")


package io.manners


fun specification(): ProviderSpecification {
    return ProviderSpecification(listOf())
}


fun state(description: String): StateSpecification {
    return StateSpecification(description)
}


fun onRequest(method: String, path: String): RequestSpecification {
    return RequestSpecification(method, path, null, null, null)
}


fun respondWith(status: Int): ResponseSpecification {
    return ResponseSpecification(status, null, null)
}


class ProviderSpecification internal constructor(
        private val interactions: List<Interaction>
) {
    fun interaction(title: String, request: RequestSpecification, response: ResponseSpecification): ProviderSpecification {
        val newInteraction = Interaction(title, null, request.result(), response.result())
        return ProviderSpecification(interactions.plus(newInteraction))
    }

    fun statefulInteraction(title: String, state: StateSpecification, request: RequestSpecification, response: ResponseSpecification): ProviderSpecification {
        val newInteraction = Interaction(title, state.result(), request.result(), response.result())
        return ProviderSpecification(interactions.plus(newInteraction))
    }

    internal fun result(): List<Interaction> {
        return interactions
    }
}


class StateSpecification internal constructor(
        private val description: String
) {

    internal fun result(): String {
        return description
    }
}


class RequestSpecification internal constructor(
        private val method: String,
        private val path: String,
        private val query: Map<String, List<String>>?,
        private val headers: Map<String, String>?,
        private val body: String?
) {
    fun body(body: String): RequestSpecification {
        return RequestSpecification(method, path, query, null, body)
    }

    fun header(name: String, value: String): RequestSpecification {
        val newHeaders = (headers ?: mapOf()).plus(name to value)
        return RequestSpecification(method, path, query, newHeaders, body)
    }

    fun queryParam(name: String, value: String): RequestSpecification {
        val newVals = ((query ?: mapOf())[name] ?: listOf()).plus(value)
        val newQuery = (query ?: mapOf()).plus(name to newVals)
        return RequestSpecification(method, path, newQuery, headers, body)
    }

    internal fun result(): Request {
        return Request(method, path, query, headers, body)
    }
}


class ResponseSpecification internal constructor(
        private val status: Int,
        private val headers: Map<String, String>?,
        private val body: String?
) {
    fun body(body: String): ResponseSpecification {
        return ResponseSpecification(status, headers, body)
    }

    fun header(name: String, value: String): ResponseSpecification {
        val newHeaders = (headers ?: mapOf()).plus(name to value)
        return ResponseSpecification(status, newHeaders, body)
    }

    internal fun result(): Response {
        return Response(status, headers, body)
    }
}
