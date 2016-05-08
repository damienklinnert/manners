package io.manners

import com.google.gson.annotations.SerializedName


data class Request(
        val method: String,
        val path: String,
        val query: Map<String, List<String>>?,
        val headers: Map<String, String>?,
        val body: String?
)

data class Response(
        val status: Int,
        val headers: Map<String, String>?,
        val body: String?
)

data class Interaction(
        val description: String,
        @SerializedName("provider_state") val providerState: String?,
        val request: Request,
        val response: Response
)

internal data class InteractionWrapper(
        val interactions: List<Interaction>
)

internal data class ServiceDescription(val name : String)

internal data class ContractHeader(
        val consumer: ServiceDescription,
        val provider: ServiceDescription
)
