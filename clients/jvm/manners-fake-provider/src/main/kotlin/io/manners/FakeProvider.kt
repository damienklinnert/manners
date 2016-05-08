package io.manners

import com.google.gson.Gson
import okhttp3.HttpUrl
import okhttp3.MediaType
import java.util.*

class FakeProvider(val consumerName: String, val providereName: String) {

    private val client = okhttp3.OkHttpClient()
    private val gson = Gson()
    private var process: Process? = null

    fun start() {
        process = Runtime.getRuntime().exec(arrayOf("../manners-bin/src/main/resources/manners", "fake-provider"))

        val scanner = Scanner(process?.inputStream)

        do {
            val line = scanner.nextLine()
            println(line)
        } while (!line.contains("listening"))
    }

    fun stop() {
        process?.destroy()
        process?.waitFor()
    }

    fun setup(spec: ProviderSpecification) {

        val payload = gson.toJson(InteractionWrapper(spec.result()))
        println(payload)
        val body = okhttp3.RequestBody.create(MediaType.parse("application/json"), payload)
        val req = okhttp3.Request.Builder()
                .put(body)
                .url(HttpUrl.parse("http://0.0.0.0:1234/interactions"))
                .header("X-Pact-Mock-Service", "True")
                .build()

        val res = client.newCall(req).execute()
        if (res.code() != 200) {
            throw Exception("could not setup interactions: " + res.body().string())
        }
    }

    fun verify() {
        val verifyReq = okhttp3.Request.Builder()
                .get()
                .url(HttpUrl.parse("http://0.0.0.0:1234/interactions/verification"))
                .header("X-Pact-Mock-Service", "True")
                .build()
        val verifyRes = client.newCall(verifyReq).execute()
        if (verifyRes.code() != 200) {
            throw Exception("Interaction verification failed: " + verifyRes.body().string())
        }

        val cleanReq = okhttp3.Request.Builder()
                .delete()
                .url(HttpUrl.parse("http://0.0.0.0:1234/interactions"))
                .header("X-Pact-Mock-Service", "True")
                .build()
        val cleanRes = client.newCall(cleanReq).execute()
        if (cleanRes.code() != 200) {
            throw Exception("Couldn't clean up interactions: " + cleanRes.body().string())
        }

        val payload = gson.toJson(ContractHeader(ServiceDescription(consumerName), ServiceDescription(providereName)))
        val body = okhttp3.RequestBody.create(MediaType.parse("application/json"), payload)
        val contractReq = okhttp3.Request.Builder()
                .post(body)
                .url(HttpUrl.parse("http://0.0.0.0:1234/pact"))
                .header("X-Pact-Mock-Service", "True")
                .build()
        val contractRes = client.newCall(contractReq).execute()
        if (contractRes.code() != 200) {
            throw Exception("Couldn't persist contract: " + cleanRes.body().string())
        }
    }
}