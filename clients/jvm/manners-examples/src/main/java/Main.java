import okhttp3.OkHttpClient;

public class Main {

    public static void performRequest() throws Exception {


        final OkHttpClient client = new OkHttpClient();
        okhttp3.Request.Builder builder = new okhttp3.Request.Builder();
        okhttp3.Request realReq = builder.get().url("http://localhost:1234/test?a=23").build();
        okhttp3.Response realRes = client.newCall(realReq).execute();

        if (realRes.code() != 202) {
            throw new Error("the fake service behaved completely different");
        }
    }
}
