package play.libs.ws.ning;

import com.ning.http.client.*;
import play.libs.F;
import play.libs.ws.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * Provides the bridge between Play and the underlying ning request
 */
public class NingWSRequest extends RequestBuilderBase<NingWSRequest> implements WSRequest {

    private FluentCaseInsensitiveStringsMap headers = new FluentCaseInsensitiveStringsMap();

    private String method;

    private String url;

    public NingWSRequest(String method) {
        super(NingWSRequest.class, method, false);
        this.method = method;
    }

    public Object getUnderlying() {
        return this;
    }

    protected NingWSRequest auth(String username, String password, Realm.AuthScheme scheme) {
        this.setRealm((new Realm.RealmBuilder())
                .setScheme(scheme)
                .setPrincipal(username)
                .setPassword(password)
                .setUsePreemptiveAuth(true)
                .build());
        return this;
    }

    /**
     * Set an HTTP header.
     */
    @Override
    public NingWSRequest setHeader(String name, String value) {
        headers.replace(name, value);
        return super.setHeader(name, value);
    }

    /**
     * Add an HTTP header (used for headers with mutiple values).
     */
    @Override
    public NingWSRequest addHeader(String name, String value) {
        if (value == null) {
            value = "";
        }
        headers.add(name, value);
        return super.addHeader(name, value);
    }

    /**
     * Defines the request headers.
     */
    @Override
    public NingWSRequest setHeaders(FluentCaseInsensitiveStringsMap hdrs) {
        headers = (headers == null ? new FluentCaseInsensitiveStringsMap() : headers);
        return super.setHeaders(hdrs);
    }

    /**
     * Defines the request headers.
     */
    @Override
    public NingWSRequest setHeaders(Map<String, Collection<String>> hdrs) {
        headers = (headers == null ? new FluentCaseInsensitiveStringsMap() : new FluentCaseInsensitiveStringsMap(headers));
        return super.setHeaders(hdrs);
    }

    /**
     * Return the headers of the request being constructed
     */
    public Map<String, List<String>> getAllHeaders() {
        return headers;
    }

    public List<String> getHeader(String name) {
        List<String> hdrs = headers.get(name);
        if (hdrs == null) return new ArrayList<String>();
        return hdrs;
    }

    public String getMethod() {
        return this.method;
    }

    @Override
    public NingWSRequest setUrl(String url) {
        this.url = url;
        return super.setUrl(url);
    }

    public String getUrl() {
        return this.url;
    }

    public F.Promise<play.libs.ws.WSResponse> execute() {
        final scala.concurrent.Promise<play.libs.ws.WSResponse> scalaPromise = scala.concurrent.Promise$.MODULE$.<play.libs.ws.WSResponse>apply();
        try {
            AsyncHttpClient client = (AsyncHttpClient) WS.client().getUnderlying();
            client.executeRequest(request, new AsyncCompletionHandler<com.ning.http.client.Response>() {
                @Override
                public com.ning.http.client.Response onCompleted(com.ning.http.client.Response response) {
                    final com.ning.http.client.Response ahcResponse = response;
                    scalaPromise.success(new NingWSResponse(ahcResponse));
                    return response;
                }
                @Override
                public void onThrowable(Throwable t) {
                    scalaPromise.failure(t);
                }
            });
        } catch (IOException exception) {
            scalaPromise.failure(exception);
        }
        return new F.Promise<play.libs.ws.WSResponse>(scalaPromise.future());
    }
}
