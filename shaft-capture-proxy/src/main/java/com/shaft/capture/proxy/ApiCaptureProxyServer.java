package com.shaft.capture.proxy;

import org.littleshoot.proxy.HttpFilters;
import org.littleshoot.proxy.HttpFiltersSourceAdapter;
import org.littleshoot.proxy.HttpProxyServer;
import org.littleshoot.proxy.impl.DefaultHttpProxyServer;

import java.net.InetSocketAddress;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

/**
 * Embedded, loopback-only MITM HTTP(S) proxy for recording native mobile API traffic. Every
 * request/response pair that passes through is delivered to {@code sink} as a raw
 * {@link ProxyTransaction}; safe, non-sensitive operational issues (a transaction that could not
 * be captured, a host with a pinned/unimpersonable certificate) are delivered to {@code warn}.
 *
 * <p>Binds to {@code 127.0.0.1} only -- this proxy is meant to be reached either directly (an
 * emulator on the same machine) or via {@code adb reverse}/port-forwarding from a device, never by
 * exposing it on a LAN-routable address.
 */
public final class ApiCaptureProxyServer implements AutoCloseable {
    private final HttpProxyServer server;
    private final Set<String> pinnedHosts = ConcurrentHashMap.newKeySet();

    /**
     * Starts the proxy on a loopback port.
     *
     * @param certificateAuthority CA used to impersonate HTTPS hosts
     * @param requestedPort port to bind, or {@code 0} to let the OS choose a free port
     * @param sink destination for each captured transaction
     * @param warn destination for safe operational warnings
     */
    public ApiCaptureProxyServer(
            CaptureCertificateAuthority certificateAuthority,
            int requestedPort,
            Consumer<ProxyTransaction> sink,
            Consumer<String> warn) {
        this.server = DefaultHttpProxyServer.bootstrap()
                .withAddress(new InetSocketAddress("127.0.0.1", requestedPort))
                .withAllowLocalOnly(true)
                .withManInTheMiddle(new ImpersonatingMitmManager(certificateAuthority))
                .withFiltersSource(new HttpFiltersSourceAdapter() {
                    @Override
                    public HttpFilters filterRequest(
                            io.netty.handler.codec.http.HttpRequest originalRequest,
                            io.netty.channel.ChannelHandlerContext ctx) {
                        return new CapturingHttpFilters(originalRequest, sink, warn, pinnedHosts);
                    }
                })
                .start();
    }

    /**
     * Known-pinned hostnames tunneled without MITM interception (Tier-3, see
     * {@code CapturingHttpFilters}) rather than having capture attempted and failing. Returns an
     * immutable snapshot -- use {@link #addPinnedHost(String)}/{@link #removePinnedHost(String)} to
     * mutate the underlying set rather than exposing it directly.
     *
     * @return immutable snapshot of currently pinned hostnames
     */
    public Set<String> pinnedHosts() {
        return Set.copyOf(pinnedHosts);
    }

    /**
     * Adds a hostname to the pinned set so its traffic is tunneled without MITM interception.
     *
     * @param host hostname, without port
     */
    public void addPinnedHost(String host) {
        pinnedHosts.add(host);
    }

    /**
     * Removes a hostname from the pinned set, resuming MITM interception for it.
     *
     * @param host hostname, without port
     */
    public void removePinnedHost(String host) {
        pinnedHosts.remove(host);
    }

    /**
     * Returns the loopback port this proxy is actually listening on (resolved even when
     * {@code requestedPort} was {@code 0}).
     *
     * @return bound port
     */
    public int port() {
        return server.getListenAddress().getPort();
    }

    /**
     * Stops the proxy, closing all active connections.
     */
    public void stop() {
        server.stop();
    }

    @Override
    public void close() {
        stop();
    }
}
