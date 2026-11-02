package com.shaft.capture.proxy;

import io.netty.handler.codec.http.HttpRequest;
import org.littleshoot.proxy.MitmManager;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLSession;
import java.security.GeneralSecurityException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.KeyStore;
import java.security.SecureRandom;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.concurrent.ConcurrentHashMap;

/**
 * {@link MitmManager} that impersonates whatever host the mobile client is connecting to, using a
 * fresh leaf certificate signed by a {@link CaptureCertificateAuthority} for every distinct
 * hostname, so the proxy can terminate and inspect TLS traffic once the device trusts that CA.
 *
 * <p>One RSA leaf key pair is generated per {@code ImpersonatingMitmManager} instance and reused
 * across every host it impersonates (only the certificate differs per host) -- generating a fresh
 * RSA key pair per TLS handshake would be prohibitively slow, and every mainstream MITM-capable
 * proxy (mitmproxy, Charles, LittleProxy's own {@code littleproxy-mitm} addon) uses this same
 * shared-leaf-key, per-host-certificate approach.
 *
 * <p>The outbound (proxy -&gt; real server) connection always uses the JVM's default trust
 * manager, so this proxy never silently downgrades validation of the real upstream server's
 * identity -- only the client-facing (device -&gt; proxy) side is impersonated.
 */
public final class ImpersonatingMitmManager implements MitmManager {
    private final CaptureCertificateAuthority certificateAuthority;
    private final KeyPair leafKeyPair;
    private final char[] inMemoryKeystorePassword;
    private final ConcurrentHashMap<String, SSLContext> serverContextsByHost = new ConcurrentHashMap<>();

    /**
     * Creates a MITM manager backed by the given certificate authority.
     *
     * @param certificateAuthority CA used to sign every per-host leaf certificate
     */
    public ImpersonatingMitmManager(CaptureCertificateAuthority certificateAuthority) {
        this.certificateAuthority = certificateAuthority;
        this.leafKeyPair = generateLeafKeyPair();
        this.inMemoryKeystorePassword = randomPassword();
    }

    @Override
    public SSLEngine serverSslEngine(String peerHost, int peerPort) {
        SSLEngine engine = serverContextFor(peerHost).createSSLEngine();
        engine.setUseClientMode(false);
        return engine;
    }

    @Override
    public SSLEngine serverSslEngine() {
        // Called before SNI is known (e.g. the initial CONNECT handshake); "localhost" is a
        // harmless placeholder subject since LittleProxy always re-invokes serverSslEngine(host,
        // port) once the real target host is known from the CONNECT request.
        SSLEngine engine = serverContextFor("localhost").createSSLEngine();
        engine.setUseClientMode(false);
        return engine;
    }

    @Override
    public SSLEngine clientSslEngineFor(HttpRequest httpRequest, SSLSession sslSession) {
        try {
            SSLContext context = SSLContext.getInstance("TLS");
            // Passing null trust managers makes the JVM install its own default trust manager,
            // which validates the real upstream server's certificate chain normally -- this proxy
            // impersonates hosts to the DEVICE, not to the real server it forwards to.
            context.init(null, null, new SecureRandom());
            SSLEngine engine = context.createSSLEngine();
            engine.setUseClientMode(true);
            return engine;
        } catch (GeneralSecurityException failure) {
            throw new CaptureProxyException("Outbound TLS engine could not be created.", failure);
        }
    }

    private SSLContext serverContextFor(String host) {
        return serverContextsByHost.computeIfAbsent(host, this::buildServerContext);
    }

    private SSLContext buildServerContext(String host) {
        try {
            X509Certificate leafCertificate = certificateAuthority.issueLeafCertificate(host, leafKeyPair.getPublic());
            KeyStore keyStore = KeyStore.getInstance("PKCS12");
            keyStore.load(null, null);
            keyStore.setKeyEntry("capture-leaf", leafKeyPair.getPrivate(), inMemoryKeystorePassword,
                    new Certificate[] {leafCertificate, certificateAuthority.certificate()});

            KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
            keyManagerFactory.init(keyStore, inMemoryKeystorePassword);

            SSLContext context = SSLContext.getInstance("TLS");
            context.init(keyManagerFactory.getKeyManagers(), null, new SecureRandom());
            return context;
        } catch (GeneralSecurityException | java.io.IOException failure) {
            throw new CaptureProxyException("Impersonation certificate for \"" + host + "\" could not be prepared.", failure);
        }
    }

    private static KeyPair generateLeafKeyPair() {
        try {
            KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA");
            generator.initialize(2048, new SecureRandom());
            return generator.generateKeyPair();
        } catch (GeneralSecurityException failure) {
            throw new CaptureProxyException("MITM leaf key pair could not be generated.", failure);
        }
    }

    private static char[] randomPassword() {
        // In-memory-only keystore; never persisted, so the password only needs to be
        // unpredictable within this process, not memorable or externally shared.
        byte[] randomBytes = new byte[24];
        new SecureRandom().nextBytes(randomBytes);
        return java.util.Base64.getEncoder().encodeToString(randomBytes).toCharArray();
    }
}
