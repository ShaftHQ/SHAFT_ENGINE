package com.shaft.capture.proxy;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManagerFactory;
import java.nio.ByteBuffer;
import java.nio.file.Path;
import java.security.KeyStore;
import java.security.cert.X509Certificate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ImpersonatingMitmManagerTest {

    @TempDir
    Path tempDir;

    @Test
    void deviceThatTrustsTheCaCompletesARealTlsHandshakeAndSeesTheImpersonatedHostname() throws Exception {
        CaptureCertificateAuthority ca = new CaptureCertificateAuthority(tempDir.resolve("capture-ca"));
        ImpersonatingMitmManager mitmManager = new ImpersonatingMitmManager(ca);

        SSLEngine serverEngine = mitmManager.serverSslEngine("api.example.test", 443);
        SSLEngine clientEngine = clientEngineTrusting(ca.certificate());
        clientEngine.setUseClientMode(true);

        SSLSession clientSession = performInMemoryHandshake(clientEngine, serverEngine);

        X509Certificate presented = (X509Certificate) clientSession.getPeerCertificates()[0];
        assertEquals("CN=api.example.test", presented.getSubjectX500Principal().getName());
        assertEquals(ca.certificate().getSubjectX500Principal(), presented.getIssuerX500Principal());
    }

    @Test
    void aClientThatDoesNotTrustTheCaRejectsTheHandshake() throws Exception {
        CaptureCertificateAuthority ca = new CaptureCertificateAuthority(tempDir.resolve("capture-ca-untrusted"));
        ImpersonatingMitmManager mitmManager = new ImpersonatingMitmManager(ca);

        SSLEngine serverEngine = mitmManager.serverSslEngine("api.example.test", 443);
        SSLContext defaultContext = SSLContext.getInstance("TLS");
        defaultContext.init(null, null, null); // default (public CA) trust store -- does not trust our local CA
        SSLEngine clientEngine = defaultContext.createSSLEngine();
        clientEngine.setUseClientMode(true);

        Exception failure = org.junit.jupiter.api.Assertions.assertThrows(Exception.class,
                () -> performInMemoryHandshake(clientEngine, serverEngine));
        assertTrue(failure instanceof SSLException || failure.getCause() instanceof SSLException,
                "Expected an SSL trust failure, got: " + failure);
    }

    private static SSLEngine clientEngineTrusting(X509Certificate caCertificate) throws Exception {
        KeyStore trustStore = KeyStore.getInstance(KeyStore.getDefaultType());
        trustStore.load(null, null);
        trustStore.setCertificateEntry("capture-ca", caCertificate);
        TrustManagerFactory trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
        trustManagerFactory.init(trustStore);

        SSLContext context = SSLContext.getInstance("TLS");
        context.init(null, trustManagerFactory.getTrustManagers(), null);
        return context.createSSLEngine();
    }

    /**
     * Drives a full client/server TLS handshake between two in-memory {@link SSLEngine}s (no real
     * socket), returning the client's session once complete. A standard, if verbose, pattern for
     * unit-testing SSLEngine-based code without opening a real network connection.
     */
    private static SSLSession performInMemoryHandshake(SSLEngine client, SSLEngine server) throws Exception {
        client.beginHandshake();
        server.beginHandshake();

        ByteBuffer clientToServerPlain = ByteBuffer.allocate(0);
        ByteBuffer serverToClientPlain = ByteBuffer.allocate(0);
        ByteBuffer clientToServerCipher = ByteBuffer.allocate(32 * 1024);
        ByteBuffer serverToClientCipher = ByteBuffer.allocate(32 * 1024);

        for (int round = 0; round < 200; round++) {
            boolean progressed = false;
            progressed |= step(client, clientToServerPlain, clientToServerCipher);
            progressed |= step(server, serverToClientPlain, serverToClientCipher);

            clientToServerCipher.flip();
            server.unwrap(clientToServerCipher, ByteBuffer.allocate(32 * 1024));
            clientToServerCipher.compact();

            serverToClientCipher.flip();
            client.unwrap(serverToClientCipher, ByteBuffer.allocate(32 * 1024));
            serverToClientCipher.compact();

            if (isHandshakeDone(client) && isHandshakeDone(server)) {
                return client.getSession();
            }
            if (!progressed && client.getHandshakeStatus() == SSLEngineResult.HandshakeStatus.NEED_UNWRAP
                    && server.getHandshakeStatus() == SSLEngineResult.HandshakeStatus.NEED_UNWRAP
                    && clientToServerCipher.position() == 0 && serverToClientCipher.position() == 0) {
                break; // both sides stuck waiting for input that will never arrive -- fail fast
            }
        }
        throw new SSLException("In-memory handshake did not complete: client=" + client.getHandshakeStatus()
                + " server=" + server.getHandshakeStatus());
    }

    private static boolean step(SSLEngine engine, ByteBuffer plainOut, ByteBuffer cipherOut) throws Exception {
        SSLEngineResult.HandshakeStatus status = engine.getHandshakeStatus();
        if (status == SSLEngineResult.HandshakeStatus.NEED_TASK) {
            Runnable task;
            while ((task = engine.getDelegatedTask()) != null) {
                task.run();
            }
            return true;
        }
        if (status == SSLEngineResult.HandshakeStatus.NEED_WRAP) {
            SSLEngineResult result = engine.wrap(plainOut, cipherOut);
            return result.bytesProduced() > 0 || result.getHandshakeStatus() != status;
        }
        return false;
    }

    private static boolean isHandshakeDone(SSLEngine engine) {
        return engine.getHandshakeStatus() == SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING
                || engine.getHandshakeStatus() == SSLEngineResult.HandshakeStatus.FINISHED;
    }
}
