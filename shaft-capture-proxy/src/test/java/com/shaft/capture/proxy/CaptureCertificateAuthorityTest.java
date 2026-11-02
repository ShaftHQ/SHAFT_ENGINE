package com.shaft.capture.proxy;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.time.Instant;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureCertificateAuthorityTest {

    @TempDir
    Path tempDir;

    @Test
    void generatesASelfSignedCaCertificateOnFirstUse() throws Exception {
        CaptureCertificateAuthority ca = new CaptureCertificateAuthority(tempDir.resolve("capture-ca"));

        X509Certificate certificate = ca.certificate();
        assertEquals(certificate.getSubjectX500Principal(), certificate.getIssuerX500Principal(),
                "Root CA must be self-signed (issuer == subject)");
        assertTrue(certificate.getSubjectX500Principal().getName().contains("SHAFT Capture Local CA"));
        assertTrue(certificate.getBasicConstraints() >= 0, "Certificate must be a CA (basic constraints present)");

        // A self-signed cert's signature must verify against its OWN public key.
        assertDoesNotThrow(() -> certificate.verify(certificate.getPublicKey()));

        Instant now = Instant.now();
        assertTrue(certificate.getNotBefore().toInstant().isBefore(now));
        assertTrue(certificate.getNotAfter().toInstant().isAfter(now.plusSeconds(3600L * 24 * 365 * 5)),
                "Root CA should be valid for years, not a short-lived leaf cert");
    }

    @Test
    void reusesTheSameCaAcrossRestartsInsteadOfRegenerating() {
        Path directory = tempDir.resolve("capture-ca-reuse");
        CaptureCertificateAuthority first = new CaptureCertificateAuthority(directory);
        CaptureCertificateAuthority second = new CaptureCertificateAuthority(directory);

        assertEquals(first.certificate(), second.certificate(),
                "A second CA instance over the same directory must load the persisted CA, not regenerate one");
        assertEquals(first.keyPair().getPrivate(), second.keyPair().getPrivate());
    }

    @Test
    void privateKeyFileIsRestrictedToOwnerOnly() throws Exception {
        Path directory = tempDir.resolve("capture-ca-perms");
        new CaptureCertificateAuthority(directory);

        Path keyFile = Files.list(directory).filter(p -> p.getFileName().toString().contains("key")).findFirst()
                .orElseThrow(() -> new AssertionError("No CA key file found in " + directory));

        try {
            Set<PosixFilePermission> permissions = Files.getPosixFilePermissions(keyFile);
            assertEquals(Set.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE), permissions);
        } catch (UnsupportedOperationException posixUnsupported) {
            // Windows/ACL fallback is covered by OwnerOnlyFilePermissionsTest directly; here we
            // only need to confirm restrictToOwner was actually invoked on this exact file, which
            // the POSIX branch above already proves on POSIX filesystems.
        }
    }

    @Test
    void exportedCertificatePemIsValidAndParsesBackToTheSameCertificate() throws Exception {
        CaptureCertificateAuthority ca = new CaptureCertificateAuthority(tempDir.resolve("capture-ca-export"));

        String pem = ca.exportCertificatePem();
        assertTrue(pem.contains("BEGIN CERTIFICATE"));
        assertTrue(pem.contains("END CERTIFICATE"));

        CertificateFactory certificateFactory = CertificateFactory.getInstance("X.509");
        X509Certificate parsed = (X509Certificate) certificateFactory.generateCertificate(
                new ByteArrayInputStream(pem.getBytes(java.nio.charset.StandardCharsets.UTF_8)));

        assertEquals(ca.certificate(), parsed);
    }

    @Test
    void exportedPemNeverContainsPrivateKeyMaterial() {
        CaptureCertificateAuthority ca = new CaptureCertificateAuthority(tempDir.resolve("capture-ca-no-leak"));

        String pem = ca.exportCertificatePem();

        assertTrue(!pem.contains("PRIVATE KEY"), "Exported certificate PEM must never include the private key");
    }

    @Test
    void issuesALeafCertificateSignedByTheCaForAGivenHostname() throws Exception {
        CaptureCertificateAuthority ca = new CaptureCertificateAuthority(tempDir.resolve("capture-ca-leaf"));
        java.security.KeyPairGenerator keyPairGenerator = java.security.KeyPairGenerator.getInstance("RSA");
        keyPairGenerator.initialize(2048);
        java.security.KeyPair leafKeyPair = keyPairGenerator.generateKeyPair();

        X509Certificate leaf = ca.issueLeafCertificate("api.example.test", leafKeyPair.getPublic());

        assertEquals("CN=api.example.test", leaf.getSubjectX500Principal().getName());
        assertEquals(ca.certificate().getSubjectX500Principal(), leaf.getIssuerX500Principal());
        assertDoesNotThrow(() -> leaf.verify(ca.certificate().getPublicKey()),
                "Leaf certificate must be signed by (and verify against) the CA's public key");
        assertEquals(-1, leaf.getBasicConstraints(), "Leaf certificate must not itself be a CA");

        boolean hasMatchingSan = leaf.getSubjectAlternativeNames().stream()
                .anyMatch(san -> "api.example.test".equals(san.get(1)));
        assertTrue(hasMatchingSan, "Leaf certificate must carry the hostname as a DNS SAN");

        Instant now = Instant.now();
        assertTrue(leaf.getNotAfter().toInstant().isBefore(now.plusSeconds(3600L * 24 * 900)),
                "Leaf certificates must be short-lived, not reuse the root CA's 10-year validity");
    }

    @Test
    void differentHostnamesGetDifferentLeafCertificates() throws Exception {
        CaptureCertificateAuthority ca = new CaptureCertificateAuthority(tempDir.resolve("capture-ca-multi-host"));
        java.security.KeyPairGenerator keyPairGenerator = java.security.KeyPairGenerator.getInstance("RSA");
        keyPairGenerator.initialize(2048);
        java.security.PublicKey leafPublicKey = keyPairGenerator.generateKeyPair().getPublic();

        X509Certificate first = ca.issueLeafCertificate("a.example.test", leafPublicKey);
        X509Certificate second = ca.issueLeafCertificate("b.example.test", leafPublicKey);

        assertTrue(!first.getSubjectX500Principal().equals(second.getSubjectX500Principal()));
    }
}
