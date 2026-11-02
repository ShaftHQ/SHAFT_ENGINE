package com.shaft.capture.proxy;

import com.shaft.capture.storage.OwnerOnlyFilePermissions;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.BasicConstraints;
import org.bouncycastle.asn1.x509.Extension;
import org.bouncycastle.asn1.x509.KeyUsage;
import org.bouncycastle.cert.X509CertificateHolder;
import org.bouncycastle.cert.X509v3CertificateBuilder;
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter;
import org.bouncycastle.cert.jcajce.JcaX509v3CertificateBuilder;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.openssl.jcajce.JcaPEMWriter;
import org.bouncycastle.operator.ContentSigner;
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder;
import org.bouncycastle.util.io.pem.PemObject;
import org.bouncycastle.util.io.pem.PemReader;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.GeneralSecurityException;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.SecureRandom;
import java.security.Security;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.security.spec.PKCS8EncodedKeySpec;
import java.time.Duration;
import java.time.Instant;
import java.util.Date;

/**
 * Per-installation self-signed root certificate authority used to impersonate HTTPS hosts for
 * mobile API capture. Generated once per machine under {@code ~/.shaft/capture-ca/} and reused on
 * every subsequent capture session; the private key is written with owner-only permissions and is
 * never logged, transmitted, or included in any capture artifact -- only the public certificate is
 * ever exported (for installation as a trusted CA on the device being captured).
 */
public final class CaptureCertificateAuthority {
    private static final String SUBJECT = "CN=SHAFT Capture Local CA, O=SHAFT, OU=capture-proxy";
    private static final Duration VALIDITY = Duration.ofDays(3650);
    private static final String KEY_FILE = "ca-key.pk8.pem";
    private static final String CERT_FILE = "ca-cert.pem";

    static {
        if (Security.getProvider(BouncyCastleProvider.PROVIDER_NAME) == null) {
            Security.addProvider(new BouncyCastleProvider());
        }
    }

    private final Path directory;
    private final KeyPair keyPair;
    private final X509Certificate certificate;

    /**
     * Loads the per-installation CA from {@code ~/.shaft/capture-ca/}, generating and persisting
     * a new one if none exists yet.
     *
     * @throws CaptureProxyException if the CA could not be loaded or generated
     */
    public CaptureCertificateAuthority() {
        this(Path.of(System.getProperty("user.home"), ".shaft", "capture-ca"));
    }

    /**
     * Loads (or generates) the CA under an explicit directory. Production code should prefer the
     * no-arg constructor's shared per-machine {@code ~/.shaft/capture-ca} directory; this overload
     * exists for tests and tools that must not touch that real, persistent, shared location and
     * need an isolated (for example {@code @TempDir}-backed) directory instead.
     *
     * @param directory directory the CA key/cert are stored in
     */
    public CaptureCertificateAuthority(Path directory) {
        this.directory = directory;
        try {
            Files.createDirectories(directory);
            OwnerOnlyFilePermissions.restrictDirectoryToOwner(directory);
            Path keyPath = directory.resolve(KEY_FILE);
            Path certPath = directory.resolve(CERT_FILE);
            if (Files.exists(keyPath) && Files.exists(certPath)) {
                X509Certificate loadedCertificate = readCertificate(certPath);
                this.certificate = loadedCertificate;
                this.keyPair = readKeyPair(keyPath, loadedCertificate.getPublicKey());
            } else {
                Generated generated = generate();
                this.keyPair = generated.keyPair();
                this.certificate = generated.certificate();
                writeKeyPair(keyPath, this.keyPair);
                writeCertificate(certPath, this.certificate);
            }
        } catch (IOException | GeneralSecurityException failure) {
            throw new CaptureProxyException("Capture CA could not be loaded or generated.", failure);
        }
    }

    /**
     * Returns the CA's root certificate (public data only -- safe to export for device trust
     * installation).
     *
     * @return root CA certificate
     */
    public X509Certificate certificate() {
        return certificate;
    }

    /**
     * Returns the CA's key pair. The private key never leaves this process -- callers must not
     * serialize or transmit it.
     *
     * @return root CA key pair
     */
    public KeyPair keyPair() {
        return keyPair;
    }

    /**
     * Returns the directory this CA's key/certificate are persisted under.
     *
     * @return CA storage directory
     */
    public Path directory() {
        return directory;
    }

    /**
     * Exports the root CA certificate (public data only) as PEM text, for a user to install as a
     * trusted CA on the mobile device/emulator being captured.
     *
     * @return PEM-encoded root CA certificate
     */
    public String exportCertificatePem() {
        try {
            return pemEncode("CERTIFICATE", certificate.getEncoded());
        } catch (java.security.cert.CertificateEncodingException | IOException failure) {
            throw new CaptureProxyException("Capture CA certificate could not be encoded.", failure);
        }
    }

    /**
     * Issues a short-lived leaf certificate for {@code hostname}, signed by this CA, impersonating
     * that host for MITM interception. The leaf never has CA capability (basic constraints
     * {@code CA=false}) so it cannot itself be used to mint further certificates.
     *
     * @param hostname the host being impersonated (recorded as the certificate's CN and DNS SAN)
     * @param leafPublicKey the public key the leaf certificate binds to
     * @return a certificate for {@code hostname}, signed by this CA
     */
    public X509Certificate issueLeafCertificate(String hostname, java.security.PublicKey leafPublicKey) {
        try {
            Instant notBefore = Instant.now().minus(Duration.ofDays(1));
            Instant notAfter = notBefore.plus(Duration.ofDays(825)); // under the ~825-day CA/Browser Forum cap
            X500Name issuer = new X500Name(SUBJECT);
            X500Name subject = new X500Name("CN=" + hostname);
            BigInteger serial = new BigInteger(160, new SecureRandom());

            X509v3CertificateBuilder builder = new JcaX509v3CertificateBuilder(
                    issuer, serial, Date.from(notBefore), Date.from(notAfter), subject, leafPublicKey);
            builder.addExtension(Extension.basicConstraints, true, new BasicConstraints(false));
            builder.addExtension(Extension.keyUsage, true,
                    new KeyUsage(KeyUsage.digitalSignature | KeyUsage.keyEncipherment));
            builder.addExtension(Extension.subjectAlternativeName, false, new org.bouncycastle.asn1.x509.GeneralNames(
                    new org.bouncycastle.asn1.x509.GeneralName(org.bouncycastle.asn1.x509.GeneralName.dNSName, hostname)));

            ContentSigner signer = new JcaContentSignerBuilder("SHA256withRSA")
                    .setProvider(BouncyCastleProvider.PROVIDER_NAME)
                    .build(keyPair.getPrivate());
            X509CertificateHolder holder = builder.build(signer);
            return new JcaX509CertificateConverter().setProvider(BouncyCastleProvider.PROVIDER_NAME).getCertificate(holder);
        } catch (IOException | GeneralSecurityException | org.bouncycastle.operator.OperatorCreationException failure) {
            throw new CaptureProxyException("Leaf certificate for \"" + hostname + "\" could not be issued.", failure);
        }
    }

    private static Generated generate() throws GeneralSecurityException {
        KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
        keyPairGenerator.initialize(2048, new SecureRandom());
        KeyPair keyPair = keyPairGenerator.generateKeyPair();

        Instant notBefore = Instant.now().minus(Duration.ofDays(1));
        Instant notAfter = notBefore.plus(VALIDITY);
        X500Name subject = new X500Name(SUBJECT);
        BigInteger serial = new BigInteger(160, new SecureRandom());

        X509v3CertificateBuilder builder = new JcaX509v3CertificateBuilder(
                subject, serial, Date.from(notBefore), Date.from(notAfter), subject, keyPair.getPublic());
        try {
            builder.addExtension(Extension.basicConstraints, true, new BasicConstraints(true));
            builder.addExtension(Extension.keyUsage, true,
                    new KeyUsage(KeyUsage.keyCertSign | KeyUsage.cRLSign | KeyUsage.digitalSignature));
        } catch (IOException extensionFailure) {
            throw new GeneralSecurityException("Capture CA extensions could not be built.", extensionFailure);
        }

        ContentSigner signer;
        try {
            signer = new JcaContentSignerBuilder("SHA256withRSA")
                    .setProvider(BouncyCastleProvider.PROVIDER_NAME)
                    .build(keyPair.getPrivate());
        } catch (org.bouncycastle.operator.OperatorCreationException signerFailure) {
            throw new GeneralSecurityException("Capture CA content signer could not be built.", signerFailure);
        }
        X509CertificateHolder holder = builder.build(signer);
        X509Certificate certificate = new JcaX509CertificateConverter()
                .setProvider(BouncyCastleProvider.PROVIDER_NAME)
                .getCertificate(holder);
        return new Generated(keyPair, certificate);
    }

    private static void writeKeyPair(Path path, KeyPair keyPair) throws IOException {
        String pem = pemEncode("PRIVATE KEY", keyPair.getPrivate().getEncoded());
        writeAtomicallyThenRestrict(path, pem);
    }

    private static void writeCertificate(Path path, X509Certificate certificate) throws IOException {
        try {
            String pem = pemEncode("CERTIFICATE", certificate.getEncoded());
            writeAtomicallyThenRestrict(path, pem);
        } catch (java.security.cert.CertificateEncodingException failure) {
            throw new IOException("Capture CA certificate could not be encoded.", failure);
        }
    }

    private static void writeAtomicallyThenRestrict(Path destination, String content) throws IOException {
        Path absolute = destination.toAbsolutePath().normalize();
        Path temporary = Files.createTempFile(absolute.getParent(), "." + absolute.getFileName(), ".tmp");
        try {
            Files.writeString(temporary, content, StandardCharsets.UTF_8);
            OwnerOnlyFilePermissions.restrictToOwner(temporary);
            Files.move(temporary, absolute, java.nio.file.StandardCopyOption.REPLACE_EXISTING,
                    java.nio.file.StandardCopyOption.ATOMIC_MOVE);
        } catch (java.nio.file.AtomicMoveNotSupportedException notSupported) {
            Files.move(temporary, absolute, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        } finally {
            Files.deleteIfExists(temporary);
        }
        OwnerOnlyFilePermissions.restrictToOwner(absolute);
    }

    private static String pemEncode(String type, byte[] content) throws IOException {
        StringWriter writer = new StringWriter();
        try (JcaPEMWriter pemWriter = new JcaPEMWriter(writer)) {
            pemWriter.writeObject(new PemObject(type, content));
        }
        return writer.toString();
    }

    private static KeyPair readKeyPair(Path path, java.security.PublicKey matchingPublicKey)
            throws IOException, GeneralSecurityException {
        byte[] der = pemDecode(Files.readString(path, StandardCharsets.UTF_8));
        KeyFactory keyFactory = KeyFactory.getInstance("RSA");
        PrivateKey privateKey = keyFactory.generatePrivate(new PKCS8EncodedKeySpec(der));
        // PKCS8 encodes only the private key; the matching public key is read from the CA's own
        // certificate (always written/read together with this key) rather than re-derived.
        return new KeyPair(matchingPublicKey, privateKey);
    }

    private static X509Certificate readCertificate(Path path) throws IOException, GeneralSecurityException {
        byte[] der = pemDecode(Files.readString(path, StandardCharsets.UTF_8));
        CertificateFactory certificateFactory = CertificateFactory.getInstance("X.509");
        return (X509Certificate) certificateFactory.generateCertificate(new java.io.ByteArrayInputStream(der));
    }

    private static byte[] pemDecode(String pem) throws IOException {
        try (PemReader reader = new PemReader(new StringReader(pem))) {
            PemObject object = reader.readPemObject();
            if (object == null) {
                throw new IOException("Not a valid PEM file.");
            }
            return object.getContent();
        }
    }

    private record Generated(KeyPair keyPair, X509Certificate certificate) {
    }
}
