package com.shaft.tools.internal.security;

import com.google.crypto.tink.CleartextKeysetHandle;
import com.google.crypto.tink.JsonKeysetWriter;
import com.google.crypto.tink.KeysetHandle;
import com.google.crypto.tink.aead.AeadConfig;
import com.google.crypto.tink.aead.AeadKeyTemplates;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.GeneralSecurityException;

@Test(singleThreaded = true)
public class GoogleTinkApiCoverageUnitTest {
    private static final Path TEMP_DIR = Path.of("target", "temp", "googleTinkCoverage");
    private String originalKeysetFilename;
    private String originalKmsServerType;
    private String originalKmsCredentialPath;
    private String originalKmsMasterKeyUri;
    private String originalTestDataPath;

    @BeforeMethod(alwaysRun = true)
    public void setUp() throws Exception {
        Files.createDirectories(TEMP_DIR);
        originalKeysetFilename = SHAFT.Properties.tinkey.keysetFilename();
        originalKmsServerType = SHAFT.Properties.tinkey.kmsServerType();
        originalKmsCredentialPath = SHAFT.Properties.tinkey.kmsCredentialPath();
        originalKmsMasterKeyUri = SHAFT.Properties.tinkey.kmsMasterKeyUri();
        originalTestDataPath = SHAFT.Properties.paths.testData();
        GoogleTink.keysetFilename = "";
        GoogleTink.kms = "";
        GoogleTink.masterKeyUri = "";
        GoogleTink.credentialPath = "";
        GoogleTink.keysetHandle = null;
        GoogleTink.aead = null;
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        SHAFT.Properties.tinkey.set().keysetFilename(originalKeysetFilename);
        SHAFT.Properties.tinkey.set().kmsServerType(originalKmsServerType);
        SHAFT.Properties.tinkey.set().kmsCredentialPath(originalKmsCredentialPath);
        SHAFT.Properties.tinkey.set().kmsMasterKeyUri(originalKmsMasterKeyUri);
        SHAFT.Properties.paths.set().testData(originalTestDataPath);
        GoogleTink.keysetFilename = "";
        GoogleTink.kms = "";
        GoogleTink.masterKeyUri = "";
        GoogleTink.credentialPath = "";
        GoogleTink.keysetHandle = null;
        GoogleTink.aead = null;
        FileActions.getInstance(true).deleteFolder(TEMP_DIR.toString());
        Properties.clearForCurrentThread();
    }

    @Test
    public void initializeShouldCoverNoopSuccessAndKmsFailureBranches() throws Exception {
        SHAFT.Properties.tinkey.set().keysetFilename("");
        SHAFT.Properties.tinkey.set().kmsServerType("");
        SHAFT.Properties.tinkey.set().kmsCredentialPath("");
        SHAFT.Properties.tinkey.set().kmsMasterKeyUri("");
        GoogleTink.initialize();
        Assert.assertEquals(GoogleTink.keysetFilename, "");

        Path keysetPath = TEMP_DIR.resolve("plaintext_keyset.json");
        createPlaintextKeysetFile(keysetPath);
        SHAFT.Properties.tinkey.set().keysetFilename(keysetPath.toString());
        SHAFT.Properties.tinkey.set().kmsServerType("");
        SHAFT.Properties.tinkey.set().kmsCredentialPath("");
        SHAFT.Properties.tinkey.set().kmsMasterKeyUri("");
        GoogleTink.initialize();
        Assert.assertNotNull(GoogleTink.keysetHandle);
        Assert.assertNotNull(GoogleTink.aead);

        SHAFT.Properties.tinkey.set().kmsServerType("AWS");
        SHAFT.Properties.tinkey.set().kmsCredentialPath("missing-credentials");
        SHAFT.Properties.tinkey.set().kmsMasterKeyUri("aws-kms://invalid-master-key");
        Assert.assertThrows(RuntimeException.class, GoogleTink::initialize);

        SHAFT.Properties.tinkey.set().kmsServerType("GCP");
        SHAFT.Properties.tinkey.set().kmsCredentialPath("missing-credentials");
        SHAFT.Properties.tinkey.set().kmsMasterKeyUri("gcp-kms://invalid-master-key");
        Assert.assertThrows(RuntimeException.class, GoogleTink::initialize);
    }

    @Test
    public void encryptAndDecryptDirectoryShouldCoverBulkAndSingleFileSuccessPaths() throws Exception {
        Path testDataDir = TEMP_DIR.resolve("testData");
        Files.createDirectories(testDataDir);
        String fileName = "secret.txt";
        String originalValue = "sha256-demo-text";
        Files.writeString(testDataDir.resolve(fileName), originalValue, StandardCharsets.UTF_8);

        GoogleTink.aead = Mockito.mock(com.google.crypto.tink.Aead.class);
        Mockito.when(GoogleTink.aead.encrypt(Mockito.any(byte[].class), Mockito.eq(GoogleTink.aad)))
                .thenAnswer(invocation -> ("ENC:" + new String(invocation.getArgument(0), StandardCharsets.UTF_8)).getBytes(StandardCharsets.UTF_8));
        Mockito.when(GoogleTink.aead.decrypt(Mockito.any(byte[].class), Mockito.eq(GoogleTink.aad)))
                .thenAnswer(invocation -> {
                    String cipherText = new String(invocation.getArgument(0), StandardCharsets.UTF_8);
                    return cipherText.replaceFirst("^ENC:", "").getBytes(StandardCharsets.UTF_8);
                });

        SHAFT.Properties.paths.set().testData(testDataDir.toString());
        GoogleTink.keysetFilename = "enabled";
        GoogleTink.encrypt();
        Assert.assertTrue(Files.readString(testDataDir.resolve(fileName)).startsWith("ENC:"));

        GoogleTink.decrypt();
        Assert.assertEquals(Files.readString(testDataDir.resolve(fileName)), originalValue);

        GoogleTink.encrypt(testDataDir + File.separator, fileName);
        Assert.assertTrue(Files.readString(testDataDir.resolve(fileName)).startsWith("ENC:"));
        GoogleTink.decrypt(testDataDir + File.separator, fileName);
        Assert.assertEquals(Files.readString(testDataDir.resolve(fileName)), originalValue);

        GoogleTink.keysetFilename = "";
        GoogleTink.encrypt();
        GoogleTink.decrypt();
    }

    @Test
    public void encryptAndDecryptShouldCoverErrorHandlingBranches() throws Exception {
        Path testDataDir = TEMP_DIR.resolve("errors");
        Files.createDirectories(testDataDir);
        String fileName = "cipher.txt";
        Files.writeString(testDataDir.resolve(fileName), "cipher", StandardCharsets.UTF_8);

        GoogleTink.aead = Mockito.mock(com.google.crypto.tink.Aead.class);
        Mockito.when(GoogleTink.aead.encrypt(Mockito.any(byte[].class), Mockito.eq(GoogleTink.aad)))
                .thenThrow(new GeneralSecurityException("forced encryption failure"));
        Assert.assertThrows(RuntimeException.class, () -> GoogleTink.encrypt(testDataDir + File.separator, fileName));

        GoogleTink.aead = Mockito.mock(com.google.crypto.tink.Aead.class);
        Mockito.when(GoogleTink.aead.decrypt(Mockito.any(byte[].class), Mockito.eq(GoogleTink.aad)))
                .thenThrow(new GeneralSecurityException("forced decryption failure"));
        GoogleTink.decrypt(testDataDir + File.separator, fileName);
        Assert.assertEquals(Files.readString(testDataDir.resolve(fileName)), "cipher");
    }

    private void createPlaintextKeysetFile(Path keysetPath) throws Exception {
        AeadConfig.register();
        KeysetHandle keysetHandle = KeysetHandle.generateNew(AeadKeyTemplates.AES256_GCM);
        CleartextKeysetHandle.write(keysetHandle, JsonKeysetWriter.withFile(new File(keysetPath.toString())));
    }
}
