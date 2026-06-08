package com.shaft.tools.internal.security;

import com.google.crypto.tink.Aead;
import com.google.crypto.tink.CleartextKeysetHandle;
import com.google.crypto.tink.JsonKeysetReader;
import com.google.crypto.tink.KeysetHandle;
import com.google.crypto.tink.daead.DeterministicAeadConfig;
import com.google.crypto.tink.hybrid.HybridConfig;
import com.google.crypto.tink.integration.awskms.AwsKmsClient;
import com.google.crypto.tink.integration.gcpkms.GcpKmsClient;
import com.google.crypto.tink.prf.PrfConfig;
import com.google.crypto.tink.signature.SignatureConfig;
import com.google.crypto.tink.streamingaead.StreamingAeadConfig;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Optional;

public class GoogleTink {
    static final byte[] aad = "This is SHAFT_Engine".getBytes();
    static String keysetFilename;
    static String kms;
    static String masterKeyUri;
    static String credentialPath;
    static KeysetHandle keysetHandle;
    static Aead aead;


    public static void initialize() {
        keysetFilename = SHAFT.Properties.tinkey.keysetFilename();
        masterKeyUri = SHAFT.Properties.tinkey.kmsMasterKeyUri();
        kms = SHAFT.Properties.tinkey.kmsServerType();
        credentialPath = SHAFT.Properties.tinkey.kmsCredentialPath();
        if (!"".equals(keysetFilename)) {
            try {
                DeterministicAeadConfig.register();
                HybridConfig.register();
                PrfConfig.register();
                SignatureConfig.register();
                StreamingAeadConfig.register();
                if ("AWS".equals(kms)) {
                    AwsKmsClient.register(Optional.ofNullable(masterKeyUri), Optional.ofNullable(credentialPath));
                } else if ("GCP".equals(kms)) {
                    GcpKmsClient.register(Optional.ofNullable(masterKeyUri), Optional.ofNullable(credentialPath));
                }
                keysetHandle = internal_loadKeyset();
                aead = keysetHandle.getPrimitive(Aead.class);
            } catch (GeneralSecurityException | IOException e) {
                FailureReporter.fail(GoogleTink.class, "Failed to Initialize Google Tink Configuration.", e);
            }
        }
    }

    public static void encrypt() {
        if (!"".equals(keysetFilename)) {
            String relativeFolderPath = SHAFT.Properties.paths.testData();
            ReportManager.logDiscrete("Loading test data files from target directory \"" + relativeFolderPath + "\" to be encrypted...");
            SHAFT.Properties.reporting.set().disableLogging(true);
            var filesList = FileActions.getInstance(true).getFileList(relativeFolderPath);
            SHAFT.Properties.reporting.set().disableLogging(false);
            filesList.forEach(file -> encrypt(file.getParent() + File.separator, file.getName()));
            ReportManager.log("Successfully Encrypted the test data directory \"" + relativeFolderPath + "\".");
        }
    }

    public static void decrypt() {
        if (!"".equals(keysetFilename)) {
            String relativeFolderPath = SHAFT.Properties.paths.testData();
            ReportManager.logDiscrete("Loading test data files from target directory \"" + relativeFolderPath + "\" to be decrypted...");
            SHAFT.Properties.reporting.set().disableLogging(true);
            var filesList = FileActions.getInstance(true).getFileList(relativeFolderPath);
            SHAFT.Properties.reporting.set().disableLogging(false);
            filesList.forEach(file -> decrypt(file.getParent() + File.separator, file.getName()));
            ReportManager.log("Successfully Decrypted the test data directory \"" + relativeFolderPath + "\".");
        }
    }

    public static void encrypt(String relativeFolderPath, String targetFileName) {
        byte[] ciphertext;
        try {
            ciphertext = internal_encrypt(FileActions.getInstance(true).readFileAsByteArray(relativeFolderPath + targetFileName));
            FileActions.getInstance(true).writeToFile(relativeFolderPath, targetFileName, ciphertext);
            ReportManager.log("Successfully Encrypted \"" + targetFileName + "\".");
        } catch (GeneralSecurityException e) {
            FailureReporter.fail(GoogleTink.class, "Failed to Encrypt \"" + targetFileName + "\".", e);
        }
    }

    public static void decrypt(String relativeFolderPath, String targetFileName) {
        byte[] decryptedText;
        try {
            decryptedText = internal_decrypt(FileActions.getInstance(true).readFileAsByteArray(relativeFolderPath + targetFileName));
            FileActions.getInstance(true).writeToFile(relativeFolderPath, targetFileName, decryptedText);
            ReportManager.log("Successfully Decrypted \"" + targetFileName + "\".");
        } catch (GeneralSecurityException e) {
            ReportManagerHelper.logDiscrete(e);
            ReportManager.log("Failed to Decrypt \"" + targetFileName + "\". It may already be in plaintext.");
//            FailureReporter.fail(GoogleTink.class,"Failed to Decrypt \""+targetFileName+"\".", e);
        }
    }

    private static KeysetHandle internal_loadKeyset() throws IOException, GeneralSecurityException {
        if (!"".equals(masterKeyUri)) {
            // working with encrypted keyset https://developers.google.com/tink/generate-encrypted-keyset
            return KeysetHandle.read(JsonKeysetReader.withInputStream(new FileInputStream(keysetFilename)), new AwsKmsClient().getAead(masterKeyUri));
        } else {
            // working with plaintext keyset https://developers.google.com/tink/generate-plaintext-keyset
            return CleartextKeysetHandle.read(JsonKeysetReader.withInputStream(new FileInputStream(keysetFilename)));
        }
    }

    private static byte[] internal_encrypt(byte[] plaintext) throws GeneralSecurityException {
        //  AEAD (Authenticated Encryption with Associated Data)
        return aead.encrypt(plaintext, aad);
    }

    private static byte[] internal_decrypt(byte[] ciphertext) throws GeneralSecurityException {
        //  AEAD (Authenticated Encryption with Associated Data)
        return aead.decrypt(ciphertext, aad);
    }
}
