package com.shaft.security;

import com.google.crypto.tink.Aead;
import com.google.crypto.tink.KeyTemplates;
import com.google.crypto.tink.KeysetHandle;
import com.google.crypto.tink.aead.AeadConfig;
import lombok.SneakyThrows;

import java.io.*;
import java.security.GeneralSecurityException;

public class TinkCrypto {
    KeysetHandle keysetHandle;
    Aead aead;
    byte[] inputBytes;
    byte[] ciphertext;
    FileInputStream inputStream;
    FileOutputStream outputStream;
    byte[] iv = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

    @SneakyThrows
    public TinkCrypto() {
        AeadConfig.register();
        keysetHandle = KeysetHandle.generateNew(KeyTemplates.get("AES128_GCM"));
        aead = keysetHandle.getPrimitive(Aead.class);
    }

    @SneakyThrows
    public TinkCrypto setInputFile(File inputFileName) {
        inputStream = new FileInputStream(inputFileName);
        inputBytes = new byte[(int) inputFileName.length()];
        inputStream.read(inputBytes);
        System.out.println("input:" + inputBytes);
        return this;
    }


    @SneakyThrows
    public TinkCrypto setOutputFile(File outputFile) {
        outputStream = new FileOutputStream(outputFile);
        return this;
    }

    @SneakyThrows
    public TinkCrypto encrypt() {
        ciphertext = aead.encrypt(inputBytes, iv);
        System.out.println("ciphertext:" + ciphertext);
        outputStream.write(ciphertext);
        return null;
    }


    public TinkCrypto decrypt() {
        try {
            byte[] decrypted = aead.decrypt(inputBytes, iv);
            System.out.println("ciphertext:" + decrypted);
            outputStream.write(decrypted);
        } catch (GeneralSecurityException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}
