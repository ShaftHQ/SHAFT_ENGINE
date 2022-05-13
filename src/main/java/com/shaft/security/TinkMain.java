package com.shaft.security;

import com.google.crypto.tink.Aead;
import com.google.crypto.tink.KeyTemplates;
import com.google.crypto.tink.KeysetHandle;
import com.google.crypto.tink.aead.AeadConfig;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.GeneralSecurityException;

public class TinkMain {
    public static void main(String [] args) throws GeneralSecurityException, IOException {
        String fileName="users.json";
        File inputFileName = new File("src/test/resources/crypto/"+fileName);
        File encryptedFile = new File("src/test/resources/crypto/"+fileName+".cph");
        File decryptedFile = new File("src/test/resources/crypto/"+fileName+"(decrypted).cph");
        TinkCrypto tinkCrypto= new TinkCrypto();
        tinkCrypto.setInputFile(inputFileName).setOutputFile(encryptedFile).encrypt();
        tinkCrypto.setInputFile(encryptedFile).setOutputFile(decryptedFile).decrypt();
    }
}
