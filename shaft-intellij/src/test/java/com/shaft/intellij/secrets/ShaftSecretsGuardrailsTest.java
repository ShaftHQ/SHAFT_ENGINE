package com.shaft.intellij.secrets;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftSecretsGuardrailsTest {
    @Test
    void findsSecretsAndEnsuresGitignore(@TempDir Path root) throws Exception {
        Path resources = root.resolve("src/main/resources");
        Files.createDirectories(resources);
        Files.writeString(resources.resolve("secrets.properties"), "cloud.accessKey=super-secret-value\n");
        List<Path> found = ShaftSecretsGuardrails.findSecretsFiles(root);
        assertEquals(1, found.size());
        assertFalse(ShaftSecretsGuardrails.gitignoreMentionsSecrets(root));
        assertTrue(ShaftSecretsGuardrails.ensureGitignoreIgnoresSecrets(root));
        assertTrue(ShaftSecretsGuardrails.gitignoreMentionsSecrets(root));
        assertFalse(ShaftSecretsGuardrails.ensureGitignoreIgnoresSecrets(root));
        Path example = ShaftSecretsGuardrails.writeExampleFile(root);
        assertTrue(Files.isRegularFile(example));
        // second write is idempotent
        assertEquals(example, ShaftSecretsGuardrails.writeExampleFile(root));
    }

    @Test
    void findsRootLevelCredentialFiles(@TempDir Path root) throws Exception {
        Files.writeString(root.resolve("credentials.properties"), "token=abc\n");
        assertEquals(1, ShaftSecretsGuardrails.findSecretsFiles(root).size());
    }

    @Test
    void masksSecretKeys() {
        assertTrue(ShaftSecretsGuardrails.looksLikeSecretKey("cloud.accessKey"));
        assertTrue(ShaftSecretsGuardrails.looksLikeSecretKey("api_key"));
        assertEquals("su****ue", ShaftSecretsGuardrails.maskValue("super-secret-value"));
        assertEquals("****", ShaftSecretsGuardrails.maskValue("ab"));
        assertEquals("", ShaftSecretsGuardrails.maskValue("   "));
        assertEquals("api.key=ab****yz", ShaftSecretsGuardrails.maskPropertiesLine("api.key=abcdyz"));
        assertEquals("no-equals", ShaftSecretsGuardrails.maskPropertiesLine("no-equals"));
        assertEquals("targetAddress=https://example",
                ShaftSecretsGuardrails.maskPropertiesLine("targetAddress=https://example"));
    }

    @Test
    void appendsToExistingGitignoreWithoutSecretEntries(@TempDir Path root) throws Exception {
        Files.writeString(root.resolve(".gitignore"), "target/\n");
        assertTrue(ShaftSecretsGuardrails.ensureGitignoreIgnoresSecrets(root));
        String body = Files.readString(root.resolve(".gitignore"));
        assertTrue(body.contains("target/"));
        assertTrue(body.contains("secrets.properties"));
    }

    @Test
    void findsSecretDotPropertiesName(@TempDir Path root) throws Exception {
        Path resources = root.resolve("src/main/resources");
        Files.createDirectories(resources);
        Files.writeString(resources.resolve("secret.properties"), "api_key=x\n");
        assertEquals(1, ShaftSecretsGuardrails.findSecretsFiles(root).size());
    }
}
