package com.shaft.test.unittests;

import com.shaft.tools.internal.support.StaleApkSignatureCleaner;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/** #7321: a stale {@code .idsig} next to a replaced APK is removed; a current one is kept. */
public class StaleApkSignatureCleanerUnitTest {
    private static Path apkWithSignature(long signatureAgeMillis) throws IOException {
        Path folder = Files.createDirectories(Path.of("target", "unit-7321")).toAbsolutePath();
        Path apk = Files.createTempFile(folder, "app-under-test", ".apk");
        Path signature = Path.of(apk + ".idsig");
        Files.write(signature, new byte[]{1});
        long now = System.currentTimeMillis();
        Assert.assertTrue(apk.toFile().setLastModified(now));
        Assert.assertTrue(signature.toFile().setLastModified(now + signatureAgeMillis));
        return apk;
    }

    @Test
    public void signatureOlderThanTheApkIsDeleted() throws IOException {
        Path apk = apkWithSignature(-60_000);
        Assert.assertTrue(StaleApkSignatureCleaner.removeStaleSignature(apk.toFile()).isPresent());
        Assert.assertFalse(Files.exists(Path.of(apk + ".idsig")));
        Assert.assertTrue(Files.exists(apk));
    }

    @Test
    public void signatureNewerThanTheApkIsKept() throws IOException {
        Path apk = apkWithSignature(60_000);
        Assert.assertTrue(StaleApkSignatureCleaner.removeStaleSignature(apk.toFile()).isEmpty());
        Assert.assertTrue(Files.exists(Path.of(apk + ".idsig")));
    }

    @Test
    public void nonApkOrMissingFileIsIgnored() {
        Assert.assertTrue(StaleApkSignatureCleaner.removeStaleSignature(new File("target/none.apk")).isEmpty());
        Assert.assertTrue(StaleApkSignatureCleaner.removeStaleSignature(null).isEmpty());
    }
}
