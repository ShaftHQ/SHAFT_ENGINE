package com.shaft.driver.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

public class CloudAppBinaryTest {
    @Test
    public void simulatorIpaAndEmulatorOnlyApkAreRejectedBeforeARealDeviceSession() throws IOException {
        Path simulator = zip("Payload/App.app/Info.plist", "DTPlatformName iphonesimulator");
        Path device = zip("Payload/App.app/Info.plist", "DTPlatformName iphoneos");
        Path emulatorApk = zip("lib/x86_64/libapp.so", "native");
        Path deviceApk = zip("lib/arm64-v8a/libapp.so", "native");

        Assert.assertThrows(IllegalArgumentException.class,
                () -> CloudAppBinary.rejectSimulatorBinaryForRealDevice(simulator));
        CloudAppBinary.rejectSimulatorBinaryForRealDevice(device);
        Assert.assertThrows(IllegalArgumentException.class,
                () -> CloudAppBinary.rejectSimulatorBinaryForRealDevice(emulatorApk));
        CloudAppBinary.rejectSimulatorBinaryForRealDevice(deviceApk);
        CloudAppBinary.rejectSimulatorBinaryForRealDevice(Path.of("/tmp/shaft-missing-app.apk"));
    }

    @Test
    public void sessionCapabilityIsTheImmutableUploadId() {
        Assert.assertEquals(CloudAppBinary.sessionBuildId("bs://uploaded-app", "shaft-engine_ApiDemos"),
                "bs://uploaded-app");
        Assert.assertThrows(IllegalArgumentException.class,
                () -> CloudAppBinary.sessionBuildId("latest-alias", "latest-alias"));
        Assert.assertThrows(IllegalArgumentException.class,
                () -> CloudAppBinary.sessionBuildId("  ", "latest-alias"));
    }

    private static Path zip(String entryName, String body) throws IOException {
        Path file = Files.createTempFile("shaft-cloud-app", ".zip");
        file.toFile().deleteOnExit();
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(file))) {
            zip.putNextEntry(new ZipEntry(entryName));
            zip.write(body.getBytes(StandardCharsets.UTF_8));
            zip.closeEntry();
        }
        return file;
    }
}
