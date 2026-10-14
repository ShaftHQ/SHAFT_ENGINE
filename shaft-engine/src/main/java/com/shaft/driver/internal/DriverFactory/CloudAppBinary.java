package com.shaft.driver.internal.DriverFactory;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Real-device cloud sessions need a device binary and the immutable uploaded build id.
 * A simulator slice and a latest-upload alias are rejected before the session starts.
 */
public final class CloudAppBinary {
    private CloudAppBinary() {
    }

    /**
     * Rejects a simulator or emulator-only binary before upload or session start.
     * An iOS {@code iphonesimulator} slice and an Android APK whose native libraries are only
     * {@code x86} or {@code x86_64} are the same class: they will not run on a real device.
     * A missing file, a non-zip, or an APK with no {@code lib/} directory is left for the upload.
     *
     * @param appFile project binary, or a path that does not exist yet
     * @throws IllegalArgumentException when the binary cannot run on a real device
     */
    public static void rejectSimulatorBinaryForRealDevice(Path appFile) {
        String platform = applePlatform(appFile);
        if (platform != null && platform.toLowerCase(Locale.ROOT).contains("simulator")) {
            throw new IllegalArgumentException(
                    "Refusing a simulator binary for a real-device cloud session. Binary platform is " + platform + ".");
        }
        String emulatorAbi = androidEmulatorOnlyAbi(appFile);
        if (emulatorAbi != null) {
            throw new IllegalArgumentException(
                    "Refusing an emulator-only Android binary for a real-device cloud session. ABI is " + emulatorAbi + ".");
        }
    }

    /**
     * Selects the immutable uploaded id. A latest-upload alias is not the session capability.
     *
     * @param immutableId id returned by the upload (not the alias)
     * @param latestAlias alias that tracks the newest upload, or blank
     * @return {@code immutableId} when it is present and is not the alias
     */
    public static String sessionBuildId(String immutableId, String latestAlias) {
        if (immutableId == null || immutableId.isBlank()
                || (latestAlias != null && !latestAlias.isBlank() && immutableId.equals(latestAlias))) {
            throw new IllegalArgumentException(
                    "Cloud session capability must be the immutable uploaded build id, not a latest-upload alias.");
        }
        return immutableId;
    }

    static String applePlatform(Path appFile) {
        if (appFile == null || !Files.isRegularFile(appFile)) {
            return null;
        }
        try (ZipFile zip = new ZipFile(appFile.toFile())) {
            var entries = zip.entries();
            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                String name = entry.getName();
                if (entry.isDirectory() || name == null || !name.endsWith("Info.plist") || entry.getSize() > 2_000_000L) {
                    continue;
                }
                byte[] bytes;
                try (InputStream input = zip.getInputStream(entry)) {
                    bytes = input.readNBytes(2_000_000);
                }
                String text = new String(bytes, StandardCharsets.ISO_8859_1);
                int simulator = indexOfIgnoreCase(text, "iphonesimulator");
                int device = indexOfIgnoreCase(text, "iphoneos");
                if (simulator >= 0 && (device < 0 || simulator <= device)) {
                    return "iphonesimulator";
                }
                if (device >= 0) {
                    return "iphoneos";
                }
            }
        } catch (IOException ignored) {
            return null;
        }
        return null;
    }

    /**
     * @return the emulator ABIs when every native library is x86 or x86_64, otherwise null
     */
    static String androidEmulatorOnlyAbi(Path appFile) {
        if (appFile == null || !Files.isRegularFile(appFile)) {
            return null;
        }
        java.util.Set<String> abis = new java.util.LinkedHashSet<>();
        try (ZipFile zip = new ZipFile(appFile.toFile())) {
            var entries = zip.entries();
            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                String name = entry.getName();
                if (name == null || !name.startsWith("lib/") || entry.isDirectory()) {
                    continue;
                }
                String[] parts = name.split("/");
                if (parts.length >= 3 && !parts[1].isBlank()) {
                    abis.add(parts[1].toLowerCase(Locale.ROOT));
                }
            }
        } catch (IOException ignored) {
            return null;
        }
        if (abis.isEmpty()) {
            return null;
        }
        boolean emulatorOnly = abis.stream().allMatch(abi -> abi.equals("x86") || abi.equals("x86_64"));
        return emulatorOnly ? String.join(",", abis) : null;
    }

    private static int indexOfIgnoreCase(String text, String token) {
        return text.toLowerCase(Locale.ROOT).indexOf(token);
    }
}
