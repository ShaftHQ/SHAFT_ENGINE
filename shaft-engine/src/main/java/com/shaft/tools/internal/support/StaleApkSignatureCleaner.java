package com.shaft.tools.internal.support;

import java.io.File;
import java.util.Optional;

/**
 * Removes a stale {@code <apk>.idsig} left by an earlier incremental install (#7321).
 * <p>
 * {@code adb install} on API 30+ switches to incremental install when an {@code .idsig} sits next to the
 * APK. When the APK was replaced at the same path, the old signature no longer matches and the install
 * fails with {@code INSTALL_PARSE_FAILED_NO_CERTIFICATES}, which surfaces as a generic session-creation
 * failure. A signature older than its APK cannot belong to it, so it is deleted; a newer one is kept.
 */
public final class StaleApkSignatureCleaner {
    private StaleApkSignatureCleaner() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Deletes {@code apk + ".idsig"} when it is older than {@code apk}.
     *
     * @param apk the local APK about to be installed
     * @return the deleted signature file, or empty when nothing was stale or it could not be deleted
     */
    public static Optional<File> removeStaleSignature(File apk) {
        if (apk == null || !apk.isFile() || !apk.getName().toLowerCase().endsWith(".apk")) {
            return Optional.empty();
        }
        File signature = new File(apk.getPath() + ".idsig");
        if (!signature.isFile() || signature.lastModified() >= apk.lastModified()) {
            return Optional.empty();
        }
        return signature.delete() ? Optional.of(signature) : Optional.empty();
    }
}
