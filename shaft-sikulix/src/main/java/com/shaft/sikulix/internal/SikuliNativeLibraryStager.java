package com.shaft.sikulix.internal;

import com.shaft.tools.io.internal.ReportManagerHelper;
import org.opencv.core.Core;
import org.sikuli.script.support.Commons;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Comparator;
import java.util.stream.Stream;

/**
 * Stages the native OpenCV matcher library into SikuliX's own "libs folder"
 * ({@code %APPDATA%/Sikulix/SikulixLibs} on Windows, {@code ~/Library/Application
 * Support/Sikulix/SikulixLibs} on macOS, {@code ~/.Sikulix/SikulixLibs} on Linux -- see
 * {@link Commons#getAppDataPath()}) before any SikuliX class touches native loading.
 * <p>
 * SikuliX 2.0.5 resolves its native OpenCV binary via
 * {@code org.sikuli.script.support.RunTime#libsLoad}, which only ever looks inside that
 * folder for a file named after the OpenCV build actually on the classpath
 * ({@link Core#NATIVE_LIBRARY_NAME} plus the platform extension). SikuliX populates the
 * folder itself, on first use, from a manifest bundled in its own jar
 * ({@code sikulixlibs/<os>/libs64/sikulixcontent}) -- but that manifest still names
 * {@code opencv_java430.dll} (OpenCV 4.3.0), stale since shaft-sikulix/pom.xml pinned
 * {@code org.openpnp:opencv:4.9.0-0} directly (PR #3408) to fix an unrelated
 * {@code NoClassDefFoundError}. That jar only contains {@code opencv_java490.dll}, so
 * SikuliX's own extraction step always fails, deletes the whole folder it just created,
 * and every later native-lib lookup throws
 * {@code SikuliXception: loadlib: opencv_java490.dll not in any libs folder} (issue
 * #3805; confirmed via {@code RunTime:libsExport: opencv_java430.dll: failed} in the
 * Windows_SikuliX_Local nightly log).
 * <p>
 * Rather than relying on SikuliX's stale manifest, this pre-stages the native that is
 * actually on the classpath (bundled as a resource inside {@code org.openpnp:opencv})
 * directly under the exact file name SikuliX's loader will ask for, and marks the
 * folder as already prepared via {@link Commons#makeVersionFile(File)} so SikuliX's own
 * (otherwise-failing) extraction step is skipped rather than re-run on top of it.
 */
public final class SikuliNativeLibraryStager {

    private SikuliNativeLibraryStager() {
    }

    /**
     * Ensures the OpenCV native SikuliX will look for is present in its libs folder.
     * Safe to call repeatedly -- a no-op once already staged for the running SikuliX
     * build -- and a best-effort no-op if the target location cannot be determined or
     * written, in which case SikuliX's own loader still surfaces its usual
     * {@code SikuliXception}, unchanged from today's behavior.
     */
    public static synchronized void ensureOpenCvNativeStaged() {
        try {
            File libsFolder = new File(Commons.getAppDataPath(), "SikulixLibs");
            if (libsFolder.isDirectory() && Commons.hasVersionFile(libsFolder)) {
                return; // already staged for this exact SikuliX build
            }
            if (libsFolder.exists()) {
                deleteRecursively(libsFolder.toPath());
            }
            String nativeLibFileName = platformLibraryFileName(Core.NATIVE_LIBRARY_NAME);
            String openCvResourcePath = openCvResourcePath(nativeLibFileName);
            try (InputStream nativeLibStream = nu.pattern.OpenCV.class.getResourceAsStream(openCvResourcePath)) {
                if (nativeLibStream == null) {
                    return; // unsupported OS/arch combination for org.openpnp:opencv; nothing we can stage
                }
                Files.createDirectories(libsFolder.toPath());
                Files.copy(nativeLibStream, new File(libsFolder, nativeLibFileName).toPath(),
                        StandardCopyOption.REPLACE_EXISTING);
            }
            Commons.makeVersionFile(libsFolder);
        } catch (Exception rootCauseException) {
            ReportManagerHelper.logDiscrete(rootCauseException);
        }
    }

    private static String platformLibraryFileName(String libraryName) {
        if (Commons.runningWindows()) {
            return libraryName + ".dll";
        }
        if (Commons.runningMac()) {
            return "lib" + libraryName + ".dylib";
        }
        return "lib" + libraryName + ".so";
    }

    private static String openCvResourcePath(String nativeLibFileName) {
        String osArch = System.getProperty("os.arch", "").toLowerCase();
        if (Commons.runningWindows()) {
            String archSegment = osArch.contains("64") ? "x86_64" : "x86_32";
            return "/nu/pattern/opencv/windows/" + archSegment + "/" + nativeLibFileName;
        }
        if (Commons.runningMac()) {
            String archSegment = osArch.contains("aarch64") ? "ARMv8" : "x86_64";
            return "/nu/pattern/opencv/osx/" + archSegment + "/" + nativeLibFileName;
        }
        String archSegment = osArch.contains("aarch64") ? "ARMv8" : osArch.contains("arm") ? "ARMv7" : "x86_64";
        return "/nu/pattern/opencv/linux/" + archSegment + "/" + nativeLibFileName;
    }

    private static void deleteRecursively(Path path) throws IOException {
        if (!Files.exists(path)) {
            return;
        }
        try (Stream<Path> walk = Files.walk(path)) {
            walk.sorted(Comparator.reverseOrder()).forEach(entry -> {
                try {
                    Files.deleteIfExists(entry);
                } catch (IOException ignored) {
                    // best-effort cleanup of a previous, stale libs folder
                }
            });
        }
    }
}
