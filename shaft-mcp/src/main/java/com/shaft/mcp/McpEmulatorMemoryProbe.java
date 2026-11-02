package com.shaft.mcp;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

/**
 * Warns when an Android emulator host process holds far more memory than its AVD needs (issue #6825).
 * A long Appium session can grow {@code qemu-system-*} to tens of GiB, after which the kernel OOM
 * killer ends it together with the desktop session that owns it.
 */
final class McpEmulatorMemoryProbe {
    /** System property that sets the warning threshold in GiB. */
    static final String WARN_PROPERTY = "shaft.android.emulator.memoryWarnGb";
    static final long DEFAULT_WARN_GB = 6;

    private McpEmulatorMemoryProbe() {
    }

    /** Warnings for the emulator processes currently running on this host; empty off Linux. */
    static List<String> currentWarnings() {
        if (!System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("linux")) {
            return List.of();
        }
        Map<Long, Long> residentKb = new TreeMap<>();
        ProcessHandle.allProcesses().forEach(process -> {
            String command = process.info().command().orElse("").toLowerCase(Locale.ROOT);
            if (command.contains("qemu-system") || command.endsWith("/emulator")) {
                residentKb.put(process.pid(), residentKb(Path.of("/proc", Long.toString(process.pid()), "status")));
            }
        });
        return warnings(residentKb, thresholdGb());
    }

    static List<String> warnings(Map<Long, Long> residentKbByPid, long thresholdGb) {
        List<String> warnings = new ArrayList<>();
        residentKbByPid.forEach((pid, kb) -> {
            long gib = kb / (1024L * 1024L);
            if (kb >= thresholdGb * 1024L * 1024L) {
                warnings.add("Android emulator process " + pid + " holds about " + gib + " GiB of host memory (warning at "
                        + thresholdGb + " GiB). Cold-restart the emulator (setup stop, then setup start) between long "
                        + "suites; SHAFT starts managed emulators under a systemd memory cap on Linux.");
            }
        });
        return warnings;
    }

    static long residentKb(Path statusFile) {
        try {
            for (String line : Files.readAllLines(statusFile)) {
                if (line.startsWith("VmRSS:")) {
                    return Long.parseLong(line.replaceAll("[^0-9]", ""));
                }
            }
        } catch (IOException | NumberFormatException ignored) {
            // process exited or is not readable
        }
        return 0L;
    }

    private static long thresholdGb() {
        try {
            return Long.parseLong(System.getProperty(WARN_PROPERTY, Long.toString(DEFAULT_WARN_GB)).trim());
        } catch (NumberFormatException invalid) {
            return DEFAULT_WARN_GB;
        }
    }
}
