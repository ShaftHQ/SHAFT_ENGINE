package com.shaft.infrastructure;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * Cold-boot flags and the recycle cadence for a managed Android emulator.
 *
 * <p>{@code -no-snapshot-save} does not drop a snapshot that is loaded at boot.
 * A snapshot or renderer wedge, and a memory-cap kill that leaves the lease half alive,
 * cold-boot with {@code -no-snapshot-load}. A healthy emulator is not recycled per method
 * or between suites. The cadence is pressure-or-wedge: no host qemu timing was available
 * when this cut was chosen, so a timer would be an unmeasured number.
 */
public final class EmulatorBoot {
    /** Recycle only when the emulator is wedged or the memory cap has killed it. */
    public static final String RECYCLE_CADENCE = "pressure-or-wedge";

    private EmulatorBoot() {
    }

    public static boolean snapshotOrRendererWedge(String evidence) {
        if (evidence == null || evidence.isBlank()) {
            return false;
        }
        String text = evidence.toLowerCase(Locale.ROOT);
        boolean snapshotFailed = text.contains("snapshot")
                && (text.contains("fail") || text.contains("mismatch") || text.contains("could not"));
        boolean rendererMismatch = text.contains("renderer")
                && (text.contains("different") || text.contains("mismatch") || text.contains("configured"));
        return snapshotFailed || rendererMismatch;
    }

    /**
     * Emulator process gone and Appium still recorded: the memory cap (or a crash) killed qemu.
     * The survivor Appium is owned by the lease and can be stopped so setup can cold-boot.
     * An emulator that is still alive while Appium is gone is not killed here.
     */
    public static boolean coldBootAfterCapKill(boolean emulatorAlive, boolean appiumAlive) {
        return !emulatorAlive && appiumAlive;
    }

    public static List<String> emulatorCommand(String emulatorExecutable, String avdName, int port,
                                               int ramMb, int cores, boolean coldBoot) {
        List<String> command = new ArrayList<>();
        command.add(emulatorExecutable);
        command.add("-avd");
        command.add(avdName);
        command.add("-port");
        command.add(Integer.toString(port));
        command.add("-no-snapshot-save");
        if (coldBoot) {
            command.add("-no-snapshot-load");
        }
        command.add("-no-boot-anim");
        command.add("-no-audio");
        command.add("-no-window");
        command.add("-memory");
        command.add(Integer.toString(ramMb));
        command.add("-cores");
        command.add(Integer.toString(cores));
        return List.copyOf(command);
    }
}
