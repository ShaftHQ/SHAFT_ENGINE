package com.shaft.ai.local;

import com.sun.jna.Native;
import com.sun.jna.Structure;
import com.sun.jna.platform.mac.SystemB;
import com.sun.jna.platform.win32.BaseTSD;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Set;

/** Cross-platform aggregate resident-memory sampling for one owned process tree. */
final class ManagedLocalAiProcessTreeRss {
    private ManagedLocalAiProcessTreeRss() {
    }

    static long sample(Process process, Set<ProcessHandle> retainedDescendants) throws Exception {
        if (!process.isAlive()) {
            return 0;
        }
        LinkedHashSet<ProcessHandle> handles = new LinkedHashSet<>();
        ProcessHandle root = process.toHandle();
        handles.add(root);
        root.descendants().forEach(handles::add);
        retainedDescendants.stream().filter(ProcessHandle::isAlive).forEach(handles::add);
        String os = System.getProperty("os.name", "").toLowerCase(Locale.ROOT);
        long total = 0;
        int measured = 0;
        for (ProcessHandle handle : handles) {
            if (!handle.isAlive()) {
                continue;
            }
            try {
                Instant startedAt = handle.info().startInstant()
                        .orElseThrow(() -> new IOException("Managed local AI process identity is unavailable."));
                long rssBytes = processRssBytes(handle.pid(), os);
                ProcessHandle current = ProcessHandle.of(handle.pid()).orElse(null);
                if (current == null || !current.isAlive()) {
                    continue;
                }
                Instant currentStartedAt = current.info().startInstant()
                        .orElseThrow(() -> new IOException("Managed local AI process identity is unavailable."));
                if (!startedAt.equals(currentStartedAt)) {
                    throw new IOException("Managed local AI process identity changed during RSS inspection.");
                }
                total = Math.addExact(total, rssBytes);
                measured++;
            } catch (IOException unavailable) {
                if (handle.isAlive()) {
                    throw unavailable;
                }
            } catch (ArithmeticException overflow) {
                return Long.MAX_VALUE;
            }
        }
        if (process.isAlive() && (measured == 0 || total <= 0)) {
            throw new IOException("Managed local AI process-tree RSS inventory is unavailable.");
        }
        return total;
    }

    private static long processRssBytes(long pid, String os) throws Exception {
        if (os.contains("linux")) {
            return linuxRssBytes(pid);
        }
        if (os.contains("win")) {
            return windowsRssBytes(pid);
        }
        if (os.contains("mac") || os.contains("darwin")) {
            return macRssBytes(pid);
        }
        throw new IOException("Managed local AI process-tree RSS enforcement is unsupported on this platform.");
    }

    private static long linuxRssBytes(long pid) throws IOException {
        Path status = Path.of("/proc", Long.toString(pid), "status");
        for (String line : Files.readAllLines(status, StandardCharsets.UTF_8)) {
            if (line.startsWith("VmRSS:")) {
                String[] fields = line.substring("VmRSS:".length()).trim().split("\\s+");
                if (fields.length >= 1) {
                    try {
                        return Math.multiplyExact(Long.parseLong(fields[0]), 1024L);
                    } catch (ArithmeticException | NumberFormatException malformed) {
                        throw new IOException("Linux process RSS is malformed.", malformed);
                    }
                }
            }
        }
        throw new IOException("Linux process RSS is unavailable.");
    }

    private static long windowsRssBytes(long pid) throws IOException {
        if (pid > Integer.MAX_VALUE) {
            throw new IOException("Windows process identity is invalid for RSS enforcement.");
        }
        WinNT.HANDLE handle = Kernel32.INSTANCE.OpenProcess(
                WinNT.PROCESS_QUERY_INFORMATION | WinNT.PROCESS_VM_READ, false, (int) pid);
        if (handle == null) {
            throw new IOException("Windows process RSS handle is unavailable.");
        }
        try {
            ProcessMemoryCounters counters = new ProcessMemoryCounters();
            counters.cb = counters.size();
            if (!WindowsPsapi.INSTANCE.GetProcessMemoryInfo(handle, counters, counters.size())) {
                throw new IOException("Windows process RSS is unavailable.");
            }
            return counters.workingSetSize.longValue();
        } finally {
            Kernel32.INSTANCE.CloseHandle(handle);
        }
    }

    private static long macRssBytes(long pid) throws IOException {
        if (pid > Integer.MAX_VALUE) {
            throw new IOException("macOS process identity is invalid for RSS enforcement.");
        }
        SystemB.ProcTaskInfo task = new SystemB.ProcTaskInfo();
        int expected = task.size();
        int read = SystemB.INSTANCE.proc_pidinfo((int) pid, SystemB.PROC_PIDTASKINFO, 0, task, expected);
        if (read != expected || task.pti_resident_size <= 0) {
            throw new IOException("macOS process RSS is unavailable.");
        }
        return task.pti_resident_size;
    }

    private interface WindowsPsapi extends StdCallLibrary {
        WindowsPsapi INSTANCE = Native.load("psapi", WindowsPsapi.class, W32APIOptions.DEFAULT_OPTIONS);

        boolean GetProcessMemoryInfo(WinNT.HANDLE process, ProcessMemoryCounters counters, int size);
    }

    @Structure.FieldOrder({"cb", "pageFaultCount", "peakWorkingSetSize", "workingSetSize",
            "quotaPeakPagedPoolUsage", "quotaPagedPoolUsage", "quotaPeakNonPagedPoolUsage",
            "quotaNonPagedPoolUsage", "pagefileUsage", "peakPagefileUsage"})
    public static final class ProcessMemoryCounters extends Structure {
        public int cb;
        public int pageFaultCount;
        public BaseTSD.SIZE_T peakWorkingSetSize;
        public BaseTSD.SIZE_T workingSetSize;
        public BaseTSD.SIZE_T quotaPeakPagedPoolUsage;
        public BaseTSD.SIZE_T quotaPagedPoolUsage;
        public BaseTSD.SIZE_T quotaPeakNonPagedPoolUsage;
        public BaseTSD.SIZE_T quotaNonPagedPoolUsage;
        public BaseTSD.SIZE_T pagefileUsage;
        public BaseTSD.SIZE_T peakPagefileUsage;
    }
}
