package com.shaft.ai.local;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedLocalAiHardwareTest {
    private static final long GIB = 1024L * 1024 * 1024;

    @TempDir
    Path temp;

    @Test
    void automaticSelectionUsesAvailableMemoryCpuAndExactPeakDisk() {
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        ManagedLocalAiHardware.Profile balanced = new ManagedLocalAiHardware.Profile(
                "windows-x86_64", true, 18 * GIB, 8, 20 * GIB);
        ManagedLocalAiHardware.Selection selected = ManagedLocalAiHardware.select(manifest, balanced, null);
        assertEquals(null, selected.selectedModelId());
        assertTrue(selected.models().values().stream()
                .allMatch(model -> model.reasons().contains("MANUAL_ONLY")));

        ManagedLocalAiManifest.ModelManifest lite = manifest.models().stream()
                .filter(model -> model.id().equals("qwen3-1.7b-q8_0")).findFirst().orElseThrow();
        long exactPeak = ManagedLocalAiHardware.requiredDiskBytes(manifest, balanced, lite);
        ManagedLocalAiHardware.Selection boundary = ManagedLocalAiHardware.select(manifest,
                new ManagedLocalAiHardware.Profile("windows-x86_64", true, 10 * GIB, 4, exactPeak), null);
        assertEquals(null, boundary.selectedModelId());
        assertTrue(boundary.models().get(lite.id()).reasons().contains("MANUAL_ONLY"));
        ManagedLocalAiHardware.Selection oneByteShort = ManagedLocalAiHardware.select(manifest,
                new ManagedLocalAiHardware.Profile("windows-x86_64", true, 10 * GIB, 4, exactPeak - 1), null);
        assertEquals(null, oneByteShort.selectedModelId());
        assertTrue(oneByteShort.models().get(lite.id()).reasons().contains("INSUFFICIENT_DISK"));

        ManagedLocalAiHardware.Profile arm = new ManagedLocalAiHardware.Profile(
                "linux-aarch64", true, 10 * GIB, 4, Long.MAX_VALUE);
        long armPeak = ManagedLocalAiHardware.requiredDiskBytes(manifest, arm, lite);
        long armArchive = manifest.runtime().assets().stream()
                .filter(asset -> asset.platform().equals("linux-aarch64")).findFirst().orElseThrow().size();
        assertEquals(armArchive, armPeak - lite.size() * 2 - 5 * GIB);
        assertEquals(null, ManagedLocalAiHardware.select(manifest,
                new ManagedLocalAiHardware.Profile("linux-aarch64", true, 10 * GIB, 4, armPeak), null)
                .selectedModelId());
        assertEquals(null, ManagedLocalAiHardware.select(manifest,
                new ManagedLocalAiHardware.Profile("linux-aarch64", true, 10 * GIB, 4, armPeak - 1), null)
                .selectedModelId());
    }

    @Test
    void explicitOverrideUsesTheSameGatesAndThirdPartyNeverBecomesAutomatic() {
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        ManagedLocalAiHardware.Profile constrained = new ManagedLocalAiHardware.Profile(
                "linux-x86_64", true, 10 * GIB, 4, 20 * GIB);
        ManagedLocalAiHardware.Selection unsafe = ManagedLocalAiHardware.select(
                manifest, constrained, "qwen3-4b-q4_k_m");
        assertEquals(null, unsafe.selectedModelId());
        assertTrue(unsafe.models().get("qwen3-4b-q4_k_m").reasons().contains("INSUFFICIENT_MEMORY"));

        ManagedLocalAiHardware.Selection automatic = ManagedLocalAiHardware.select(manifest,
                new ManagedLocalAiHardware.Profile("linux-x86_64", true, 32 * GIB, 16, 30 * GIB), null);
        assertEquals(null, automatic.selectedModelId());
        assertTrue(automatic.models().get("qwen3-4b-q4_k_m").reasons().contains("MANUAL_ONLY"));
        assertFalse(automatic.models().get("phi-4-mini-q4_k_m").eligible());

        ManagedLocalAiHardware.Profile compactOnly = new ManagedLocalAiHardware.Profile(
                "linux-x86_64", true, 2 * GIB, 2, 20 * GIB);
        ManagedLocalAiHardware.Selection noUnbenchmarkedAutomatic = ManagedLocalAiHardware.select(
                manifest, compactOnly, null);
        assertEquals(null, noUnbenchmarkedAutomatic.selectedModelId());
        assertTrue(noUnbenchmarkedAutomatic.models().get("qwen3-0.6b-q8_0").reasons().contains("MANUAL_ONLY"));
        assertEquals("qwen3-0.6b-q8_0", ManagedLocalAiHardware.select(
                manifest, compactOnly, "qwen3-0.6b-q8_0").selectedModelId());
    }

    @Test
    void linuxProfileHonorsV2AndV1MemoryCpuQuotaAndCpuset() throws Exception {
        FakeHost v2 = new FakeHost("Linux", "amd64", "linux-glibc", "2.31",
                32 * GIB, 16, 30 * GIB);
        v2.files.put("/sys/fs/cgroup/memory.max", Long.toString(12 * GIB));
        v2.files.put("/sys/fs/cgroup/memory.current", Long.toString(2 * GIB));
        v2.files.put("/sys/fs/cgroup/cpu.max", "150000 100000");
        v2.files.put("/sys/fs/cgroup/cpuset.cpus.effective", "2-3,7");
        ManagedLocalAiHardware.Profile v2Profile = ManagedLocalAiHardware.profile(temp.resolve("absent"), v2);
        assertEquals(8 * GIB, v2Profile.effectiveMemoryBytes());
        assertEquals(1, v2Profile.cpuCount());

        FakeHost v1 = new FakeHost("Linux", "aarch64", "linux-glibc", "2.31",
                32 * GIB, 12, 30 * GIB);
        v1.files.put("/proc/self/cgroup", "5:memory,cpu,cpuset:/workers/job-7\n");
        v1.files.put("/proc/self/mountinfo",
                "31 23 0:27 / /sys/fs/cgroup/combined rw,nosuid - cgroup cgroup rw,memory,cpu,cpuset\n");
        v1.files.put("/sys/fs/cgroup/combined/workers/job-7/memory.limit_in_bytes", Long.toString(16 * GIB));
        v1.files.put("/sys/fs/cgroup/combined/workers/job-7/memory.usage_in_bytes", Long.toString(4 * GIB));
        v1.files.put("/sys/fs/cgroup/combined/workers/job-7/cpu.cfs_quota_us", "200000");
        v1.files.put("/sys/fs/cgroup/combined/workers/job-7/cpu.cfs_period_us", "100000");
        v1.files.put("/sys/fs/cgroup/combined/workers/job-7/cpuset.cpus", "0-7");
        ManagedLocalAiHardware.Profile v1Profile = ManagedLocalAiHardware.profile(temp, v1);
        assertEquals("linux-aarch64", v1Profile.platform());
        assertEquals(10 * GIB, v1Profile.effectiveMemoryBytes());
        assertEquals(2, v1Profile.cpuCount());
    }

    @Test
    void unsupportedAndMalformedSignalsFailClosedWithoutCreatingCache() throws Exception {
        String[][] supported = {
                {"Windows 11", "amd64", "windows-msvc", "", "windows-x86_64"},
                {"Windows 11", "arm64", "windows-msvc", "", "windows-aarch64"},
                {"Mac OS X", "x64", "macos-darwin", "", "macos-x86_64"},
                {"Darwin", "arm64", "macos-darwin", "", "macos-aarch64"},
                {"Linux", "x86_64", "linux-glibc", "2.31", "linux-x86_64"},
                {"Linux", "aarch64", "linux-glibc", "2.31", "linux-aarch64"}
        };
        for (String[] host : supported) {
            FakeHost fake = new FakeHost(host[0], host[1], host[2], host[3], 32 * GIB, 8, 30 * GIB);
            ManagedLocalAiHardware.Profile supportedProfile = ManagedLocalAiHardware.profile(temp, fake);
            assertTrue(supportedProfile.runtimeCompatible());
            assertEquals(host[4], supportedProfile.platform());
        }

        FakeHost unsupported = new FakeHost("Plan9", "mips", "unknown", "", 32 * GIB, 8, 30 * GIB);
        Path cache = temp.resolve("never-created");
        ManagedLocalAiHardware.Profile profile = ManagedLocalAiHardware.profile(cache, unsupported);
        assertFalse(profile.runtimeCompatible());
        assertFalse(java.nio.file.Files.exists(cache));
        ManagedLocalAiHardware.Selection selection = ManagedLocalAiHardware.select(
                ManagedLocalAiManifest.loadDefault(), profile, null);
        assertEquals(null, selection.selectedModelId());
        assertTrue(selection.models().values().stream()
                .allMatch(model -> model.reasons().contains("UNSUPPORTED_RUNTIME")));
    }

    private static final class FakeHost implements ManagedLocalAiHardware.HostAccess {
        private final String os;
        private final String arch;
        private final String abi;
        private final String abiVersion;
        private final long memory;
        private final int processors;
        private final long disk;
        private final Map<String, String> files = new HashMap<>();

        private FakeHost(String os, String arch, String abi, String abiVersion,
                         long memory, int processors, long disk) {
            this.os = os; this.arch = arch; this.abi = abi; this.abiVersion = abiVersion;
            this.memory = memory; this.processors = processors; this.disk = disk;
        }
        public String osName() { return os; }
        public String architecture() { return arch; }
        public String abi() { return abi; }
        public String abiVersion() { return abiVersion; }
        public long availableMemoryBytes() { return memory; }
        public int availableProcessors() { return processors; }
        public long usableSpace(Path existingAncestor) { return disk; }
        public String read(String path) throws java.io.IOException {
            if (!files.containsKey(path)) throw new java.io.IOException("missing");
            return files.get(path);
        }
    }
}
