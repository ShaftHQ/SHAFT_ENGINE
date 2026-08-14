package com.shaft.ai.local;

import com.sun.management.OperatingSystemMXBean;

import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/** Read-only hardware profiling and deterministic model selection for managed local inference. */
final class ManagedLocalAiHardware {
    private static final long GIB = 1024L * 1024 * 1024;
    private static final long MEMORY_RESERVE_BYTES = 2 * GIB;
    private static final long DISK_RESERVE_BYTES = GIB;
    private static final long MAXIMUM_RUNTIME_EXPANSION_BYTES = 4 * GIB;

    private ManagedLocalAiHardware() {
    }

    static Profile profile(Path cache, HostAccess host) {
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        String platform = platform(host.osName(), host.architecture());
        boolean compatible = manifest.supportsRuntime(
                host.osName(), host.architecture(), host.abi(), host.abiVersion());
        long available = positiveOrZero(host.availableMemoryBytes());
        int processors = Math.max(0, host.availableProcessors());
        if ("linux".equals(normalizedOs(host.osName()))) {
            available = Math.min(available, cgroupAvailableMemory(host).orElse(available));
            processors = Math.min(processors, cgroupProcessors(host).orElse(processors));
        }
        long effectiveMemory = Math.max(0, available - MEMORY_RESERVE_BYTES);
        Path ancestor = nearestExistingAncestor(cache.toAbsolutePath().normalize());
        long disk = ancestor == null ? 0 : positiveOrZero(host.usableSpace(ancestor));
        return new Profile(platform, compatible, effectiveMemory, processors, disk);
    }

    static Selection select(ManagedLocalAiManifest manifest, Profile profile, String requestedModelId) {
        if (requestedModelId != null && manifest.models().stream()
                .noneMatch(model -> model.id().equals(requestedModelId))) {
            throw new IllegalArgumentException("Unknown managed local AI model: " + requestedModelId);
        }
        Map<String, ModelEvaluation> evaluations = new LinkedHashMap<>();
        ManagedLocalAiManifest.ModelManifest selected = null;
        for (ManagedLocalAiManifest.ModelManifest model : manifest.models()) {
            List<String> reasons = new ArrayList<>();
            boolean requested = requestedModelId != null && requestedModelId.equals(model.id());
            if (!profile.runtimeCompatible()) {
                reasons.add("UNSUPPORTED_RUNTIME");
            }
            if (profile.effectiveMemoryBytes() < gibibytes(model.minimumRamGb())) {
                reasons.add("INSUFFICIENT_MEMORY");
            }
            if (profile.cpuCount() < model.minimumCpuCount()) {
                reasons.add("INSUFFICIENT_CPU");
            }
            long requiredDisk = requiredDiskBytes(manifest, profile, model);
            if (profile.freeDiskBytes() < requiredDisk) {
                reasons.add("INSUFFICIENT_DISK");
            }
            if (requestedModelId == null && (!model.automatic() || !model.firstPartyQuantization())) {
                reasons.add("MANUAL_ONLY");
            }
            boolean eligible = reasons.isEmpty();
            evaluations.put(model.id(), new ModelEvaluation(eligible, List.copyOf(reasons), requiredDisk));
            if (eligible && (requested || requestedModelId == null && better(model, selected))) {
                selected = model;
            }
        }
        return new Selection(selected == null ? null : selected.id(), Map.copyOf(evaluations));
    }

    static long requiredDiskBytes(ManagedLocalAiManifest manifest, Profile profile,
                                  ManagedLocalAiManifest.ModelManifest model) {
        long runtimeArchive = manifest.runtime().assets().stream()
                .filter(asset -> asset.platform().equals(profile.platform()))
                .mapToLong(ManagedLocalAiManifest.RuntimeAsset::size)
                .findFirst().orElse(0);
        long peak = saturatedAdd(runtimeArchive, MAXIMUM_RUNTIME_EXPANSION_BYTES);
        peak = saturatedAdd(peak, saturatedMultiply(model.size(), 2));
        peak = saturatedAdd(peak, DISK_RESERVE_BYTES);
        long declared = gibibytes(model.minimumFreeDiskGb());
        return Math.max(peak, declared);
    }

    private static boolean better(ManagedLocalAiManifest.ModelManifest candidate,
                                  ManagedLocalAiManifest.ModelManifest current) {
        if (current == null) {
            return true;
        }
        int ram = Double.compare(candidate.minimumRamGb(), current.minimumRamGb());
        if (ram != 0) {
            return ram > 0;
        }
        int cpu = Integer.compare(candidate.minimumCpuCount(), current.minimumCpuCount());
        return cpu > 0 || cpu == 0 && candidate.id().compareTo(current.id()) < 0;
    }

    private static Optional<Long> cgroupAvailableMemory(HostAccess host) {
        for (String base : cgroupBases(host, "memory")) {
            Optional<Long> maximum = positiveLong(read(host, base + "/memory.max"));
            Optional<Long> current = nonNegativeLong(read(host, base + "/memory.current"));
            if (maximum.isPresent() && current.isPresent()) {
                return Optional.of(Math.max(0, maximum.get() - current.get()));
            }
            maximum = positiveLong(read(host, base + "/memory.limit_in_bytes"));
            current = nonNegativeLong(read(host, base + "/memory.usage_in_bytes"));
            if (maximum.isPresent() && current.isPresent() && maximum.get() < Long.MAX_VALUE / 2) {
                return Optional.of(Math.max(0, maximum.get() - current.get()));
            }
        }
        return Optional.empty();
    }

    private static Optional<Integer> cgroupProcessors(HostAccess host) {
        int limit = Integer.MAX_VALUE;
        boolean constrained = false;
        for (String base : cgroupBases(host, "cpu")) {
            String cpuMax = read(host, base + "/cpu.max");
            if (cpuMax != null) {
                String[] parts = cpuMax.trim().split("\\s+");
                if (parts.length == 2 && !"max".equals(parts[0])) {
                    Optional<Long> quota = positiveLong(parts[0]);
                    Optional<Long> period = positiveLong(parts[1]);
                    if (quota.isPresent() && period.isPresent()) {
                        limit = Math.min(limit, Math.max(1, (int) (quota.get() / period.get())));
                        constrained = true;
                    }
                }
            }
            Optional<Long> quota = positiveLong(read(host, base + "/cpu.cfs_quota_us"));
            Optional<Long> period = positiveLong(read(host, base + "/cpu.cfs_period_us"));
            if (quota.isPresent() && period.isPresent()) {
                limit = Math.min(limit, Math.max(1, (int) (quota.get() / period.get())));
                constrained = true;
            }
        }
        for (String base : cgroupBases(host, "cpuset")) {
            String cpus = read(host, base + "/cpuset.cpus.effective");
            if (cpus == null) {
                cpus = read(host, base + "/cpuset.cpus");
            }
            Optional<Integer> count = countCpuSet(cpus);
            if (count.isPresent()) {
                limit = Math.min(limit, count.get());
                constrained = true;
            }
        }
        return constrained ? Optional.of(limit) : Optional.empty();
    }

    private static List<String> cgroupBases(HostAccess host, String controller) {
        List<String> bases = new ArrayList<>();
        String membership = read(host, "/proc/self/cgroup");
        String mountInfo = read(host, "/proc/self/mountinfo");
        if (membership != null) {
            for (String line : membership.lines().toList()) {
                String[] fields = line.split(":", 3);
                if (fields.length != 3 || !fields[2].startsWith("/")) {
                    continue;
                }
                if (fields[0].equals("0") && fields[1].isEmpty()) {
                    mountedCgroupBase(mountInfo, "cgroup2", controller, fields[2]).ifPresent(bases::add);
                    bases.add("/sys/fs/cgroup" + fields[2]);
                } else if (List.of(fields[1].split(",")).contains(controller)) {
                    mountedCgroupBase(mountInfo, "cgroup", controller, fields[2]).ifPresent(bases::add);
                }
            }
        }
        bases.add("/sys/fs/cgroup");
        bases.add("/sys/fs/cgroup/" + controller);
        return bases.stream().distinct().toList();
    }

    private static Optional<String> mountedCgroupBase(String mountInfo, String fileSystem,
                                                       String controller, String membership) {
        if (mountInfo == null) {
            return Optional.empty();
        }
        for (String line : mountInfo.lines().toList()) {
            String[] fields = line.trim().split("\\s+");
            int separator = List.of(fields).indexOf("-");
            if (separator < 6 || separator + 3 >= fields.length || !fileSystem.equals(fields[separator + 1])) {
                continue;
            }
            if ("cgroup".equals(fileSystem)) {
                boolean mountedController = false;
                for (int index = separator + 2; index < fields.length; index++) {
                    if (List.of(fields[index].split(",")).contains(controller)) {
                        mountedController = true;
                        break;
                    }
                }
                if (!mountedController) {
                    continue;
                }
            }
            String root = fields[3];
            String mountPoint = fields[4];
            if (root.contains("\\") || mountPoint.contains("\\") || !membership.startsWith("/")) {
                continue;
            }
            if (!"/".equals(root) && !(membership.equals(root) || membership.startsWith(root + "/"))) {
                continue;
            }
            String suffix = "/".equals(root) ? membership : membership.substring(root.length());
            return Optional.of(mountPoint + suffix);
        }
        return Optional.empty();
    }

    private static Optional<Integer> countCpuSet(String value) {
        if (value == null || value.isBlank()) {
            return Optional.empty();
        }
        long total = 0;
        try {
            for (String segment : value.trim().split(",")) {
                String[] range = segment.split("-", -1);
                int first = Integer.parseInt(range[0]);
                int last = range.length == 1 ? first : Integer.parseInt(range[1]);
                if (range.length > 2 || first < 0 || last < first) {
                    return Optional.empty();
                }
                total += (long) last - first + 1;
                if (total > Integer.MAX_VALUE) {
                    return Optional.empty();
                }
            }
            return total > 0 ? Optional.of((int) total) : Optional.empty();
        } catch (NumberFormatException malformed) {
            return Optional.empty();
        }
    }

    private static String read(HostAccess host, String path) {
        try {
            return host.read(path);
        } catch (IOException unavailable) {
            return null;
        }
    }

    private static Optional<Long> positiveLong(String value) {
        Optional<Long> parsed = nonNegativeLong(value);
        return parsed.filter(number -> number > 0);
    }

    private static Optional<Long> nonNegativeLong(String value) {
        if (value == null) {
            return Optional.empty();
        }
        try {
            long parsed = Long.parseLong(value.trim());
            return parsed >= 0 ? Optional.of(parsed) : Optional.empty();
        } catch (NumberFormatException malformed) {
            return Optional.empty();
        }
    }

    private static Path nearestExistingAncestor(Path path) {
        Path candidate = path;
        while (candidate != null && !Files.exists(candidate)) {
            candidate = candidate.getParent();
        }
        return candidate;
    }

    private static String platform(String osName, String architecture) {
        String os = normalizedOs(osName);
        String arch = architecture.toLowerCase(Locale.ROOT);
        String normalizedArch = arch.equals("amd64") || arch.equals("x86_64") || arch.equals("x64") ? "x86_64"
                : arch.equals("aarch64") || arch.equals("arm64") ? "aarch64" : "unsupported";
        return os.equals("unsupported") || normalizedArch.equals("unsupported")
                ? "unsupported" : os + "-" + normalizedArch;
    }

    private static String normalizedOs(String osName) {
        String os = osName.toLowerCase(Locale.ROOT);
        if (os.contains("mac") || os.contains("darwin")) return "macos";
        if (os.startsWith("windows")) return "windows";
        if (os.contains("linux")) return "linux";
        return "unsupported";
    }

    private static long gibibytes(double value) {
        if (!Double.isFinite(value) || value <= 0 || value > Long.MAX_VALUE / (double) GIB) {
            return Long.MAX_VALUE;
        }
        return (long) Math.ceil(value * GIB);
    }

    private static long positiveOrZero(long value) {
        return Math.max(0, value);
    }

    private static long saturatedAdd(long left, long right) {
        return left > Long.MAX_VALUE - right ? Long.MAX_VALUE : left + right;
    }

    private static long saturatedMultiply(long value, int multiplier) {
        return value > Long.MAX_VALUE / multiplier ? Long.MAX_VALUE : value * multiplier;
    }

    record Profile(String platform, boolean runtimeCompatible, long effectiveMemoryBytes,
                   int cpuCount, long freeDiskBytes) {
        Profile {
            if (platform == null || platform.isBlank() || effectiveMemoryBytes < 0
                    || cpuCount < 0 || freeDiskBytes < 0) {
                throw new IllegalArgumentException("Invalid managed local AI hardware profile.");
            }
        }
    }

    record ModelEvaluation(boolean eligible, List<String> reasons, long requiredDiskBytes) {
    }

    record Selection(String selectedModelId, Map<String, ModelEvaluation> models) {
    }

    interface HostAccess {
        String osName();
        String architecture();
        String abi();
        String abiVersion();
        long availableMemoryBytes();
        int availableProcessors();
        long usableSpace(Path existingAncestor);
        String read(String path) throws IOException;
    }

    static HostAccess systemHost() {
        return new SystemHostAccess();
    }

    private static final class SystemHostAccess implements HostAccess {
        public String osName() { return System.getProperty("os.name", ""); }
        public String architecture() { return System.getProperty("os.arch", ""); }
        public String abi() {
            return switch (normalizedOs(osName())) {
                case "windows" -> "windows-msvc";
                case "macos" -> "macos-darwin";
                case "linux" -> "linux-glibc";
                default -> "unsupported";
            };
        }
        public String abiVersion() {
            if (!"linux".equals(normalizedOs(osName()))) return "";
            try {
                Process process = new ProcessBuilder("getconf", "GNU_LIBC_VERSION")
                        .redirectErrorStream(true).start();
                if (!process.waitFor(2, TimeUnit.SECONDS)) {
                    process.destroyForcibly();
                    return "";
                }
                String output = new String(process.getInputStream().readNBytes(64)).trim();
                String[] fields = output.split("\\s+");
                return fields.length == 2 && fields[0].equalsIgnoreCase("glibc") ? fields[1] : "";
            } catch (IOException failure) {
                return "";
            } catch (InterruptedException cancelled) {
                Thread.currentThread().interrupt();
                return "";
            }
        }
        public long availableMemoryBytes() {
            java.lang.management.OperatingSystemMXBean bean = ManagementFactory.getOperatingSystemMXBean();
            return bean instanceof OperatingSystemMXBean extended ? extended.getFreeMemorySize() : 0;
        }
        public int availableProcessors() { return Runtime.getRuntime().availableProcessors(); }
        public long usableSpace(Path existingAncestor) { return existingAncestor.toFile().getUsableSpace(); }
        public String read(String path) throws IOException { return Files.readString(Path.of(path)); }
    }
}
