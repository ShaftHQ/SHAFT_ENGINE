package com.shaft.commandline.command;

import com.shaft.infrastructure.ReportingSetupService;
import com.shaft.infrastructure.InfrastructureSetupService;
import com.shaft.infrastructure.AndroidSetupRequest;
import com.shaft.infrastructure.AndroidRuntimeManager;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.infrastructure.SetupOperation;
import com.shaft.infrastructure.SetupApproval;
import com.shaft.infrastructure.SetupArchitecture;
import com.shaft.infrastructure.SetupCatalog;
import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupPlan;
import com.shaft.infrastructure.SetupPlanJson;
import com.shaft.infrastructure.SetupPlanStore;
import com.shaft.infrastructure.SetupPlatform;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupReadiness;
import com.shaft.infrastructure.SetupReport;
import com.shaft.infrastructure.SetupSelection;
import com.shaft.infrastructure.ShaftCachePaths;
import com.shaft.commandline.util.Json;
import com.shaft.ai.local.ManagedLocalAiService;
import picocli.CommandLine.Command;
import picocli.CommandLine.Mixin;
import picocli.CommandLine.Option;
import picocli.CommandLine.Spec;
import picocli.CommandLine.Model.CommandSpec;

import java.nio.file.Path;
import java.nio.file.Files;
import java.time.Instant;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.List;

/** Direct, MCP-independent setup planning and lifecycle commands. */
@Command(name = "setup", mixinStandardHelpOptions = true,
        description = "Plan and manage SHAFT-owned local infrastructure.", subcommands = {
        SetupCommand.Catalog.class, SetupCommand.Doctor.class, SetupCommand.Status.class,
        SetupCommand.Plan.class, SetupCommand.Install.class, SetupCommand.Verify.class,
        SetupCommand.Start.class, SetupCommand.Stop.class, SetupCommand.Logs.class})
public final class SetupCommand implements Runnable {
    @Spec
    private CommandSpec spec;

    @Override
    public void run() {
        spec.commandLine().usage(spec.commandLine().getOut());
    }

    @Command(name = "catalog", aliases = "profiles", mixinStandardHelpOptions = true,
            description = "Show setup profiles, targets, and capabilities.")
    static final class Catalog implements Callable<Integer> {
        @Option(names = "--json", description = "Print machine-readable JSON.")
        private boolean json;
        @Spec private CommandSpec spec;

        @Override
        public Integer call() {
            SetupCatalog catalog = SetupCatalog.builtIn();
            if (json) spec.commandLine().getOut().println(Json.MAPPER.writerWithDefaultPrettyPrinter()
                    .writeValueAsString(catalog));
            else catalog.profiles().forEach(profile -> spec.commandLine().getOut().println(
                    profile.profile() + "\t" + profile.displayName() + "\t" + profile.targets()));
            return 0;
        }
    }

    @Command(name = "doctor", mixinStandardHelpOptions = true,
            description = "Diagnose host support and reporting prerequisites.")
    static final class Doctor extends ReadinessCommand { }

    @Command(name = "status", mixinStandardHelpOptions = true,
            description = "Report actual setup readiness.")
    static final class Status extends ReadinessCommand { }

    @Command(name = "verify", mixinStandardHelpOptions = true,
            description = "Execute provider verification checks.")
    static final class Verify extends ReadinessCommand { }

    @Command(name = "plan", mixinStandardHelpOptions = true,
            description = "Create an exact, reviewable setup plan.")
    static final class Plan implements Callable<Integer> {
        @Option(names = "--profile", required = true, description = "Setup profile.")
        private SetupProfile profile;

        @Option(names = "--mode", defaultValue = "EXTERNAL", description = "Ownership mode.")
        private SetupMode mode;

        @Option(names = "--operation", defaultValue = "INSTALL", description = "Plan operation.")
        private SetupOperation operation;

        @Option(names = "--output", required = true, description = "Plan JSON output file.")
        private Path output;

        @Option(names = "--json", description = "Print the plan as JSON.")
        private boolean json;

        @Mixin private RootOptions roots;
        @Mixin private PolicyOptions policy;
        @Mixin private AndroidOptions android;
        @Option(names = "--language", description = "OCR language code (repeatable).")
        private List<String> languages = new java.util.ArrayList<>();

        @Spec
        private CommandSpec spec;

        @Override
        public Integer call() {
            try {
                if (!InfrastructureSetupService.builtIn().supports(profile)) {
                    spec.commandLine().getErr().println("No setup provider is available for profile " + profile + '.');
                    return 4;
                }
                if (profile != SetupProfile.OCR && !languages.isEmpty()) {
                    throw new IllegalArgumentException("--language is supported only for profile OCR.");
                }
                SetupOptions options = policy.options(profile, mode, roots.paths(profile));
                SetupSelection selection = selection(profile, languages, android.request(profile));
                SetupPlan plan = InfrastructureSetupService.builtIn(
                        SetupPlatform.current(), SetupArchitecture.current())
                        .plan(options, selection, operation);
                if (!output.isAbsolute()) {
                    spec.commandLine().getErr().println("--output must be an absolute path.");
                    return 2;
                }
                SetupPlanStore.write(output, plan);
                if (json) {
                    spec.commandLine().getOut().print(SetupPlanJson.write(plan));
                } else {
                    spec.commandLine().getOut().println("Plan: " + output.toAbsolutePath().normalize());
                    spec.commandLine().getOut().println("Digest: " + plan.digest());
                }
                return 0;
            } catch (IllegalArgumentException failure) {
                spec.commandLine().getErr().println(failure.getMessage());
                return 2;
            } catch (Exception failure) {
                spec.commandLine().getErr().println(failure.getMessage());
                return 5;
            }
        }
    }

    @Command(name = "install", aliases = {"apply", "update"}, mixinStandardHelpOptions = true,
            description = "Install one exact approved setup plan.")
    static final class Install implements Callable<Integer> {
        @Option(names = "--plan", required = true, description = "Persisted setup plan JSON.")
        private Path planFile;
        @Option(names = "--approve", required = true, description = "Exact plan digest to approve.")
        private String approvedDigest;
        @Option(names = "--accept-license", description = "Accepted license identifier.")
        private Set<String> acceptedLicenses = new LinkedHashSet<>();
        @Option(names = "--json", description = "Print machine-readable JSON.")
        private boolean json;
        @Mixin private RootOptions roots;
        @Mixin private PolicyOptions policy;
        @Mixin private AndroidOptions android;
        @Option(names = "--language", description = "OCR language code from the reviewed plan (repeatable).")
        private List<String> languages = new java.util.ArrayList<>();
        @Spec private CommandSpec spec;

        @Override
        public Integer call() {
            try {
                SetupPlan plan = SetupPlanStore.read(planFile);
                if (plan.profile() != SetupProfile.OCR && !languages.isEmpty()) {
                    throw new IllegalArgumentException("--language is supported only for profile OCR.");
                }
                SetupSelection selection = selectionFromPlan(plan, languages, android);
                SetupOptions options = policy.options(plan.profile(), plan.mode(), roots.paths(plan.profile()));
                var receipt = InfrastructureSetupService.builtIn().install(plan,
                        new SetupApproval(approvedDigest, Instant.now(), acceptedLicenses), options, selection,
                        progress -> spec.commandLine().getErr().println(progress.phase() + "\t"
                                + progress.completedBytes() + "/" + progress.totalBytes() + "\t"
                                + progress.percentage() + "%"));
                if (json) spec.commandLine().getOut().println(Json.MAPPER.writerWithDefaultPrettyPrinter()
                        .writeValueAsString(receipt));
                else spec.commandLine().getOut().println("Installed approved plan " + receipt.planDigest());
                return 0;
            } catch (IllegalArgumentException failure) {
                spec.commandLine().getErr().println(failure.getMessage());
                return 2;
            } catch (Exception failure) {
                spec.commandLine().getErr().println(failureDetails(failure));
                return 5;
            }
        }
    }

    @Command(name = "start", mixinStandardHelpOptions = true,
            description = "Start a SHAFT-owned service from its verified receipt.")
    static final class Start implements Callable<Integer> {
        @Option(names = "--plan", required = true) private Path planFile;
        @Option(names = "--approve", required = true) private String approvedDigest;
        @Option(names = "--accept-license") private Set<String> acceptedLicenses = new LinkedHashSet<>();
        @Option(names = "--json") private boolean json;
        @Option(names = "--language") private List<String> languages = new java.util.ArrayList<>();
        @Mixin private RootOptions roots;
        @Mixin private PolicyOptions policy;
        @Mixin private AndroidOptions android;
        @Spec private CommandSpec spec;

        @Override
        public Integer call() {
            try {
                SetupPlan plan = SetupPlanStore.read(planFile);
                SetupSelection selection = selectionFromPlan(plan, languages, android);
                SetupOptions options = policy.options(plan.profile(), plan.mode(), roots.paths(plan.profile()));
                var environment = InfrastructureSetupService.builtIn().start(plan,
                        new SetupApproval(approvedDigest, Instant.now(), acceptedLicenses), options, selection);
                if (json) spec.commandLine().getOut().println(Json.MAPPER.writerWithDefaultPrettyPrinter()
                        .writeValueAsString(java.util.Map.of("profile", environment.profile(),
                                "endpoint", environment.endpoint().map(Object::toString).orElse(""),
                                "connectionProperties", environment.connectionProperties(),
                                "planDigest", environment.receipt().planDigest())));
                else spec.commandLine().getOut().println("Started " + environment.profile() + " at "
                        + environment.endpoint().map(Object::toString).orElse("owned local runtime"));
                return 0;
            } catch (IllegalArgumentException failure) {
                spec.commandLine().getErr().println(failure.getMessage());
                return 2;
            } catch (Exception failure) {
                spec.commandLine().getErr().println(failureDetails(failure));
                return 5;
            }
        }
    }

    @Command(name = "stop", mixinStandardHelpOptions = true,
            description = "Stop a SHAFT-owned service identified by its lease.")
    static final class Stop implements Callable<Integer> {
        @Option(names = "--plan") private Path planFile;
        @Option(names = "--approve") private String approvedDigest;
        @Option(names = "--accept-license") private Set<String> acceptedLicenses = new LinkedHashSet<>();
        @Option(names = "--profile", description = "Deprecated profile-only compatibility input.")
        private SetupProfile legacyProfile;
        @Option(names = "--json") private boolean json;
        @Option(names = "--language") private List<String> languages = new java.util.ArrayList<>();
        @Mixin private RootOptions roots;
        @Mixin private PolicyOptions policy;
        @Mixin private AndroidOptions android;
        @Spec private CommandSpec spec;

        @Override
        public Integer call() {
            try {
                if (planFile == null || approvedDigest == null || approvedDigest.isBlank()) {
                    throw new IllegalArgumentException("setup stop requires --plan and --approve; profile-only stop "
                            + "cannot satisfy exact approval.");
                }
                SetupPlan plan = SetupPlanStore.read(planFile);
                SetupSelection selection = selectionFromPlan(plan, languages, android);
                ShaftCachePaths paths = roots.paths(plan.profile());
                SetupOptions options = policy.options(plan.profile(), plan.mode(), paths);
                boolean stopped = InfrastructureSetupService.builtIn().stop(plan,
                        new SetupApproval(approvedDigest, Instant.now(), acceptedLicenses), options, selection);
                if (!stopped) {
                    spec.commandLine().getErr().println("No live owned service exists for profile "
                            + plan.profile() + '.');
                    return 3;
                }
                if (json) spec.commandLine().getOut().println(Json.MAPPER.writeValueAsString(
                        java.util.Map.of("profile", plan.profile(), "stopped", true,
                                "planDigest", plan.digest())));
                else spec.commandLine().getOut().println("Stopped the owned service for profile " + plan.profile() + '.');
                return 0;
            } catch (IllegalArgumentException failure) {
                spec.commandLine().getErr().println(failure.getMessage());
                return 2;
            } catch (Exception failure) {
                spec.commandLine().getErr().println(failure.getMessage());
                return 5;
            }
        }
    }

    @Command(name = "logs", mixinStandardHelpOptions = true,
            description = "Read logs for a SHAFT-owned setup provider.")
    static final class Logs implements Callable<Integer> {
        @Option(names = "--profile", required = true) private SetupProfile profile;
        @Mixin private RootOptions roots;
        @Mixin private PolicyOptions policy;
        @Mixin private AndroidOptions android;
        @Spec private CommandSpec spec;

        @Override
        public Integer call() throws Exception {
            if (!InfrastructureSetupService.builtIn().supports(profile)) return unsupported(spec, profile);
            ShaftCachePaths paths = roots.paths(profile);
            String content;
            try {
                content = InfrastructureSetupService.builtIn().logs(
                        policy.options(profile, SetupMode.EXTERNAL, paths),
                        selection(profile, List.of(), android.request(profile)));
            } catch (UnsupportedOperationException unsupported) {
                return unsupported(spec, profile);
            }
            if (content.isEmpty()) {
                spec.commandLine().getErr().println("No owned logs exist for profile " + profile + '.');
                return 3;
            }
            spec.commandLine().getOut().print(content);
            return 0;
        }
    }

    static class ReadinessCommand implements Callable<Integer> {
        @Option(names = "--profile", required = true) private SetupProfile profile;
        @Option(names = "--mode", defaultValue = "EXTERNAL", description = "Ownership mode.")
        private SetupMode mode;
        @Option(names = "--json", description = "Print machine-readable JSON.") private boolean json;
        @Mixin private RootOptions roots;
        @Mixin private PolicyOptions policy;
        @Mixin private AndroidOptions android;
        @Option(names = "--language", description = "OCR language code (repeatable).")
        private List<String> languages = new java.util.ArrayList<>();
        @Spec private CommandSpec spec;

        @Override
        public Integer call() {
            try {
                if (!InfrastructureSetupService.builtIn().supports(profile)) return unsupported(spec, profile);
                if (profile != SetupProfile.OCR && !languages.isEmpty()) {
                    throw new IllegalArgumentException("--language is supported only for profile OCR.");
                }
                ShaftCachePaths paths = roots.paths(profile);
                SetupReport status = InfrastructureSetupService.builtIn().status(
                        policy.options(profile, mode, paths),
                        selection(profile, languages, android.request(profile)));
                if (json) spec.commandLine().getOut().println(Json.MAPPER.writerWithDefaultPrettyPrinter()
                        .writeValueAsString(status));
                else status.targets().forEach(target -> spec.commandLine().getOut().println(
                        target.target() + "\t" + target.readiness() + "\t" + target.detectedVersion()
                                + "\t" + target.detail()));
                return status.readiness() == SetupReadiness.READY ? 0 : 3;
            } catch (IllegalArgumentException failure) {
                spec.commandLine().getErr().println(failure.getMessage());
                return 2;
            }
        }
    }

    private static SetupSelection ocrSelection(SetupPlan plan, List<String> supplied) {
        List<String> fromPlan = plan.actions().stream().map(action -> {
            String version = action.version();
            int separator = version.lastIndexOf(':');
            if (action.target() != com.shaft.infrastructure.SetupTarget.OCR_TESSDATA || separator < 0) {
                throw new IllegalArgumentException("OCR plan contains an unsupported action.");
            }
            return version.substring(separator + 1);
        }).toList();
        SetupSelection selected = new SetupSelection(fromPlan);
        if (!supplied.isEmpty() && !selected.equals(new SetupSelection(supplied))) {
            throw new IllegalArgumentException("--language does not match the reviewed OCR plan.");
        }
        return selected;
    }

    private static SetupSelection selectionFromPlan(SetupPlan plan, List<String> languages,
                                                    AndroidOptions android) {
        if (plan.profile() != SetupProfile.OCR && !languages.isEmpty()) {
            throw new IllegalArgumentException("--language is supported only for profile OCR.");
        }
        return switch (plan.profile()) {
            case OCR -> ocrSelection(plan, languages);
            case MOBILE_ANDROID -> android.selectionFromPlan(plan);
            case MOBILE_IOS, MOBILE_WINDOWS, SELENIUM_GRID, HEALENIUM, REPORT_PORTAL, BROWSERSTACK_LOCAL,
                    AGENT_TOOLS -> {
                android.rejectIfSupplied(plan.profile());
                yield InfrastructureSetupService.builtIn(plan.platform(), plan.architecture())
                        .selectionFromPlan(plan);
            }
            default -> {
                android.rejectIfSupplied(plan.profile());
                yield SetupSelection.defaults();
            }
        };
    }

    private static SetupSelection selection(SetupProfile profile, List<String> languages,
                                            AndroidSetupRequest androidRequest) {
        if (profile == SetupProfile.OCR) return new SetupSelection(languages);
        if (!languages.isEmpty()) throw new IllegalArgumentException("--language is supported only for profile OCR.");
        return profile == SetupProfile.MOBILE_ANDROID
                ? androidRequest.toSelection() : SetupSelection.defaults();
    }

    static class UnsupportedLifecycle implements Callable<Integer> {
        @Option(names = "--profile", required = true) private SetupProfile profile;
        @Spec private CommandSpec spec;
        @Override public Integer call() { return unsupported(spec, profile); }
    }

    static final class RootOptions {
        @Option(names = "--cache-root", description = "Override the absolute SHAFT cache root.")
        private Path cacheRoot;
        @Option(names = "--data-root", description = "Override the absolute SHAFT durable-data root.")
        private Path dataRoot;

        ShaftCachePaths paths() {
            return paths(null);
        }

        ShaftCachePaths paths(SetupProfile profile) {
            if (cacheRoot == null && dataRoot == null) {
                ShaftCachePaths defaults = ShaftCachePaths.current();
                if (profile != SetupProfile.LOCAL_AI) return defaults;
                Path cache = new ManagedLocalAiService().effectiveCacheDirectory();
                return new ShaftCachePaths(cache, defaults.dataRoot(), cache.resolve("downloads"), defaults.tools(),
                        defaults.state(), defaults.receipts());
            }
            if (cacheRoot == null || dataRoot == null) {
                throw new IllegalArgumentException("--cache-root and --data-root must be supplied together.");
            }
            if (!cacheRoot.isAbsolute() || !dataRoot.isAbsolute()) {
                throw new IllegalArgumentException("--cache-root and --data-root must be absolute.");
            }
            Path cache = cacheRoot.normalize();
            Path data = dataRoot.normalize();
            return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                    data.resolve("state"), data.resolve("receipts"));
        }
    }

    static final class PolicyOptions {
        @Option(names = "--offline", description = "Require verified cached artifacts and disable network access.")
        private boolean offline;
        @Option(names = "--auto-start", description = "Request managed service start after verification.")
        private boolean autoStart;
        @Option(names = "--prefer-system-tools", defaultValue = "true",
                description = "Prefer compatible host tools (true or false).")
        private boolean preferSystemTools;
        @Option(names = "--reuse-owned-processes", defaultValue = "true",
                description = "Reuse compatible SHAFT-owned services (true or false).")
        private boolean reuseOwnedProcesses;
        @Option(names = "--startup-timeout", defaultValue = "PT2M")
        private String startupTimeout;
        @Option(names = "--shutdown-timeout", defaultValue = "PT30S")
        private String shutdownTimeout;

        SetupOptions options(SetupProfile profile, SetupMode mode, ShaftCachePaths paths) {
            try {
                return SetupOptions.defaults(profile, paths).withMode(mode).withOffline(offline)
                        .withAutoStart(autoStart).withPreferSystemTools(preferSystemTools)
                        .withReuseOwnedProcesses(reuseOwnedProcesses)
                        .withTimeouts(java.time.Duration.parse(startupTimeout),
                                java.time.Duration.parse(shutdownTimeout));
            } catch (RuntimeException invalid) {
                throw new IllegalArgumentException("Setup timeouts must be positive ISO-8601 durations.", invalid);
            }
        }
    }

    static final class AndroidOptions {
        @Option(names = "--api-level") private Integer apiLevel;
        @Option(names = "--device-profile") private String deviceProfile;
        @Option(names = "--image-tag") private String imageTag;
        @Option(names = "--abi") private String abi;
        @Option(names = "--avd-name") private String avdName;
        @Option(names = "--ram-mb") private Integer ramMb;
        @Option(names = "--cores") private Integer cores;
        @Option(names = "--port") private Integer port;

        AndroidSetupRequest request(SetupProfile profile) {
            if (profile != SetupProfile.MOBILE_ANDROID) {
                rejectIfSupplied(profile);
                return AndroidSetupRequest.defaults();
            }
            return apply(AndroidSetupRequest.defaults());
        }

        SetupSelection selectionFromPlan(SetupPlan plan) {
            AndroidSetupRequest reviewed = AndroidSetupRequest.fromPlan(plan);
            if (hasAny()) {
                AndroidSetupRequest supplied = apply(reviewed);
                if (!supplied.equals(reviewed)) {
                    throw new IllegalArgumentException("Android selectors do not match the reviewed plan.");
                }
            }
            return reviewed.toSelection();
        }

        void rejectIfSupplied(SetupProfile profile) {
            if (hasAny()) throw new IllegalArgumentException(
                    "Android selectors are supported only for profile MOBILE_ANDROID, not " + profile + '.');
        }

        private AndroidSetupRequest apply(AndroidSetupRequest base) {
            return new AndroidSetupRequest(selected(apiLevel, base.apiLevel()),
                    selected(deviceProfile, base.deviceProfile()), selected(imageTag, base.imageTag()),
                    selected(abi, base.abi()), selected(avdName, base.avdName()),
                    selected(ramMb, base.ramMb()), selected(cores, base.cores()),
                    selected(port, base.appiumPort()));
        }

        private static int selected(Integer supplied, int fallback) {
            return supplied == null ? fallback : supplied;
        }

        private static String selected(String supplied, String fallback) {
            return supplied == null ? fallback : supplied;
        }

        private boolean hasAny() {
            return apiLevel != null || deviceProfile != null || imageTag != null || abi != null || avdName != null
                    || ramMb != null || cores != null || port != null;
        }
    }

    private static ReportingSetupService service(RootOptions roots) {
        return new ReportingSetupService(roots.paths(), SetupPlatform.current(), SetupArchitecture.current());
    }

    private static int unsupported(CommandSpec spec, SetupProfile profile) {
        spec.commandLine().getErr().println("No lifecycle provider is available for profile " + profile + '.');
        return 4;
    }

    static String failureDetails(Throwable failure) {
        StringBuilder details = new StringBuilder();
        for (Throwable current = failure; current != null; current = current.getCause()) {
            String message = current.getMessage();
            if (message != null && !message.isBlank()
                    && (details.isEmpty() || !details.toString().endsWith(message))) {
                if (!details.isEmpty()) details.append(": ");
                details.append(message);
            }
        }
        return details.isEmpty() ? failure.getClass().getSimpleName() : details.toString();
    }
}
