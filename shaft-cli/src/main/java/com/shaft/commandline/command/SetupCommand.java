package com.shaft.commandline.command;

import com.shaft.infrastructure.ReportingSetupPlanner;
import com.shaft.infrastructure.ReportingSetupService;
import com.shaft.infrastructure.SetupApproval;
import com.shaft.infrastructure.SetupArchitecture;
import com.shaft.infrastructure.SetupCatalog;
import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupPlan;
import com.shaft.infrastructure.SetupPlanJson;
import com.shaft.infrastructure.SetupPlanStore;
import com.shaft.infrastructure.SetupPlatform;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupProfileStatus;
import com.shaft.infrastructure.SetupReadiness;
import com.shaft.infrastructure.ShaftCachePaths;
import com.shaft.commandline.util.Json;
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

        @Option(names = "--output", required = true, description = "Plan JSON output file.")
        private Path output;

        @Option(names = "--json", description = "Print the plan as JSON.")
        private boolean json;

        @Spec
        private CommandSpec spec;

        @Override
        public Integer call() throws Exception {
            if (profile != SetupProfile.REPORTING) {
                spec.commandLine().getErr().println("No setup provider is available for profile " + profile + '.');
                return 4;
            }
            SetupPlan plan = ReportingSetupPlanner.plan(
                    SetupPlatform.current(), SetupArchitecture.current(), mode);
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
        @Spec private CommandSpec spec;

        @Override
        public Integer call() {
            try {
                SetupPlan plan = SetupPlanStore.read(planFile);
                ReportingSetupService service = service(roots);
                var receipt = service.install(plan,
                        new SetupApproval(approvedDigest, Instant.now(), acceptedLicenses));
                if (json) spec.commandLine().getOut().println(Json.MAPPER.writerWithDefaultPrettyPrinter()
                        .writeValueAsString(receipt));
                else spec.commandLine().getOut().println("Installed approved plan " + receipt.planDigest());
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

    @Command(name = "start", mixinStandardHelpOptions = true,
            description = "Start a SHAFT-owned service from its verified receipt.")
    static final class Start extends UnsupportedLifecycle { }

    @Command(name = "stop", mixinStandardHelpOptions = true,
            description = "Stop a SHAFT-owned service identified by its lease.")
    static final class Stop extends UnsupportedLifecycle { }

    @Command(name = "logs", mixinStandardHelpOptions = true,
            description = "Read logs for a SHAFT-owned setup provider.")
    static final class Logs implements Callable<Integer> {
        @Option(names = "--profile", required = true) private SetupProfile profile;
        @Mixin private RootOptions roots;
        @Spec private CommandSpec spec;

        @Override
        public Integer call() throws Exception {
            if (profile != SetupProfile.REPORTING) return unsupported(spec, profile);
            Path log = service(roots).logFile();
            if (Files.notExists(log)) {
                spec.commandLine().getErr().println("No owned logs exist for profile " + profile + '.');
                return 3;
            }
            spec.commandLine().getOut().print(Files.readString(log));
            return 0;
        }
    }

    static class ReadinessCommand implements Callable<Integer> {
        @Option(names = "--profile", required = true) private SetupProfile profile;
        @Option(names = "--json", description = "Print machine-readable JSON.") private boolean json;
        @Mixin private RootOptions roots;
        @Spec private CommandSpec spec;

        @Override
        public Integer call() {
            if (profile != SetupProfile.REPORTING) return unsupported(spec, profile);
            SetupProfileStatus status = service(roots).status();
            if (json) spec.commandLine().getOut().println(Json.MAPPER.writerWithDefaultPrettyPrinter()
                    .writeValueAsString(status));
            else status.targets().forEach(target -> spec.commandLine().getOut().println(
                    target.target() + "\t" + target.readiness() + "\t" + target.detectedVersion()
                            + "\t" + target.detail()));
            return status.readiness() == SetupReadiness.READY ? 0 : 3;
        }
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
            if (cacheRoot == null && dataRoot == null) return ShaftCachePaths.current();
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

    private static ReportingSetupService service(RootOptions roots) {
        return new ReportingSetupService(roots.paths(), SetupPlatform.current(), SetupArchitecture.current());
    }

    private static int unsupported(CommandSpec spec, SetupProfile profile) {
        spec.commandLine().getErr().println("No lifecycle provider is available for profile " + profile + '.');
        return 4;
    }
}
