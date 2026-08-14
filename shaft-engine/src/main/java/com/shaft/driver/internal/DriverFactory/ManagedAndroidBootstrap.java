package com.shaft.driver.internal.DriverFactory;

import com.shaft.infrastructure.AndroidSetupRequest;
import com.shaft.infrastructure.InfrastructureSetupService;
import com.shaft.infrastructure.ManagedEnvironment;
import com.shaft.infrastructure.SetupApproval;
import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.infrastructure.SetupPlan;
import com.shaft.infrastructure.SetupProfile;

import java.io.IOException;
import java.net.URI;
import java.time.Instant;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/** Receipt-bound local Android bootstrap policy used immediately before Appium driver creation. */
final class ManagedAndroidBootstrap {
    private ManagedAndroidBootstrap() { }

    static Optional<Session> startIfConfigured(String executionAddress, String targetPlatform, SetupOptions options,
                                               AndroidSetupRequest request, Gateway gateway) throws IOException {
        if (isExplicitRemote(executionAddress) || !"Android".equalsIgnoreCase(text(targetPlatform))) {
            return Optional.empty();
        }
        if (options.profile() != SetupProfile.MOBILE_ANDROID || !options.autoStart()
                || options.effectiveMode() != SetupMode.MANAGED) {
            return Optional.empty();
        }
        SetupPlan plan = gateway.plan(options, request);
        Set<String> licenses = plan.actions().stream().flatMap(action -> action.requiredLicenses().stream())
                .collect(Collectors.toUnmodifiableSet());
        ManagedEnvironment environment = gateway.start(plan,
                new SetupApproval(plan.digest(), Instant.now(), licenses), options, request);
        URI endpoint = environment.endpoint().orElseThrow(() -> {
            environment.close();
            return new IllegalStateException("Managed Android runtime did not publish an Appium endpoint.");
        });
        return Optional.of(new Session(endpoint, environment.connectionProperties(), environment));
    }

    static Gateway builtInGateway() {
        InfrastructureSetupService service = InfrastructureSetupService.builtIn();
        return new Gateway() {
            @Override
            public SetupPlan plan(SetupOptions options, AndroidSetupRequest request) {
                return service.plan(options, request);
            }

            @Override
            public ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                            AndroidSetupRequest request) throws IOException {
                return service.start(plan, approval, options, request);
            }
        };
    }

    interface Gateway {
        SetupPlan plan(SetupOptions options, AndroidSetupRequest request);
        ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                 AndroidSetupRequest request) throws IOException;
    }

    record Session(URI endpoint, Map<String, String> connectionProperties, ManagedEnvironment environment)
            implements AutoCloseable {
        Session {
            connectionProperties = Map.copyOf(connectionProperties);
        }

        @Override public void close() { environment.close(); }
    }

    private static boolean isExplicitRemote(String value) {
        String normalized = text(value);
        return !normalized.isBlank() && !normalized.equalsIgnoreCase("local")
                && !normalized.equalsIgnoreCase("dockerized");
    }

    private static String text(String value) { return value == null ? "" : value.trim(); }
}
