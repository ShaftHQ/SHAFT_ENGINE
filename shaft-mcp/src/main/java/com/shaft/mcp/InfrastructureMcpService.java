package com.shaft.mcp;

import com.shaft.infrastructure.InfrastructureSetupService;
import com.shaft.infrastructure.SetupApproval;
import com.shaft.infrastructure.SetupCatalog;
import com.shaft.infrastructure.SetupOperation;
import com.shaft.infrastructure.SetupPlan;
import com.shaft.infrastructure.SetupPlanJson;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupReceipt;
import com.shaft.infrastructure.SetupReport;
import com.shaft.infrastructure.SetupSelection;
import com.shaft.infrastructure.SetupTarget;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.time.Instant;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;

/** MCP adapter over the provider-neutral setup coordinator. */
@Service
public final class InfrastructureMcpService {
    private final InfrastructureSetupService coordinator;

    public InfrastructureMcpService() {
        this(InfrastructureSetupService.builtIn());
    }

    InfrastructureMcpService(InfrastructureSetupService coordinator) {
        this.coordinator = Objects.requireNonNull(coordinator, "coordinator");
    }

    @Tool(name = "setup_catalog",
            description = "returns the deterministic SHAFT setup profiles, targets, and capabilities")
    public SetupCatalog setupCatalog() {
        return coordinator.catalog();
    }

    @Tool(name = "setup_doctor",
            description = "diagnoses a setup profile without installing, downloading, or starting services")
    public SetupReport setupDoctor(@ToolParam(description = "setup profile, paths, policy, and components")
                                   McpSetupRequest request) {
        McpSetupRequest value = requireRequest(request);
        return coordinator.status(value.options(), value.selection());
    }

    @Tool(name = "setup_status",
            description = "returns actual read-only readiness for a registered setup profile")
    public SetupReport setupStatus(@ToolParam(description = "setup profile, paths, policy, and components")
                                   McpSetupRequest request) {
        McpSetupRequest value = requireRequest(request);
        return coordinator.status(value.options(), value.selection());
    }

    @Tool(name = "setup_verify",
            description = "verifies exact setup readiness without mutating the host")
    public SetupReport setupVerify(@ToolParam(description = "setup profile, paths, policy, and components")
                                   McpSetupRequest request) {
        McpSetupRequest value = requireRequest(request);
        return coordinator.verify(value.options(), value.selection());
    }

    @Tool(name = "setup_plan",
            description = "creates an exact reviewable setup plan and approval digest without mutating the host")
    public McpSetupPlanResult setupPlan(
            @ToolParam(description = "setup profile, ownership mode, paths, policy, and components")
            McpSetupRequest request) {
        McpSetupRequest value = requireRequest(request);
        SetupOperation operation = value.setupOperation();
        SetupPlan plan = operation == SetupOperation.INSTALL
                ? coordinator.plan(value.options(), value.selection())
                : coordinator.plan(value.options(), value.selection(), operation);
        return new McpSetupPlanResult(plan, SetupPlanJson.write(plan), plan.digest());
    }

    @Tool(name = "setup_install",
            description = "installs one reviewed setup plan after exact digest and license approval")
    public SetupReceipt setupInstall(
            @ToolParam(description = "exact plan JSON returned by setup_plan") String planJson,
            @ToolParam(description = "exact plan digest explicitly approved by the caller") String approvedDigest,
            @ToolParam(required = false, description = "explicitly accepted license identifiers")
            List<String> acceptedLicenses,
            @ToolParam(description = "the same paths and execution policy used to create the plan")
            McpSetupRequest request) throws IOException {
        if (planJson == null || planJson.isBlank()) throw new IllegalArgumentException("planJson must not be blank.");
        SetupPlan plan = parsePlan(planJson);
        McpSetupRequest value = requireRequest(request);
        if (SetupOperation.fromPlan(plan) != value.setupOperation()) {
            throw new IllegalArgumentException("Setup plan operation does not match the request.");
        }
        SetupSelection selection = installSelection(plan, value);
        return coordinator.install(plan, new SetupApproval(approvedDigest, Instant.now(),
                new LinkedHashSet<>(acceptedLicenses == null ? List.of() : acceptedLicenses)),
                value.options(), selection);
    }

    @Tool(name = "setup_start",
            description = "reports whether the selected setup profile owns a startable managed service")
    public McpSetupLifecycleResult setupStart(@ToolParam(description = "setup profile") String profile) {
        return unsupported(profile, "start");
    }

    @Tool(name = "setup_stop",
            description = "reports whether the selected setup profile owns a stoppable managed service lease")
    public McpSetupLifecycleResult setupStop(@ToolParam(description = "setup profile") String profile) {
        return unsupported(profile, "stop");
    }

    @Tool(name = "setup_logs",
            description = "reports whether the selected setup profile exposes owned service logs")
    public McpSetupLifecycleResult setupLogs(@ToolParam(description = "setup profile") String profile) {
        return unsupported(profile, "logs");
    }

    private static McpSetupRequest requireRequest(McpSetupRequest request) {
        return Objects.requireNonNull(request, "request");
    }

    private static SetupPlan parsePlan(String planJson) {
        try {
            return SetupPlanJson.read(planJson);
        } catch (RuntimeException invalid) {
            throw new IllegalArgumentException("planJson is not a valid strict setup plan.", invalid);
        }
    }

    private static SetupSelection installSelection(SetupPlan plan, McpSetupRequest request) {
        if (plan.profile() != SetupProfile.OCR) return request.selection();
        List<String> fromPlan = plan.actions().stream().map(action -> {
            int separator = action.version().lastIndexOf(':');
            if (action.target() != SetupTarget.OCR_TESSDATA || separator < 0) {
                throw new IllegalArgumentException("OCR plan contains an unsupported action.");
            }
            return action.version().substring(separator + 1);
        }).toList();
        SetupSelection selected = new SetupSelection(fromPlan);
        if (!request.components().isEmpty() && !selected.equals(request.selection())) {
            throw new IllegalArgumentException("components do not match the reviewed OCR plan.");
        }
        return selected;
    }

    private static McpSetupLifecycleResult unsupported(String profile, String operation) {
        if (profile == null || profile.isBlank()) throw new IllegalArgumentException("profile must not be blank.");
        SetupProfile parsed;
        try {
            parsed = SetupProfile.valueOf(profile.trim().toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException invalid) {
            throw new IllegalArgumentException("Unsupported setup profile: " + profile, invalid);
        }
        return new McpSetupLifecycleResult(false, parsed.name(), operation,
                "Profile " + parsed + " does not currently own a " + operation + " lifecycle through MCP.");
    }
}
