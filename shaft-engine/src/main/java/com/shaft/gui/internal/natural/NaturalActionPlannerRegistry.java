package com.shaft.gui.internal.natural;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;

import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.ServiceLoader;

final class NaturalActionPlannerRegistry {
    private static final NaturalActionPlanner DETERMINISTIC = new DeterministicNaturalActionPlanner();
    private static volatile List<NaturalActionPlanner> cachedPlanners;

    private NaturalActionPlannerRegistry() {
        throw new IllegalStateException("Utility class");
    }

    static NaturalActionPlan plan(NaturalActionRequest request) {
        String configuredPlanner = normalize(SHAFT.Properties.naturalActions.planner());
        if (configuredPlanner.isBlank() || "deterministic".equals(configuredPlanner)) {
            return DETERMINISTIC.plan(request);
        }

        List<NaturalActionPlanner> planners = planners();
        if ("auto".equals(configuredPlanner)) {
            NaturalActionPlan deterministic = DETERMINISTIC.plan(request);
            if (!deterministic.steps().isEmpty() || !SHAFT.Properties.naturalActions.aiFallbackEnabled()) {
                return deterministic;
            }
            return planners.stream()
                    .filter(planner -> !"deterministic".equals(planner.id()))
                    .filter(planner -> planner.supports(request))
                    .map(planner -> safePlan(planner, request))
                    .filter(plan -> !plan.steps().isEmpty())
                    .findFirst()
                    .orElse(deterministic);
        }

        return planners.stream()
                .filter(planner -> configuredPlanner.equals(normalize(planner.id())))
                .findFirst()
                .map(planner -> safePlan(planner, request))
                .orElseGet(() -> {
                    ReportManager.logDiscrete("Natural action planner \"" + configuredPlanner
                            + "\" was requested but no matching provider was found.");
                    return NaturalActionPlan.unsupported(
                            configuredPlanner,
                            request.intent(),
                            "Requested natural-action planner is unavailable.");
                });
    }

    private static NaturalActionPlan safePlan(NaturalActionPlanner planner, NaturalActionRequest request) {
        try {
            if (!planner.supports(request)) {
                return NaturalActionPlan.unsupported(planner.id(), request.intent(), "Planner does not support request.");
            }
            NaturalActionPlan plan = planner.plan(request);
            return plan == null
                    ? NaturalActionPlan.unsupported(
                            planner.id(),
                            request.intent(),
                            "Planner returned no plan; preserving deterministic engine behavior.")
                    : plan;
        } catch (RuntimeException exception) {
            ReportManagerHelper.logDiscrete(exception);
            return NaturalActionPlan.unsupported(
                    planner.id(),
                    request.intent(),
                    "Planner failed; preserving deterministic engine behavior.");
        }
    }

    private static List<NaturalActionPlanner> planners() {
        List<NaturalActionPlanner> planners = cachedPlanners;
        if (planners == null) {
            synchronized (NaturalActionPlannerRegistry.class) {
                planners = cachedPlanners;
                if (planners == null) {
                    planners = ServiceLoader.load(NaturalActionPlanner.class)
                            .stream()
                            .map(ServiceLoader.Provider::get)
                            .filter(planner -> !"deterministic".equals(normalize(planner.id())))
                            .sorted(Comparator
                                    .comparingInt(NaturalActionPlanner::priority)
                                    .reversed()
                                    .thenComparing(NaturalActionPlanner::id))
                            .toList();
                    cachedPlanners = planners;
                }
            }
        }
        return planners;
    }

    private static String normalize(String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    }
}
