package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.util.List;

import static com.shaft.mcp.EngineService.getDriver;

/**
 * MCP entry point for trust-gated SHAFT natural-language actions.
 */
@Service
public class NaturalActionService {
    private static final Logger logger = LoggerFactory.getLogger(NaturalActionService.class);

    /**
     * Performs a natural-language browser, element, or touch action through the active SHAFT driver.
     *
     * @param intent natural-language action intent
     * @param arguments optional sensitive action arguments, such as typed values
     * @param minimumTrustPercentage optional trust threshold from 0 to 100; uses current configuration when null
     * @param planner optional planner id: deterministic, auto, or a ServiceLoader planner id
     * @param aiFallbackEnabled optional provider fallback flag; uses current configuration when null
     * @param allowedActions optional comma-separated targets: browser, element, touch
     * @return redacted execution metadata
     */
    @Tool(name = "natural_act",
            description = "performs a trust-gated natural-language browser, element, or touch action on the active page")
    public McpNaturalActionResult act(
            String intent,
            List<String> arguments,
            Integer minimumTrustPercentage,
            String planner,
            Boolean aiFallbackEnabled,
            String allowedActions) {
        SHAFT.GUI.WebDriver driver = getDriver();
        NaturalActionSettings previous = NaturalActionSettings.current();
        NaturalActionSettings effective = previous.withOverrides(
                minimumTrustPercentage,
                planner,
                aiFallbackEnabled,
                allowedActions);
        List<String> safeArguments = arguments == null ? List.of() : List.copyOf(arguments);
        try {
            effective.applyEnabled();
            driver.act(intent, safeArguments.toArray(Object[]::new));
            logger.info(
                    "Natural action completed (intent length: {}, argument count: {}, threshold: {}, planner: {}, aiFallback: {})",
                    safeLength(intent),
                    safeArguments.size(),
                    effective.minimumTrustPercentage(),
                    effective.planner(),
                    effective.aiFallbackEnabled());
            return new McpNaturalActionResult(
                    safeLength(intent),
                    safeArguments.size(),
                    effective.minimumTrustPercentage(),
                    effective.planner(),
                    effective.aiFallbackEnabled(),
                    effective.allowedActions());
        } catch (Exception exception) {
            logger.error(
                    "Natural action failed (intent length: {}, argument count: {}, threshold: {}, planner: {}, aiFallback: {})",
                    safeLength(intent),
                    safeArguments.size(),
                    effective.minimumTrustPercentage(),
                    effective.planner(),
                    effective.aiFallbackEnabled(),
                    exception);
            throw exception;
        } finally {
            previous.apply();
        }
    }

    private static int safeLength(String value) {
        return value == null ? 0 : value.length();
    }

    private record NaturalActionSettings(
            boolean enabled,
            int minimumTrustPercentage,
            String planner,
            boolean aiFallbackEnabled,
            String allowedActions) {
        static NaturalActionSettings current() {
            return new NaturalActionSettings(
                    SHAFT.Properties.naturalActions.enabled(),
                    SHAFT.Properties.naturalActions.minimumTrustPercentage(),
                    SHAFT.Properties.naturalActions.planner(),
                    SHAFT.Properties.naturalActions.aiFallbackEnabled(),
                    SHAFT.Properties.naturalActions.allowedActions());
        }

        NaturalActionSettings withOverrides(
                Integer trustOverride,
                String plannerOverride,
                Boolean aiFallbackOverride,
                String allowedActionsOverride) {
            return new NaturalActionSettings(
                    true,
                    trustOverride == null ? minimumTrustPercentage : Math.max(0, Math.min(100, trustOverride)),
                    textOrDefault(plannerOverride, planner),
                    aiFallbackOverride == null ? aiFallbackEnabled : aiFallbackOverride,
                    textOrDefault(allowedActionsOverride, allowedActions));
        }

        void applyEnabled() {
            apply(true);
        }

        void apply() {
            apply(enabled);
        }

        private void apply(boolean effectiveEnabled) {
            SHAFT.Properties.naturalActions.set()
                    .enabled(effectiveEnabled)
                    .minimumTrustPercentage(minimumTrustPercentage)
                    .planner(planner)
                    .aiFallbackEnabled(aiFallbackEnabled)
                    .allowedActions(allowedActions);
        }

        private static String textOrDefault(String value, String defaultValue) {
            return value == null || value.isBlank() ? defaultValue : value.trim();
        }
    }
}
