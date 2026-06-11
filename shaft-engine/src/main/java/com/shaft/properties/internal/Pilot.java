package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration for optional SHAFT Pilot AI providers.
 *
 * <p>AI is disabled by default. Credentials are never configured here; providers
 * resolve them from the named environment variables only after the caller has
 * explicitly enabled AI and approved the relevant processing location.</p>
 */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/custom.properties",
        "file:src/main/resources/properties/default/custom.properties",
        "classpath:custom.properties"})
public interface Pilot extends EngineProperties<Pilot> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.pilotOverride.set(ConfigFactory.create(Pilot.class, ThreadLocalPropertiesManager.getOverrides()));
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    /** @return whether optional AI execution is enabled */
    @Key("pilot.ai.enabled")
    @DefaultValue("false")
    boolean enabled();

    /** @return selected provider identifier */
    @Key("pilot.ai.provider")
    @DefaultValue("none")
    String provider();

    /** @return whether local inference is approved */
    @Key("pilot.ai.consent.local")
    @DefaultValue("false")
    boolean localConsent();

    /** @return whether remote inference is approved */
    @Key("pilot.ai.consent.remote")
    @DefaultValue("false")
    boolean remoteConsent();

    /** @return comma-separated approved evidence categories */
    @Key("pilot.ai.allowedEvidenceCategories")
    @DefaultValue("")
    String allowedEvidenceCategories();

    /** @return whether optional external telemetry is enabled */
    @Key("pilot.ai.telemetry.enabled")
    @DefaultValue("false")
    boolean telemetryEnabled();

    /** @return maximum provider timeout in seconds */
    @Key("pilot.ai.timeoutSeconds")
    @DefaultValue("30")
    int timeoutSeconds();

    /** @return maximum serialized request size */
    @Key("pilot.ai.maxRequestBytes")
    @DefaultValue("1048576")
    int maxRequestBytes();

    /** @return maximum estimated input tokens */
    @Key("pilot.ai.maxInputTokens")
    @DefaultValue("16000")
    int maxInputTokens();

    /** @return maximum output tokens */
    @Key("pilot.ai.maxOutputTokens")
    @DefaultValue("2000")
    int maxOutputTokens();

    /** @return maximum accepted provider-reported cost in USD */
    @Key("pilot.ai.maxCostUsd")
    @DefaultValue("0")
    String maxCostUsd();

    /** @return maximum provider execution attempts */
    @Key("pilot.ai.retryMaxAttempts")
    @DefaultValue("2")
    int retryMaxAttempts();

    /** @return maximum concurrent calls per provider */
    @Key("pilot.ai.maxConcurrency")
    @DefaultValue("2")
    int maxConcurrency();

    /** @return failures required to open the circuit */
    @Key("pilot.ai.circuitBreaker.failureThreshold")
    @DefaultValue("3")
    int circuitBreakerFailureThreshold();

    /** @return circuit-open cooldown in seconds */
    @Key("pilot.ai.circuitBreaker.cooldownSeconds")
    @DefaultValue("60")
    int circuitBreakerCooldownSeconds();

    /** @return comma-separated CSS selectors to redact */
    @Key("pilot.ai.redaction.selectors")
    @DefaultValue("input[type=password],[autocomplete=current-password],[autocomplete=new-password]")
    String redactionSelectors();

    /** @return comma-separated structured or DOM attributes to redact */
    @Key("pilot.ai.redaction.attributes")
    @DefaultValue("authorization,cookie,set-cookie,password,passwd,secret,token,api-key,apikey,access-key")
    String redactionAttributes();

    /** @return double-semicolon-separated custom regular expressions */
    @Key("pilot.ai.redaction.patterns")
    @DefaultValue("")
    String redactionPatterns();

    /** @return OpenAI Responses endpoint */
    @Key("pilot.ai.openai.endpoint")
    @DefaultValue("https://api.openai.com/v1/responses")
    String openAiEndpoint();

    /** @return configured OpenAI model */
    @Key("pilot.ai.openai.model")
    @DefaultValue("")
    String openAiModel();

    /** @return environment variable containing the OpenAI credential */
    @Key("pilot.ai.openai.apiKeyEnvironmentVariable")
    @DefaultValue("OPENAI_API_KEY")
    String openAiApiKeyEnvironmentVariable();

    /** @return Anthropic Messages endpoint */
    @Key("pilot.ai.anthropic.endpoint")
    @DefaultValue("https://api.anthropic.com/v1/messages")
    String anthropicEndpoint();

    /** @return configured Anthropic model */
    @Key("pilot.ai.anthropic.model")
    @DefaultValue("")
    String anthropicModel();

    /** @return environment variable containing the Anthropic credential */
    @Key("pilot.ai.anthropic.apiKeyEnvironmentVariable")
    @DefaultValue("ANTHROPIC_API_KEY")
    String anthropicApiKeyEnvironmentVariable();

    /** @return Anthropic API contract version */
    @Key("pilot.ai.anthropic.version")
    @DefaultValue("2023-06-01")
    String anthropicVersion();

    /** @return Gemini models endpoint */
    @Key("pilot.ai.gemini.endpoint")
    @DefaultValue("https://generativelanguage.googleapis.com/v1beta/models")
    String geminiEndpoint();

    /** @return configured Gemini model */
    @Key("pilot.ai.gemini.model")
    @DefaultValue("")
    String geminiModel();

    /** @return environment variable containing the Gemini credential */
    @Key("pilot.ai.gemini.apiKeyEnvironmentVariable")
    @DefaultValue("GEMINI_API_KEY")
    String geminiApiKeyEnvironmentVariable();

    /** @return Ollama chat endpoint */
    @Key("pilot.ai.ollama.endpoint")
    @DefaultValue("http://127.0.0.1:11434/api/chat")
    String ollamaEndpoint();

    /** @return configured Ollama model */
    @Key("pilot.ai.ollama.model")
    @DefaultValue("")
    String ollamaModel();

    /**
     * Returns a fluent builder for current-thread overrides.
     *
     * @return current-thread property builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    /**
     * Fluent current-thread overrides for Pilot configuration.
     */
    class SetProperty implements EngineProperties.SetProperty {
        /** @param value enabled state @return this builder */
        public SetProperty enabled(boolean value) {
            setProperty("pilot.ai.enabled", String.valueOf(value));
            return this;
        }

        /** @param value provider identifier @return this builder */
        public SetProperty provider(String value) {
            setProperty("pilot.ai.provider", value);
            return this;
        }

        /** @param value local consent state @return this builder */
        public SetProperty localConsent(boolean value) {
            setProperty("pilot.ai.consent.local", String.valueOf(value));
            return this;
        }

        /** @param value remote consent state @return this builder */
        public SetProperty remoteConsent(boolean value) {
            setProperty("pilot.ai.consent.remote", String.valueOf(value));
            return this;
        }

        /** @param value approved category list @return this builder */
        public SetProperty allowedEvidenceCategories(String value) {
            setProperty("pilot.ai.allowedEvidenceCategories", value);
            return this;
        }

        /** @param value telemetry state @return this builder */
        public SetProperty telemetryEnabled(boolean value) {
            setProperty("pilot.ai.telemetry.enabled", String.valueOf(value));
            return this;
        }

        /** @param value timeout seconds @return this builder */
        public SetProperty timeoutSeconds(int value) {
            setProperty("pilot.ai.timeoutSeconds", String.valueOf(value));
            return this;
        }

        /** @param value maximum request bytes @return this builder */
        public SetProperty maxRequestBytes(int value) {
            setProperty("pilot.ai.maxRequestBytes", String.valueOf(value));
            return this;
        }

        /** @param value maximum input tokens @return this builder */
        public SetProperty maxInputTokens(int value) {
            setProperty("pilot.ai.maxInputTokens", String.valueOf(value));
            return this;
        }

        /** @param value maximum output tokens @return this builder */
        public SetProperty maxOutputTokens(int value) {
            setProperty("pilot.ai.maxOutputTokens", String.valueOf(value));
            return this;
        }

        /** @param value maximum cost in USD @return this builder */
        public SetProperty maxCostUsd(String value) {
            setProperty("pilot.ai.maxCostUsd", value);
            return this;
        }

        /** @param value maximum attempts @return this builder */
        public SetProperty retryMaxAttempts(int value) {
            setProperty("pilot.ai.retryMaxAttempts", String.valueOf(value));
            return this;
        }

        /** @param value maximum concurrency @return this builder */
        public SetProperty maxConcurrency(int value) {
            setProperty("pilot.ai.maxConcurrency", String.valueOf(value));
            return this;
        }

        /** @param value failure threshold @return this builder */
        public SetProperty circuitBreakerFailureThreshold(int value) {
            setProperty("pilot.ai.circuitBreaker.failureThreshold", String.valueOf(value));
            return this;
        }

        /** @param value cooldown seconds @return this builder */
        public SetProperty circuitBreakerCooldownSeconds(int value) {
            setProperty("pilot.ai.circuitBreaker.cooldownSeconds", String.valueOf(value));
            return this;
        }

        /** @param value selector list @return this builder */
        public SetProperty redactionSelectors(String value) {
            setProperty("pilot.ai.redaction.selectors", value);
            return this;
        }

        /** @param value attribute list @return this builder */
        public SetProperty redactionAttributes(String value) {
            setProperty("pilot.ai.redaction.attributes", value);
            return this;
        }

        /** @param value custom pattern list @return this builder */
        public SetProperty redactionPatterns(String value) {
            setProperty("pilot.ai.redaction.patterns", value);
            return this;
        }

        /** @param value endpoint URL @return this builder */
        public SetProperty openAiEndpoint(String value) {
            setProperty("pilot.ai.openai.endpoint", value);
            return this;
        }

        /** @param value model identifier @return this builder */
        public SetProperty openAiModel(String value) {
            setProperty("pilot.ai.openai.model", value);
            return this;
        }

        /** @param value environment variable name @return this builder */
        public SetProperty openAiApiKeyEnvironmentVariable(String value) {
            setProperty("pilot.ai.openai.apiKeyEnvironmentVariable", value);
            return this;
        }

        /** @param value endpoint URL @return this builder */
        public SetProperty anthropicEndpoint(String value) {
            setProperty("pilot.ai.anthropic.endpoint", value);
            return this;
        }

        /** @param value model identifier @return this builder */
        public SetProperty anthropicModel(String value) {
            setProperty("pilot.ai.anthropic.model", value);
            return this;
        }

        /** @param value environment variable name @return this builder */
        public SetProperty anthropicApiKeyEnvironmentVariable(String value) {
            setProperty("pilot.ai.anthropic.apiKeyEnvironmentVariable", value);
            return this;
        }

        /** @param value Anthropic API contract version @return this builder */
        public SetProperty anthropicVersion(String value) {
            setProperty("pilot.ai.anthropic.version", value);
            return this;
        }

        /** @param value base endpoint URL @return this builder */
        public SetProperty geminiEndpoint(String value) {
            setProperty("pilot.ai.gemini.endpoint", value);
            return this;
        }

        /** @param value model identifier @return this builder */
        public SetProperty geminiModel(String value) {
            setProperty("pilot.ai.gemini.model", value);
            return this;
        }

        /** @param value environment variable name @return this builder */
        public SetProperty geminiApiKeyEnvironmentVariable(String value) {
            setProperty("pilot.ai.gemini.apiKeyEnvironmentVariable", value);
            return this;
        }

        /** @param value endpoint URL @return this builder */
        public SetProperty ollamaEndpoint(String value) {
            setProperty("pilot.ai.ollama.endpoint", value);
            return this;
        }

        /** @param value model identifier @return this builder */
        public SetProperty ollamaModel(String value) {
            setProperty("pilot.ai.ollama.model", value);
            return this;
        }
    }
}
