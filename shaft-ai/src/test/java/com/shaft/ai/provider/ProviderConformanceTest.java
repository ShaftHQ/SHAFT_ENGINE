package com.shaft.ai.provider;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import com.shaft.driver.SHAFT;
import com.shaft.doctor.DoctorAiAnalysisRequest;
import com.shaft.doctor.ai.DoctorAiAnalysisService;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.DoctorAdvisory;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceItem;
import com.shaft.doctor.model.EvidenceProvenance;
import com.shaft.doctor.model.Finding;
import com.shaft.doctor.model.RedactionSummary;
import com.shaft.pilot.ai.AiBudget;
import com.shaft.pilot.ai.AiExecutionService;
import com.shaft.pilot.ai.AiProvider;
import com.shaft.pilot.ai.AiProviderRegistry;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;
import com.shaft.pilot.ai.ProcessingLocation;
import com.shaft.pilot.config.PilotConfiguration;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.http.HttpClient;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ProviderConformanceTest {
    private static final ObjectMapper JSON = new ObjectMapper();
    @TempDir
    Path temp;
    private HttpServer server;

    @AfterEach
    void cleanup() {
        if (server != null) {
            server.stop(0);
        }
        SHAFT.Properties.clearForCurrentThread();
    }

    @ParameterizedTest(name = "{0} returns the same structured contract")
    @MethodSource("providerIds")
    void allProvidersPassStructuredMockConformance(String providerId) throws Exception {
        AtomicReference<String> capturedBody = new AtomicReference<>("");
        AtomicReference<String> capturedCredential = new AtomicReference<>("");
        server = server(exchange -> {
            capturedBody.set(readBody(exchange));
            capturedCredential.set(switch (providerId) {
                case "openai" -> exchange.getRequestHeaders().getFirst("Authorization");
                case "anthropic" -> exchange.getRequestHeaders().getFirst("x-api-key");
                case "gemini" -> exchange.getRequestHeaders().getFirst("x-goog-api-key");
                default -> "";
            });
            respond(exchange, 200, successfulResponse(providerId, "{\"answer\":\"ok\"}"));
        });
        configure(providerId, server.getAddress().getPort());
        AiResponse response = execute(providerId, request(providerId, Duration.ofSeconds(1)));

        assertEquals(AiResponseStatus.SUCCESS, response.status());
        assertEquals("ok", response.structuredPayload().path("answer").asText());
        assertFalse(capturedBody.get().contains("do-not-transmit"));
        assertFalse(capturedBody.get().contains("test-credential"));
        assertTrue(capturedBody.get().contains("[REDACTED]"));
        assertTrue(capturedBody.get().contains("answer"));
        if (!"ollama".equals(providerId)) {
            assertTrue(capturedCredential.get().contains("test-credential"));
        }
    }

    @ParameterizedTest(name = "{0} accepts the Doctor advisory contract")
    @MethodSource("providerIds")
    void allProvidersPassDoctorAdvisoryConformance(String providerId) throws Exception {
        AtomicReference<String> capturedBody = new AtomicReference<>("");
        server = server(exchange -> {
            capturedBody.set(readBody(exchange));
            respond(exchange, 200, successfulResponse(providerId, doctorPayload()));
        });
        configure(providerId, server.getAddress().getPort());
        boolean local = "ollama".equals(providerId);
        ApprovalPolicy approval = new ApprovalPolicy(local, !local, EnumSet.allOf(EvidenceCategory.class));
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(provider(providerId));
        try {
            DoctorAiAnalysisService service = new DoctorAiAnalysisService(
                    request -> new AiExecutionService(
                            registry,
                            PilotConfiguration::current,
                            event -> {
                            }).execute(request),
                    PilotConfiguration::current);

            DoctorAdvisory advisory = service.analyze(
                    doctorBundle(),
                    doctorDiagnosis(),
                    DoctorAiAnalysisRequest.defaults(approval),
                    temp.resolve(providerId));

            assertEquals(DoctorAdvisory.Status.SUCCESS, advisory.status());
            assertEquals(providerId, advisory.metadata().provider());
            assertEquals(List.of("evidence-1"),
                    advisory.analysis().observations().getFirst().evidenceIds());
            assertTrue(capturedBody.get().contains("schemaVersion"));
            assertTrue(capturedBody.get().contains("evidence-1"));
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @ParameterizedTest(name = "{0} normalizes {1}")
    @MethodSource("providerFailures")
    void allProvidersNormalizeFailures(String providerId, FailureCase failureCase,
                                       AiResponseStatus expectedStatus) throws Exception {
        server = server(exchange -> {
            if (failureCase == FailureCase.TIMEOUT) {
                try {
                    Thread.sleep(200);
                } catch (InterruptedException exception) {
                    Thread.currentThread().interrupt();
                }
                respondQuietly(exchange, 200, successfulResponse(providerId, "{\"answer\":\"late\"}"));
            } else if (failureCase == FailureCase.MALFORMED_JSON) {
                respond(exchange, 200, "{not-json");
            } else if (failureCase == FailureCase.SCHEMA_VIOLATION) {
                respond(exchange, 200, successfulResponse(providerId, "{\"wrong\":true}"));
            } else if (failureCase == FailureCase.RATE_LIMIT) {
                respond(exchange, 429, "{}");
            } else {
                respond(exchange, 401, "{}");
            }
        });
        configure(providerId, server.getAddress().getPort());
        Duration timeout = failureCase == FailureCase.TIMEOUT ? Duration.ofMillis(50) : Duration.ofSeconds(1);

        AiResponse response = execute(providerId, request(providerId, timeout));

        assertEquals(expectedStatus, response.status());
        assertEquals("deterministic", response.structuredPayload().path("answer").asText());
        assertFalse(response.fallbackReason().contains("do-not-transmit"));
        assertFalse(response.fallbackReason().contains("test-credential"));
    }

    @Test
    void serviceLoaderDiscoversAllDirectProviders() {
        var ids = new AiProviderRegistry().serviceProviders().stream().map(AiProvider::id).toList();

        assertEquals(java.util.List.of("anthropic", "gemini", "ollama", "openai"), ids);
    }

    @Test
    void securedOnPremOllamaUsesOnlyNamedEnvironmentCredential() throws Exception {
        AtomicReference<String> authorization = new AtomicReference<>("");
        server = server(exchange -> {
            authorization.set(exchange.getRequestHeaders().getFirst("Authorization"));
            respond(exchange, 200, successfulResponse("ollama", "{\"answer\":\"ok\"}"));
        });
        SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider("ollama")
                .localConsent(false)
                .onPremConsent(true)
                .remoteConsent(false)
                .allowedEvidenceCategories("TEXT")
                .retryMaxAttempts(1)
                .ollamaEndpoint("http://127.0.0.1:" + server.getAddress().getPort() + "/api/chat")
                .ollamaModel("test-model")
                .ollamaProcessingLocation("on-prem")
                .ollamaApiKeyEnvironmentVariable("OLLAMA_GATEWAY_TOKEN")
                .ollamaApiKeyHeader("Authorization")
                .ollamaApiKeyPrefix("Bearer ");
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(new OllamaProvider(
                HttpClient.newHttpClient(),
                name -> "OLLAMA_GATEWAY_TOKEN".equals(name) ? "gateway-secret" : ""));
        try {
            ApprovalPolicy approval = new ApprovalPolicy(
                    false, true, false, EnumSet.of(EvidenceCategory.TEXT));
            AiResponse response = new AiExecutionService(
                    registry, PilotConfiguration::current, event -> {
            }).execute(requestWithApproval(approval));

            assertEquals(AiResponseStatus.SUCCESS, response.status());
            assertEquals("Bearer gateway-secret", authorization.get());
            assertFalse(response.toString().contains("gateway-secret"));
        } finally {
            registry.clearForCurrentThread();
        }
    }

    private AiResponse execute(String providerId, AiRequest request) {
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(provider(providerId));
        try {
            return new AiExecutionService(registry, com.shaft.pilot.config.PilotConfiguration::current,
                    event -> {
                    }).execute(request);
        } finally {
            registry.clearForCurrentThread();
        }
    }

    private static AiProvider provider(String providerId) {
        HttpClient client = HttpClient.newBuilder().build();
        return switch (providerId) {
            case "openai" -> new OpenAiProvider(client, ignored -> "test-credential");
            case "anthropic" -> new AnthropicProvider(client, ignored -> "test-credential");
            case "gemini" -> new GeminiProvider(client, ignored -> "test-credential");
            case "ollama" -> new OllamaProvider(client, ignored -> "");
            default -> throw new IllegalArgumentException("Unknown provider: " + providerId);
        };
    }

    private static AiRequest request(String providerId, Duration timeout) {
        JsonNode schema = JSON.createObjectNode()
                .put("type", "object")
                .set("properties", JSON.createObjectNode()
                        .set("answer", JSON.createObjectNode().put("type", "string")));
        ((com.fasterxml.jackson.databind.node.ObjectNode) schema).putArray("required").add("answer");
        boolean local = "ollama".equals(providerId);
        ApprovalPolicy approval = new ApprovalPolicy(local, !local, EnumSet.allOf(EvidenceCategory.class));
        return AiRequest.builder("provider-conformance", schema)
                .text("Authorization: Bearer do-not-transmit")
                .budget(new AiBudget(1_000, 100, java.math.BigDecimal.ONE))
                .approvalPolicy(approval)
                .timeout(timeout)
                .deterministicFallback(JSON.createObjectNode().put("answer", "deterministic"))
                .build();
    }

    private static AiRequest requestWithApproval(ApprovalPolicy approval) {
        JsonNode schema = JSON.createObjectNode()
                .put("type", "object")
                .set("properties", JSON.createObjectNode()
                        .set("answer", JSON.createObjectNode().put("type", "string")));
        ((com.fasterxml.jackson.databind.node.ObjectNode) schema).putArray("required").add("answer");
        return AiRequest.builder("provider-conformance", schema)
                .text("bounded evidence")
                .budget(new AiBudget(1_000, 100, java.math.BigDecimal.ONE))
                .approvalPolicy(approval)
                .timeout(Duration.ofSeconds(1))
                .deterministicFallback(JSON.createObjectNode().put("answer", "deterministic"))
                .build();
    }

    private static void configure(String providerId, int port) {
        boolean local = "ollama".equals(providerId);
        var properties = SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider(providerId)
                .localConsent(local)
                .remoteConsent(!local)
                .allowedEvidenceCategories("TEXT,LOG,CONFIGURATION,DOM")
                .retryMaxAttempts(1)
                .timeoutSeconds(1);
        String base = "http://127.0.0.1:" + port;
        switch (providerId) {
            case "openai" -> properties.openAiEndpoint(base + "/responses").openAiModel("test-model");
            case "anthropic" -> properties.anthropicEndpoint(base + "/messages").anthropicModel("test-model");
            case "gemini" -> properties.geminiEndpoint(base + "/v1beta/models").geminiModel("test-model");
            case "ollama" -> properties.ollamaEndpoint(base + "/api/chat").ollamaModel("test-model");
            default -> throw new IllegalArgumentException("Unknown provider: " + providerId);
        }
    }

    private static Stream<String> providerIds() {
        return Stream.of("openai", "anthropic", "gemini", "ollama");
    }

    private static Stream<Arguments> providerFailures() {
        return providerIds().flatMap(providerId -> Stream.of(
                Arguments.of(providerId, FailureCase.AUTHENTICATION, AiResponseStatus.AUTHENTICATION_FAILED),
                Arguments.of(providerId, FailureCase.RATE_LIMIT, AiResponseStatus.RATE_LIMITED),
                Arguments.of(providerId, FailureCase.TIMEOUT, AiResponseStatus.TIMEOUT),
                Arguments.of(providerId, FailureCase.MALFORMED_JSON, AiResponseStatus.INVALID_RESPONSE),
                Arguments.of(providerId, FailureCase.SCHEMA_VIOLATION, AiResponseStatus.INVALID_RESPONSE)));
    }

    private static String doctorPayload() throws JsonProcessingException {
        return JSON.writeValueAsString(Map.of(
                "schemaVersion", "1.0",
                "observations", List.of(Map.of(
                        "statement", "The submitted exception reports a missing element.",
                        "evidenceIds", List.of("evidence-1"))),
                "hypotheses", List.of(Map.of(
                        "causeCategory", "LOCATOR",
                        "statement", "The locator likely no longer matches the intended element.",
                        "confidence", "HIGH",
                        "evidenceIds", List.of("evidence-1"))),
                "missingEvidence", List.of("Current DOM snapshot"),
                "recommendedActions", List.of(Map.of(
                        "title", "Inspect locator",
                        "action", "Compare the locator with the current DOM.",
                        "evidenceIds", List.of("evidence-1"))),
                "limitations", List.of("Only submitted evidence was analyzed.")));
    }

    private static EvidenceBundle doctorBundle() {
        return new EvidenceBundle(
                EvidenceBundle.CURRENT_SCHEMA_VERSION,
                "bundle-1",
                List.of(new EvidenceItem(
                        "evidence-1",
                        com.shaft.doctor.model.EvidenceCategory.EXCEPTION_CHAIN,
                        "text/plain",
                        "",
                        "sha-1",
                        50,
                        "NoSuchElementException: unable to locate element",
                        true,
                        false,
                        Map.of(),
                        new EvidenceProvenance("fixture", "fixture/result.json", "sha-1"))),
                new RedactionSummary(List.of(), List.of(), 0),
                Map.of());
    }

    private static Diagnosis doctorDiagnosis() {
        return new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                CauseCategory.LOCATOR,
                List.of(),
                Confidence.HIGH,
                "Locator did not resolve an element.",
                "The deterministic locator rule matched submitted evidence.",
                List.of(new Finding(
                        "finding-1",
                        Finding.Kind.INFERENCE,
                        CauseCategory.LOCATOR,
                        Finding.Severity.ERROR,
                        "Locator not found",
                        "The locator did not resolve an element.",
                        "locator-not-found",
                        List.of("evidence-1"))),
                List.of(),
                List.of("Current DOM snapshot"));
    }

    private static HttpServer server(ExchangeHandler handler) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/", exchange -> handler.handle(exchange));
        server.start();
        return server;
    }

    private static String successfulResponse(String providerId, String payload) {
        String escaped = payload.replace("\\", "\\\\").replace("\"", "\\\"");
        return switch (providerId) {
            case "openai" -> "{\"model\":\"test-model\",\"output\":[{\"content\":[{\"type\":\"output_text\","
                    + "\"text\":\"" + escaped + "\"}]}],\"usage\":{\"input_tokens\":3,\"output_tokens\":2}}";
            case "anthropic" -> "{\"model\":\"test-model\",\"content\":[{\"type\":\"text\",\"text\":\""
                    + escaped + "\"}],\"usage\":{\"input_tokens\":3,\"output_tokens\":2}}";
            case "gemini" -> "{\"modelVersion\":\"test-model\",\"candidates\":[{\"content\":{\"parts\":[{\"text\":\""
                    + escaped + "\"}]}}],\"usageMetadata\":{\"promptTokenCount\":3,\"candidatesTokenCount\":2}}";
            case "ollama" -> "{\"model\":\"test-model\",\"message\":{\"role\":\"assistant\",\"content\":\""
                    + escaped + "\"},\"prompt_eval_count\":3,\"eval_count\":2}";
            default -> throw new IllegalArgumentException("Unknown provider: " + providerId);
        };
    }

    private static String readBody(HttpExchange exchange) throws IOException {
        return new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
    }

    private static void respond(HttpExchange exchange, int status, String body) throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(status, bytes.length);
        exchange.getResponseBody().write(bytes);
        exchange.close();
    }

    private static void respondQuietly(HttpExchange exchange, int status, String body) {
        try {
            respond(exchange, status, body);
        } catch (IOException ignored) {
            exchange.close();
        }
    }

    @FunctionalInterface
    private interface ExchangeHandler {
        void handle(HttpExchange exchange) throws IOException;
    }

    private enum FailureCase {
        AUTHENTICATION,
        RATE_LIMIT,
        TIMEOUT,
        MALFORMED_JSON,
        SCHEMA_VIOLATION
    }
}
