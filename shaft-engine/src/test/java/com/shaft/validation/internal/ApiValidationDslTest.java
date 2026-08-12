package com.shaft.validation.internal;

import com.shaft.validation.Validations;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.tools.io.ReportManager;
import io.qameta.allure.Allure;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.model.StepResult;
import io.restassured.response.Response;
import io.restassured.response.ResponseBody;
import io.restassured.builder.ResponseBuilder;
import org.mockito.Mockito;
import org.mockito.MockedStatic;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import javax.tools.ToolProvider;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Set;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class ApiValidationDslTest {
    private static final Set<String> FOCUSED_STARTERS = Set.of(
            "statusCodeValue", "headerValue", "cookieValue", "bodyValue",
            "jsonValue", "jsonValues", "responseTimeMillis", "matchesContract");

    @Test
    public void focusedApiVocabularyShouldBeDiscoverableFromTheExistingResponseRoot() throws Exception {
        Set<String> methods = Arrays.stream(RestValidationsBuilder.class.getMethods())
                .map(Method::getName)
                .collect(Collectors.toSet());

        Assert.assertTrue(methods.containsAll(FOCUSED_STARTERS),
                "Missing focused API validation starters: " + FOCUSED_STARTERS.stream()
                        .filter(method -> !methods.contains(method)).sorted().toList());
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("statusCodeValue").getReturnType(),
                NumberValidationsBuilder.class);
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("headerValue", String.class).getReturnType(),
                NativeValidationsBuilder.class);
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("cookieValue", String.class).getReturnType(),
                NativeValidationsBuilder.class);
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("bodyValue").getReturnType(),
                NativeValidationsBuilder.class);
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("jsonValue", String.class).getReturnType(),
                NativeValidationsBuilder.class);
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("jsonValues", String.class).getReturnType(),
                NativeValidationsBuilder.class);
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("responseTimeMillis").getReturnType(),
                NumberValidationsBuilder.class);
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("matchesContract", String.class).getReturnType(),
                ValidationsExecutor.class);
    }

    @AfterMethod(alwaysRun = true)
    public void resetSoftAssertions() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test
    public void focusedScalarStartersShouldReadTheCompletedResponseForHardAndSoftAssertions() throws Exception {
        assertVocabularyPresent();
        Response response = Mockito.mock(Response.class);
        ResponseBody<?> responseBody = Mockito.mock(ResponseBody.class);
        Mockito.when(response.statusCode()).thenReturn(201);
        Mockito.when(response.getHeader("X-Trace")).thenReturn("trace-value");
        Mockito.when(response.getCookie("session")).thenReturn("cookie-value");
        Mockito.when(response.getBody()).thenReturn(responseBody);
        Mockito.when(responseBody.asString()).thenReturn("{\"id\":7,\"items\":[1,2]}");
        Mockito.when(response.asPrettyString()).thenReturn("{\"id\":7,\"items\":[1,2]}");
        Mockito.when(response.timeIn(TimeUnit.MILLISECONDS)).thenReturn(42L);

        try (MockedStatic<JavaScriptWaitManager> waits = Mockito.mockStatic(JavaScriptWaitManager.class)) {
            ((NumberValidationsBuilder) RestValidationsBuilder.class.getMethod("statusCodeValue")
                    .invoke(Validations.assertThat().response(response))).isEqualTo(201).perform();
            invokeNative(Validations.assertThat().response(response), "headerValue", "X-Trace")
                    .isEqualTo("trace-value").perform();
            invokeNative(Validations.verifyThat().response(response), "cookieValue", "session")
                    .isEqualTo("cookie-value").perform();
            invokeNative(Validations.assertThat().response(response), "bodyValue")
                    .contains("\"id\":7").perform();
            invokeNative(Validations.assertThat().response(response), "jsonValue", "id").isEqualTo("7").perform();
            invokeNative(Validations.verifyThat().response(response), "jsonValues", "items")
                    .isEqualTo(List.of(1, 2)).perform();
            ((NumberValidationsBuilder) RestValidationsBuilder.class.getMethod("responseTimeMillis")
                    .invoke(Validations.assertThat().response(response))).isEqualTo(42).perform();
            waits.verifyNoInteractions();
        }
        verify(response).statusCode();
        verify(response).getHeader("X-Trace");
        verify(response).getCookie("session");
        verify(response).getBody();
        verify(response, times(2)).asPrettyString();
        verify(response).timeIn(TimeUnit.MILLISECONDS);
        Mockito.verifyNoMoreInteractions(response);
        verify(responseBody).asString();
        Mockito.verifyNoMoreInteractions(responseBody);
    }

    @Test
    public void focusedStartersShouldKeepLegacyAliasesAndNativeResponseAccess() throws Exception {
        assertVocabularyPresent();
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("body").getReturnType(), JSONValidationsBuilder.class);
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("time").getReturnType(), NumberValidationsBuilder.class);
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("extractedJsonValue", String.class).getReturnType(),
                NativeValidationsBuilder.class);
        Assert.assertEquals(RestValidationsBuilder.class.getConstructor(
                com.shaft.validation.ValidationEnums.ValidationCategory.class, Object.class, StringBuilder.class)
                .getParameterCount(), 3);
    }

    @Test
    public void frozenLegacyRestBuilderBytecodeShouldLinkAgainstTheFocusedDsl() throws Exception {
        Path classes = Files.createTempDirectory("shaft-old-rest-validations-");
        try {
            Path packageDirectory = Files.createDirectories(classes.resolve("com/shaft/validation/internal"));
            Files.writeString(packageDirectory.resolve("RestValidationsBuilder.java"), """
                    package com.shaft.validation.internal;
                    import com.shaft.validation.ValidationEnums;
                    public class RestValidationsBuilder {
                        public RestValidationsBuilder(ValidationEnums.ValidationCategory category,
                                Object response, StringBuilder message) {}
                        public JSONValidationsBuilder body() { return null; }
                        public NumberValidationsBuilder time() { return null; }
                        public NativeValidationsBuilder extractedJsonValue(String path) { return null; }
                        public NativeValidationsBuilder extractedJsonValueAsList(String path) { return null; }
                    }
                    """, StandardCharsets.UTF_8);
            Files.writeString(packageDirectory.resolve("OldRestValidationConsumer.java"), """
                    package com.shaft.validation.internal;
                    public final class OldRestValidationConsumer {
                        public static Object[] invoke(RestValidationsBuilder builder) {
                            return new Object[] { builder.body(), builder.time(),
                                    builder.extractedJsonValue("id"),
                                    builder.extractedJsonValueAsList("items") };
                        }
                    }
                    """, StandardCharsets.UTF_8);
            int result = ToolProvider.getSystemJavaCompiler().run(null, null, null,
                    "-classpath", System.getProperty("java.class.path"), "-d", classes.toString(),
                    packageDirectory.resolve("RestValidationsBuilder.java").toString(),
                    packageDirectory.resolve("OldRestValidationConsumer.java").toString());
            Assert.assertEquals(result, 0, "Frozen legacy response consumer must compile.");

            try (URLClassLoader loader = new URLClassLoader(new java.net.URL[]{classes.toUri().toURL()},
                    RestValidationsBuilder.class.getClassLoader())) {
                Class<?> consumer = Class.forName(
                        "com.shaft.validation.internal.OldRestValidationConsumer", true, loader);
                Response response = Mockito.mock(Response.class);
                RestValidationsBuilder builder = Validations.assertThat().response(response);
                Object[] values = (Object[]) consumer.getMethod("invoke", RestValidationsBuilder.class)
                        .invoke(null, builder);
                Assert.assertEquals(values.length, 4);
                Mockito.verifyNoInteractions(response);
            }
        } finally {
            try (var paths = Files.walk(classes)) {
                paths.sorted(Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (java.io.IOException exception) {
                        throw new IllegalStateException("Could not clean frozen consumer fixture.", exception);
                    }
                });
            }
        }
    }

    @Test
    public void focusedResponseStartersShouldHaveExplicitSourceCollisionBehavior() throws Exception {
        Assert.assertEquals(compileCurrentRestSubclass("CompatibleRestBuilder", """
                @Override public NumberValidationsBuilder statusCodeValue() { return super.statusCodeValue(); }
                @Override public NativeValidationsBuilder headerValue(String name) { return super.headerValue(name); }
                """), 0, "Compatible focused-value overrides must compile.");
        Assert.assertNotEquals(compileCurrentRestSubclass("IncompatibleStatusRestBuilder", """
                public String statusCodeValue() { return "legacy"; }
                """), 0, "An incompatible zero-argument return collision must fail compilation.");
        Assert.assertNotEquals(compileCurrentRestSubclass("IncompatibleHeaderRestBuilder", """
                public String headerValue(String name) { return "legacy"; }
                """), 0, "An incompatible parameterized return collision must fail compilation.");
    }

    @Test
    public void focusedApiValuesShouldResolveOnlyAtTheTerminalAndPreserveProviderFailures() {
        Response response = Mockito.mock(Response.class);
        RuntimeException sentinel = new IllegalStateException("provider sentinel");
        NativeValidationsBuilder retained = Validations.assertThat().response(response).headerValue("X-Late");
        verify(response, never()).getHeader("X-Late");
        Mockito.when(response.getHeader("X-Late")).thenThrow(sentinel);

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> retained.isEqualTo("value"));
        Assert.assertSame(thrown, sentinel);
    }

    @Test
    public void commonWebMobileAndApiPhrasesShouldStayWithinThreeSemanticSteps() throws Exception {
        Assert.assertEquals(com.shaft.gui.driver.DriverAssertions.class.getMethod("browser").getReturnType(),
                com.shaft.gui.driver.BrowserAssertions.class);
        Assert.assertEquals(com.shaft.gui.driver.DriverAssertions.class.getMethod("mobileValues").getReturnType(),
                com.shaft.gui.driver.MobileAssertions.class);
        Assert.assertEquals(ValidationsBuilder.class.getMethod("response", Object.class).getReturnType(),
                RestValidationsBuilder.class);
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("matchesContract", String.class).getReturnType(),
                ValidationsExecutor.class);
        Assert.assertEquals(RestValidationsBuilder.class.getMethod("matchesSchema", String.class).getReturnType(),
                ValidationsExecutor.class);
    }

    @Test
    public void focusedApiStartersShouldPreserveHardAndSoftFailureSemantics() {
        Response response = response();
        Response contractResponse = new ResponseBuilder().setStatusCode(200).setBody("{\"unexpected\":true}").build();
        assertHardFails(() -> Validations.assertThat().response(response).statusCodeValue().isEqualTo(500));
        assertHardFails(() -> Validations.assertThat().response(response).headerValue("X-Trace").isEqualTo("wrong"));
        assertHardFails(() -> Validations.assertThat().response(response).cookieValue("session").isEqualTo("wrong"));
        assertHardFails(() -> Validations.assertThat().response(response).bodyValue().contains("missing"));
        assertHardFails(() -> Validations.assertThat().response(response).jsonValue("id").isEqualTo("8"));
        assertHardFails(() -> Validations.assertThat().response(response).jsonValues("items").isEqualTo(List.of(9)));
        assertHardFails(() -> Validations.assertThat().response(response).responseTimeMillis().isEqualTo(99));
        assertHardFails(() -> Validations.assertThat().response(contractResponse)
                .matchesContract("jsonFileManagerTestData.json"));

        assertSoftFails(() -> Validations.verifyThat().response(response).statusCodeValue().isEqualTo(500));
        assertSoftFails(() -> Validations.verifyThat().response(response).headerValue("X-Trace").isEqualTo("wrong"));
        assertSoftFails(() -> Validations.verifyThat().response(response).cookieValue("session").isEqualTo("wrong"));
        assertSoftFails(() -> Validations.verifyThat().response(response).bodyValue().contains("missing"));
        assertSoftFails(() -> Validations.verifyThat().response(response).jsonValue("id").isEqualTo("8"));
        assertSoftFails(() -> Validations.verifyThat().response(response).jsonValues("items").isEqualTo(List.of(9)));
        assertSoftFails(() -> Validations.verifyThat().response(response).responseTimeMillis().isEqualTo(99));
        assertSoftFails(() -> Validations.verifyThat().response(contractResponse)
                .matchesContract("jsonFileManagerTestData.json"));
    }

    @Test
    public void contractAliasShouldBeOrderInsensitiveAndCoexistWithSchemaValidation() {
        Response response = new ResponseBuilder().setStatusCode(200).setBody("""
                {"body":"quia et suscipit\\nsuscipit recusandae consequuntur expedita et cum\\nreprehenderit molestiae ut ut quas totam\\nnostrum rerum est autem sunt rem eveniet architecto",
                 "title":"sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
                 "id":1,"userId":1}
                """).build();
        try (MockedStatic<JavaScriptWaitManager> waits = Mockito.mockStatic(JavaScriptWaitManager.class)) {
            Validations.assertThat().response(response).matchesContract("post1Response.json");
            Validations.assertThat().response(response).matchesSchema("post1ResponseSchema.json");
            assertHardFails(() -> Validations.assertThat().response(response)
                    .matchesContract("jsonFileManagerTestData.json"));
            Assert.expectThrows(RuntimeException.class,
                    () -> Validations.assertThat().response(response).matchesContract("missing-contract.json"));
            waits.verifyNoInteractions();
        }
    }

    @Test
    public void contractAliasShouldTreatDifferentAndScalarRootShapesAsValidationOutcomes() throws Exception {
        Response arrayResponse = new ResponseBuilder().setStatusCode(200).setBody("[1,2]").build();
        assertHardFails(() -> Validations.assertThat().response(arrayResponse)
                .matchesContract("jsonFileManagerTestData.json"));
        assertSoftFails(() -> Validations.verifyThat().response(arrayResponse)
                .matchesContract("jsonFileManagerTestData.json"));

        Path scalar = Files.createTempFile("shaft-api-contract-", ".json");
        try {
            Files.writeString(scalar, "42", StandardCharsets.UTF_8);
            Response matchingScalar = new ResponseBuilder().setStatusCode(200).setBody("42").build();
            Validations.assertThat().response(matchingScalar).matchesContract(scalar.toString());
            Response mismatchingScalar = new ResponseBuilder().setStatusCode(200).setBody("43").build();
            assertHardFails(() -> Validations.assertThat().response(mismatchingScalar)
                    .matchesContract(scalar.toString()));
            assertSoftFails(() -> Validations.verifyThat().response(mismatchingScalar)
                    .matchesContract(scalar.toString()));
        } finally {
            Files.deleteIfExists(scalar);
        }
    }

    @Test
    public void apiValueReportingShouldNeverPublishComparedPayloads() {
        String actualSecret = "actual-api-secret-7491";
        String expectedSecret = "expected-api-secret-3852";
        Response response = Mockito.mock(Response.class);
        ResponseBody<?> body = Mockito.mock(ResponseBody.class);
        Mockito.when(response.getHeader("Authorization")).thenReturn(actualSecret);
        Mockito.when(response.getCookie("session")).thenReturn(actualSecret);
        Mockito.when(response.getBody()).thenReturn(body);
        Mockito.when(body.asString()).thenReturn(actualSecret);
        Mockito.when(response.asPrettyString()).thenReturn(
                "{\"secret\":\"" + actualSecret + "\",\"items\":[\"" + actualSecret + "\"]}");
        try (MockedStatic<Allure> allure = Mockito.mockStatic(Allure.class);
             MockedStatic<ReportManager> report = Mockito.mockStatic(ReportManager.class)) {
            AllureLifecycle lifecycle = Mockito.mock(AllureLifecycle.class);
            StepResult step = captureStepUpdates(lifecycle);
            allure.when(Allure::getLifecycle).thenReturn(lifecycle);

            Validations.assertThat().response(response).headerValue("Authorization").isEqualTo(actualSecret);
            assertStepDoesNotContain(step, actualSecret, expectedSecret);
            Validations.assertThat().response(response).cookieValue("session").isEqualTo(actualSecret);
            assertStepDoesNotContain(step, actualSecret, expectedSecret);
            Validations.assertThat().response(response).bodyValue().isEqualTo(actualSecret);
            assertStepDoesNotContain(step, actualSecret, expectedSecret);
            Validations.assertThat().response(response).jsonValue("secret").isEqualTo(actualSecret);
            assertStepDoesNotContain(step, actualSecret, expectedSecret);
            Validations.assertThat().response(response).jsonValues("items").isEqualTo(List.of(actualSecret));
            assertStepDoesNotContain(step, actualSecret, expectedSecret);
            AssertionError hard = Assert.expectThrows(AssertionError.class,
                    () -> Validations.assertThat().response(response).headerValue("Authorization")
                            .isEqualTo(expectedSecret));
            Assert.assertFalse(String.valueOf(hard.getMessage()).contains(actualSecret));
            Assert.assertFalse(String.valueOf(hard.getMessage()).contains(expectedSecret));

            Validations.verifyThat().response(response).headerValue("Authorization").isEqualTo(expectedSecret);
            AssertionError soft = ValidationsHelper.getVerificationErrorToForceFail();
            Assert.assertNotNull(soft);
            Assert.assertFalse(String.valueOf(soft.getMessage()).contains(actualSecret));
            Assert.assertFalse(String.valueOf(soft.getMessage()).contains(expectedSecret));
            ValidationsHelper.resetVerificationStateAfterFailing();

            assertStepDoesNotContain(step, actualSecret, expectedSecret);
            report.verify(() -> ReportManager.logDiscrete(argThat(message ->
                    message.contains(actualSecret) || message.contains(expectedSecret))), never());
            report.verify(() -> ReportManager.logDiscrete(argThat(message ->
                    message.contains(actualSecret) || message.contains(expectedSecret)), any()), never());
        }
    }

    @Test
    public void apiValueReportingShouldNotInvokeUserCollectionMethods() {
        java.util.Collection<Object> hostileCollection = new java.util.AbstractCollection<>() {
            @Override
            public java.util.Iterator<Object> iterator() {
                return java.util.Collections.emptyIterator();
            }

            @Override
            public int size() {
                throw new AssertionError("reporting must not invoke collection methods");
            }
        };

        NativeValidationsBuilder plan = NativeValidationsBuilder.apiValue(
                com.shaft.validation.ValidationEnums.ValidationCategory.HARD_ASSERT,
                "hostile collection",
                () -> hostileCollection,
                new StringBuilder("the API response hostile collection "));
        ValidationsExecutor executor = plan.isEqualTo(hostileCollection);

        Assert.assertEquals(executor.reportMessageBuilder.toString(),
                "the API response hostile collection is equal to \"collection API value\".");
    }

    @Test
    public void jsonStartersShouldRejectNullPathsBeforeReadingTheResponse() {
        Response response = Mockito.mock(Response.class);
        NullPointerException scalar = Assert.expectThrows(NullPointerException.class,
                () -> Validations.assertThat().response(response).jsonValue(null));
        NullPointerException list = Assert.expectThrows(NullPointerException.class,
                () -> Validations.assertThat().response(response).jsonValues(null));
        Assert.assertEquals(scalar.getMessage(), "JSON path must not be null.");
        Assert.assertEquals(list.getMessage(), "JSON path must not be null.");
        Mockito.verifyNoInteractions(response);
    }

    @Test
    public void focusedPlansShouldBeIndependentDuringConcurrentRootReuse() throws Exception {
        Response response = response();
        RestValidationsBuilder root = Validations.assertThat().response(response);
        Path contract = Files.createTempFile("shaft-concurrent-api-contract-", ".json");
        Files.writeString(contract, "{\"id\":7,\"items\":[1,2]}", StandardCharsets.UTF_8);
        try (var executor = Executors.newFixedThreadPool(8)) {
            for (int iteration = 0; iteration < 10; iteration++) {
                CountDownLatch start = new CountDownLatch(1);
                var futures = List.of(
                        executor.submit(() -> after(start, () -> root.statusCodeValue().isEqualTo(201))),
                        executor.submit(() -> after(start, () -> root.headerValue("X-Trace").isEqualTo("trace-value"))),
                        executor.submit(() -> after(start, () -> root.cookieValue("session").isEqualTo("cookie-value"))),
                        executor.submit(() -> after(start, () -> root.bodyValue().contains("\"id\":7"))),
                        executor.submit(() -> after(start, () -> root.jsonValue("id").isEqualTo("7"))),
                        executor.submit(() -> after(start, () -> root.jsonValues("items").isEqualTo(List.of(1, 2)))),
                        executor.submit(() -> after(start, () -> root.responseTimeMillis().isEqualTo(42))),
                        executor.submit(() -> after(start, () -> root.matchesContract(contract.toString()))));
                start.countDown();
                List<String> messages = new java.util.ArrayList<>();
                for (var future : futures) messages.add(future.get(10, TimeUnit.SECONDS).reportMessageBuilder.toString());
                Assert.assertEquals(messages, List.of(
                        "the API response status code is equal to \"201\".",
                        "the API response header value is equal to \"text API value (11 characters)\".",
                        "the API response cookie value is equal to \"text API value (12 characters)\".",
                        "the API response response body contains \"text API value (6 characters)\".",
                        "the API response JSON value is equal to \"text API value (1 characters)\".",
                        "the API response JSON value list is equal to \"collection API value\".",
                        "the API response response time in milliseconds is equal to \"42\".",
                        "the API response is equal to the contents of this file \"" + contract
                                + "\" (Ignoring Ordering)."));
            }
        } finally {
            Files.deleteIfExists(contract);
        }
    }

    @Test
    public void focusedPlansShouldBeFullyLazyAndPreserveProviderFailureIdentityByFamily() {
        Response response = Mockito.mock(Response.class);
        NativeValidationsBuilder header = Validations.assertThat().response(response).headerValue("X-Late");
        Validations.assertThat().response(response).cookieValue("session");
        Validations.assertThat().response(response).bodyValue();
        Validations.assertThat().response(response).jsonValue("id");
        Validations.assertThat().response(response).jsonValues("items");
        NumberValidationsBuilder status = Validations.assertThat().response(response).statusCodeValue();
        NumberValidationsBuilder time = Validations.assertThat().response(response).responseTimeMillis();
        Mockito.verifyNoInteractions(response);

        RuntimeException scalarFailure = new IllegalStateException("scalar sentinel");
        Mockito.when(response.getHeader("X-Late")).thenThrow(scalarFailure);
        Assert.assertSame(Assert.expectThrows(RuntimeException.class, () -> header.isEqualTo("value")), scalarFailure);
        RuntimeException statusFailure = new IllegalStateException("status sentinel");
        Mockito.when(response.statusCode()).thenThrow(statusFailure);
        Assert.assertSame(Assert.expectThrows(RuntimeException.class, () -> status.isEqualTo(200)), statusFailure);
        RuntimeException timeFailure = new IllegalStateException("time sentinel");
        Mockito.when(response.timeIn(TimeUnit.MILLISECONDS)).thenThrow(timeFailure);
        Assert.assertSame(Assert.expectThrows(RuntimeException.class, () -> time.isEqualTo(1)), timeFailure);

        Response contractResponse = Mockito.mock(Response.class);
        RuntimeException contractFailure = new IllegalStateException("contract sentinel");
        Mockito.when(contractResponse.asString()).thenThrow(contractFailure);
        Assert.assertSame(Assert.expectThrows(RuntimeException.class,
                () -> Validations.assertThat().response(contractResponse).matchesContract("post1Response.json")),
                contractFailure);
    }

    private static void assertVocabularyPresent() {
        Set<String> methods = Arrays.stream(RestValidationsBuilder.class.getMethods())
                .map(Method::getName).collect(Collectors.toSet());
        Assert.assertTrue(methods.containsAll(FOCUSED_STARTERS), "Focused API vocabulary is incomplete.");
    }

    private static NativeValidationsBuilder invokeNative(RestValidationsBuilder root, String method,
                                                          Object... arguments) throws Exception {
        Class<?>[] types = Arrays.stream(arguments).map(Object::getClass).toArray(Class<?>[]::new);
        return (NativeValidationsBuilder) RestValidationsBuilder.class.getMethod(method, types).invoke(root, arguments);
    }

    private static Response response() {
        Response response = Mockito.mock(Response.class);
        ResponseBody<?> body = Mockito.mock(ResponseBody.class);
        Mockito.when(response.statusCode()).thenReturn(201);
        Mockito.when(response.getHeader("X-Trace")).thenReturn("trace-value");
        Mockito.when(response.getCookie("session")).thenReturn("cookie-value");
        Mockito.when(response.getBody()).thenReturn(body);
        Mockito.when(body.asString()).thenReturn("{\"id\":7,\"items\":[1,2]}");
        Mockito.when(response.asString()).thenReturn("{\"id\":7,\"items\":[1,2]}");
        Mockito.when(response.asPrettyString()).thenReturn("{\"id\":7,\"items\":[1,2]}");
        Mockito.when(response.timeIn(TimeUnit.MILLISECONDS)).thenReturn(42L);
        return response;
    }

    private static void assertHardFails(Runnable terminal) {
        Assert.expectThrows(AssertionError.class, terminal::run);
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    private static ValidationsExecutor after(CountDownLatch start, Supplier<ValidationsExecutor> terminal) {
        try {
            start.await(5, TimeUnit.SECONDS);
            return terminal.get();
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("Concurrent API validation was interrupted.", exception);
        }
    }

    private static void assertSoftFails(Runnable terminal) {
        terminal.run();
        Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @SuppressWarnings("unchecked")
    private static StepResult captureStepUpdates(AllureLifecycle lifecycle) {
        StepResult step = new StepResult();
        Mockito.doAnswer(invocation -> {
            Consumer<StepResult> consumer = invocation.getArgument(0);
            consumer.accept(step);
            return null;
        }).when(lifecycle).updateStep(any(Consumer.class));
        return step;
    }

    private static void assertStepDoesNotContain(StepResult step, String... sentinels) {
        Assert.assertTrue(step.getParameters().stream().noneMatch(parameter -> Arrays.stream(sentinels)
                .anyMatch(sentinel -> String.valueOf(parameter.getValue()).contains(sentinel))));
    }

    private static int compileCurrentRestSubclass(String className, String methods) throws Exception {
        Path classes = Files.createTempDirectory("shaft-rest-source-collision-");
        try {
            Path source = classes.resolve(className + ".java");
            Files.writeString(source, """
                    package com.shaft.validation.internal;
                    import com.shaft.validation.ValidationEnums;
                    public final class %s extends RestValidationsBuilder {
                        public %s() {
                            super(ValidationEnums.ValidationCategory.HARD_ASSERT, null, new StringBuilder());
                        }
                        %s
                    }
                    """.formatted(className, className, methods), StandardCharsets.UTF_8);
            return ToolProvider.getSystemJavaCompiler().run(null, null, null,
                    "-classpath", System.getProperty("java.class.path"), "-d", classes.toString(), source.toString());
        } finally {
            try (var paths = Files.walk(classes)) {
                paths.sorted(Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (java.io.IOException exception) {
                        throw new IllegalStateException("Could not clean source collision fixture.", exception);
                    }
                });
            }
        }
    }
}
