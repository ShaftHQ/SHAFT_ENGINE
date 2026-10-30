package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.cli.TerminalActions;
import com.shaft.listeners.AllureListener;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.Reporting;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.CheckpointStatus;
import com.shaft.tools.io.internal.CheckpointType;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.qameta.allure.Allure;
import io.qameta.allure.AllureExternalKey;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.AllureResultsWriter;
import io.qameta.allure.FileSystemResultsWriter;
import io.qameta.allure.listener.LifecycleNotifier;
import io.qameta.allure.listener.StepLifecycleListener;
import io.qameta.allure.model.Attachment;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StepResult;
import io.qameta.allure.model.TestResult;
import org.aeonbits.owner.ConfigFactory;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import org.testng.annotations.DataProvider;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.Callable;

public class SourceCodeAttachmentTest {
    @AfterMethod(alwaysRun = true)
    public void resetProperties() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void successfulValidationAttachesItsUserSourceLine() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always");
        SHAFT.Properties.reporting.set().disableLogging(false).alwaysLogDiscreetly(false);

        try (Fixture fixture = new Fixture(null)) {
            SHAFT.Validations.assertThat().object("source-proof").isEqualTo("source-proof").perform();
            List<Attachment> attachments = fixture.attachments();
            Assert.assertEquals(attachments.size(), 1, "The validation must own exactly one source attachment.");
            Attachment attachment = attachments.getFirst();
            Assert.assertEquals(attachment.getType(), "text/html");
            Assert.assertTrue(attachment.getName().contains("SourceCodeAttachmentTest.java:"), attachment.getName());
            String html = Files.readString(fixture.directory.resolve(attachment.getSource()));
            Assert.assertTrue(html.contains("source-proof"), html);
            Assert.assertTrue(html.contains("source-line active"), html);
            Assert.assertTrue(html.contains("&#8594;"), html);
            fixture.result.setStatus(Status.PASSED);
            fixture.write();
            Path sample = Path.of("target/source-code-attachment-evidence");
            Files.createDirectories(sample);
            try (var files = Files.list(fixture.directory)) {
                for (Path file : files.toList()) {
                    Files.copy(file, sample.resolve(file.getFileName()), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                }
            }
        }
    }

    @DataProvider
    public Object[][] policies() {
        List<Object[]> cases = new ArrayList<>();
        for (String mode : List.of("Never", "FailuresOnly", "ValidationPointsOnly", "Always")) {
            for (Status status : List.of(Status.PASSED, Status.FAILED, Status.BROKEN, Status.SKIPPED)) {
                for (boolean validation : List.of(false, true)) {
                    boolean failed = status == Status.FAILED || status == Status.BROKEN;
                    boolean expected = status != Status.SKIPPED && (mode.equals("Always")
                            || (mode.equals("FailuresOnly") && failed)
                            || (mode.equals("ValidationPointsOnly") && (validation || failed)));
                    cases.add(new Object[]{mode, status, validation, expected});
                }
            }
        }
        return cases.toArray(Object[][]::new);
    }

    @Test(dataProvider = "policies")
    public void modesSelectIndividualFinalStepStatus(String mode, Status status, boolean validation, boolean expected) throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode(mode).disableLogging(false).alwaysLogDiscreetly(false);
        try (Fixture fixture = new Fixture(status)) {
            if (validation) {
                ReportManagerHelper.logNestedSteps("validation fixture", null, null, CheckpointStatus.PASS, CheckpointType.VERIFICATION);
            } else {
                terminalAction();
            }
            List<Attachment> attachments = fixture.attachments();
            Assert.assertEquals(attachments.size(), expected ? 1 : 0, mode + "/" + status + "/" + validation);
            Assert.assertEquals(fixture.result.getSteps().size(), 1, "Source evidence must not add report steps.");
            Assert.assertEquals(fixture.result.getSteps().getFirst().getStatus(), status);
            if (expected) {
                Attachment attachment = attachments.getFirst();
                Assert.assertEquals(attachment.getType(), "text/html");
                Assert.assertTrue(Files.readString(fixture.directory.resolve(attachment.getSource())).contains("SourceCodeAttachmentTest"));
            }
            fixture.write();
            String json = Files.readString(fixture.directory.resolve(fixture.result.getUuid() + "-result.json"));
            Assert.assertEquals(json.contains("Source code"), expected, json);
        }
    }

    @Test
    public void defaultAndEvidencePresetsPreserveExplicitOptIn() {
        Assert.assertEquals(ConfigFactory.create(Reporting.class).whenToAttachSourceCode(), "Never");
        Assert.assertEquals(ConfigFactory.create(Reporting.class, Map.of("whenToAttachSourceCode", "Always"))
                .whenToAttachSourceCode(), "Always");
        for (String preset : List.of("FAILURE_ONLY", "BALANCED", "FAST", "FULL", "CUSTOM")) {
            SHAFT.Properties.reporting.set().whenToAttachSourceCode("Never").evidenceLevel(preset);
            Assert.assertEquals(SHAFT.Properties.reporting.whenToAttachSourceCode(), "Never", preset);
            SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").evidenceLevel(preset);
            Assert.assertEquals(SHAFT.Properties.reporting.whenToAttachSourceCode(), "Always", preset);
        }
    }

    @Test
    public void propertyFileAndSystemPropertyConfigureSourcePolicy() throws Exception {
        String previous = System.getProperty("whenToAttachSourceCode");
        Path file = Files.createTempFile("source-attachment", ".properties");
        try {
            System.setProperty("whenToAttachSourceCode", "ValidationPointsOnly");
            Assert.assertEquals(ConfigFactory.create(Reporting.class).whenToAttachSourceCode(), "ValidationPointsOnly");
            System.clearProperty("whenToAttachSourceCode");
            Files.writeString(file, "whenToAttachSourceCode=FailuresOnly\n");
            java.util.Properties imported = new java.util.Properties();
            try (var reader = Files.newBufferedReader(file)) {
                imported.load(reader);
            }
            Assert.assertEquals(ConfigFactory.create(Reporting.class, imported).whenToAttachSourceCode(), "FailuresOnly");
        } finally {
            if (previous == null) {
                System.clearProperty("whenToAttachSourceCode");
            } else {
                System.setProperty("whenToAttachSourceCode", previous);
            }
            Files.deleteIfExists(file);
        }
    }

    @Test
    public void genericLogsAndAttachmentStepsAreExcluded() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").disableLogging(false).alwaysLogDiscreetly(false);
        try (Fixture fixture = new Fixture(null)) {
            ReportManager.log("ordinary user log");
            ReportManagerHelper.attachAsStep("Text", "fixture", new java.io.ByteArrayInputStream("content".getBytes(java.nio.charset.StandardCharsets.UTF_8)));
            Assert.assertTrue(fixture.attachments().isEmpty());
        }
    }

    @Test
    public void suppressedSuccessfulStepsStaySuppressed() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").disableLogging(true);
        try (Fixture fixture = new Fixture(null)) {
            terminalAction();
            Assert.assertTrue(fixture.result.getSteps().isEmpty());
        }
    }

    @Test
    public void pageObjectOwnsTheNearestUserCallSite() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").disableLogging(false).alwaysLogDiscreetly(false);
        try (Fixture fixture = new Fixture(null)) {
            new PageFixture().open();
            Assert.assertEquals(fixture.attachments().size(), 1);
            String html = Files.readString(fixture.directory.resolve(fixture.attachments().getFirst().getSource()));
            Assert.assertTrue(html.contains("SourceCodeAttachmentTest$PageFixture.open"), html);
            Assert.assertTrue(html.contains(".passAction(null, &quot;navigateToURL&quot;, &quot;page-source-proof&quot;)"), html);
        }
    }

    @Test
    public void attachmentWriteFailureDoesNotChangeActionStatus() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").disableLogging(false).alwaysLogDiscreetly(false);
        try (Fixture fixture = new Fixture(null, true)) {
            terminalAction();
            Assert.assertEquals(fixture.result.getSteps().getFirst().getStatus(), Status.PASSED);
            Assert.assertEquals(fixture.result.getSteps().size(), 1);
        }
    }

    @Test
    public void unavailableSourceLifecycleDoesNotChangeActionStatus() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").disableLogging(false).alwaysLogDiscreetly(false);
        try (Fixture fixture = new Fixture(null)) {
            AllureLifecycle unavailable = Mockito.mock(AllureLifecycle.class);
            Mockito.when(unavailable.getCurrentExecutableKey()).thenThrow(new IllegalStateException("source lifecycle unavailable"));
            fixture.listeners.clear();
            fixture.listeners.add(new AllureListener(unavailable));
            terminalAction();
            Assert.assertEquals(fixture.result.getSteps().size(), 1);
            Assert.assertEquals(fixture.result.getSteps().getFirst().getStatus(), Status.PASSED);
            Assert.assertTrue(fixture.attachments().isEmpty());
        }
    }

    @Test
    public void lostAmbientContextDoesNotAttachSourceToAnUnrelatedStep() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").disableLogging(false).alwaysLogDiscreetly(false);
        try (Fixture fixture = new Fixture(null)) {
            fixture.listeners.addFirst(new StepLifecycleListener() {
                @Override
                public void afterStepStart(StepResult step) {
                    fixture.lifecycle.clearCurrent();
                }
            });
            terminalAction();
            Assert.assertEquals(fixture.result.getSteps().size(), 1);
            Assert.assertEquals(fixture.result.getSteps().getFirst().getStatus(), Status.PASSED);
            Assert.assertTrue(fixture.attachments().isEmpty());
        }
    }

    @Test
    public void stepWithoutFinalStatusDoesNotClaimExecutedSource() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").disableLogging(false).alwaysLogDiscreetly(false);
        try (Fixture fixture = new Fixture(null)) {
            fixture.listeners.addFirst(new StepLifecycleListener() {
                @Override
                public void beforeStepStop(StepResult step) {
                    step.setStatus(null);
                }
            });
            terminalAction();
            Assert.assertEquals(fixture.result.getSteps().size(), 1);
            Assert.assertNull(fixture.result.getSteps().getFirst().getStatus());
            Assert.assertTrue(fixture.attachments().isEmpty());
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void serviceLoadedTestListenerReleasesAbandonedStepCapture() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").disableLogging(false).alwaysLogDiscreetly(false);
        AllureLifecycle lifecycle = new AllureLifecycle(Mockito.mock(AllureResultsWriter.class));
        var lifecycleLookup = MethodHandles.privateLookupIn(AllureLifecycle.class, MethodHandles.lookup());
        var notifier = (LifecycleNotifier) lifecycleLookup
                .findVarHandle(AllureLifecycle.class, "notifier", LifecycleNotifier.class).get(lifecycle);
        var notifierLookup = MethodHandles.privateLookupIn(LifecycleNotifier.class, MethodHandles.lookup());
        List<StepLifecycleListener> listeners = (List<StepLifecycleListener>) notifierLookup
                .findVarHandle(LifecycleNotifier.class, "stepListeners", List.class).get(notifier);
        Object stepListener = listeners.stream().filter(AllureListener.class::isInstance).findFirst().orElseThrow();
        Object testListener = ((List<?>) notifierLookup.findVarHandle(LifecycleNotifier.class, "testListeners", List.class)
                .get(notifier)).stream().filter(AllureListener.class::isInstance).findFirst().orElseThrow();
        Assert.assertNotSame(stepListener, testListener, "The real SPI uses distinct listener objects.");
        listeners.add(new StepLifecycleListener() {
            @Override
            public void afterStepStart(StepResult step) {
                // Allure suppresses recursive listener events here, reproducing an abandoned capture.
                lifecycle.stopStep(lifecycle.getCurrentExecutableKey().orElseThrow());
            }
        });
        var collectorLookup = MethodHandles.privateLookupIn(com.shaft.tools.io.internal.SourceCodeAttachment.class,
                MethodHandles.lookup());
        IdentityHashMap<StepResult, ?> pending = (IdentityHashMap<StepResult, ?>) collectorLookup
                .findStaticVarHandle(com.shaft.tools.io.internal.SourceCodeAttachment.class, "active", IdentityHashMap.class).get();
        AllureExternalKey root = AllureExternalKey.random(SourceCodeAttachmentTest.class);
        TestResult result = new TestResult().setUuid(UUID.randomUUID().toString()).setStatus(Status.PASSED);
        try (MockedStatic<Allure> allure = Mockito.mockStatic(Allure.class)) {
            allure.when(Allure::getLifecycle).thenReturn(lifecycle);
            lifecycle.scheduleTest(root, result);
            lifecycle.startTest(root);
            terminalAction();
            StepResult abandoned = result.getSteps().getFirst();
            Assert.assertTrue(pending.containsKey(abandoned), "Fixture must reproduce a retained unfinished capture.");
            lifecycle.stopTest(root);
            Assert.assertFalse(pending.containsKey(abandoned), "Separate test SPI must clear the abandoned step capture.");
        } finally {
            lifecycle.clearCurrent();
        }
    }

    @Test
    public void browserApiAndElementReportersAttachWithoutLiveInfrastructure() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").disableLogging(false)
                .alwaysLogDiscreetly(false).traceEnabled(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never").whenToTakePageSourceSnapshot("Never");
        boolean discrete = ReportManagerHelper.getDiscreteLogging();
        ReportManagerHelper.setDiscreteLogging(false);
        try (Fixture fixture = new Fixture(null)) {
            new com.shaft.gui.browser.internal.BrowserActionsHelper(false).passAction(null, "navigateToURL", "fixture");
            new com.shaft.gui.element.internal.ElementActionsHelper(false)
                    .reportActionResult(null, "tap", "fixture", null, null, "fixture", true);
            var api = MethodHandles.privateLookupIn(com.shaft.api.RestActions.class, MethodHandles.lookup())
                    .findStatic(com.shaft.api.RestActions.class, "reportActionResult", MethodType.methodType(String.class,
                            String.class, String.class, Object.class, io.restassured.specification.RequestSpecification.class,
                            io.restassured.response.Response.class, Boolean.class, List.class, Boolean.class, Throwable[].class));
            api.invoke("performRequest", "fixture", null, null, null, false, null, true, new Throwable[0]);
            Assert.assertEquals(fixture.result.getSteps().size(), 3);
            Assert.assertEquals(fixture.attachments().size(), 3);
        } finally {
            ReportManagerHelper.setDiscreteLogging(discrete);
        }
    }

    @Test
    public void mobileActionUsesItsExistingReportingStep() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").disableLogging(false)
                .alwaysLogDiscreetly(false).traceEnabled(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never").whenToTakePageSourceSnapshot("Never");
        var driver = Mockito.mock(io.appium.java_client.android.AndroidDriver.class);
        var helper = Mockito.mock(com.shaft.driver.internal.DriverFactory.DriverFactoryHelper.class);
        Mockito.when(helper.getDriver()).thenReturn(driver);
        try (var waits = Mockito.mockStatic(com.shaft.gui.browser.internal.JavaScriptWaitManager.class);
             Fixture fixture = new Fixture(null)) {
            new com.shaft.gui.element.TouchActions(helper).hideNativeKeyboard();
            Mockito.verify(driver).hideKeyboard();
            Assert.assertEquals(fixture.result.getSteps().size(), 1);
            Assert.assertEquals(fixture.attachments().size(), 1);
            Assert.assertEquals(fixture.result.getSteps().getFirst().getStatus(), Status.PASSED);
        }
    }

    @DataProvider
    public Object[][] publicApiPolicies() {
        List<Object[]> cases = new ArrayList<>();
        for (String mode : List.of("Never", "FailuresOnly", "ValidationPointsOnly", "Always")) {
            for (boolean failed : List.of(false, true)) {
                cases.add(new Object[]{mode, failed, mode.equals("Always") || (failed && !mode.equals("Never"))});
            }
        }
        return cases.toArray(Object[][]::new);
    }

    @Test(dataProvider = "publicApiPolicies")
    public void publicApiRequestAttachesToItsExistingStep(String mode, boolean failed, boolean expected) throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode(mode).disableLogging(false)
                .alwaysLogDiscreetly(false).traceEnabled(false);
        SHAFT.Properties.flags.set().automaticallyAssertResponseStatusCode(true);
        var previousFilters = List.copyOf(io.restassured.RestAssured.filters());
        var requests = new java.util.concurrent.atomic.AtomicInteger();
        io.restassured.RestAssured.replaceFiltersWith((request, response, context) -> {
            requests.incrementAndGet();
            return new io.restassured.builder.ResponseBuilder().setStatusCode(failed ? 404 : 200)
                    .setContentType("application/json").setBody("{\"message\":\"source-api-proof\"}").build();
        });
        try (Fixture fixture = new Fixture(null)) {
            SHAFT.API api = new SHAFT.API("https://source-attachment.invalid/");
            if (failed) {
                Assert.expectThrows(AssertionError.class, () -> api.get("fixture").perform());
            } else {
                api.get("fixture").perform();
                Assert.assertEquals(api.getResponse().getStatusCode(), 200);
            }
            Assert.assertEquals(requests.get(), 1, "The in-memory transport must handle the whole request.");
            Assert.assertEquals(fixture.result.getSteps().size(), 1, "The existing request step must be reused.");
            StepResult step = fixture.result.getSteps().getFirst();
            Assert.assertEquals(step.getStatus(), failed ? Status.FAILED : Status.PASSED);
            Assert.assertEquals(fixture.attachments().size(), expected ? 1 : 0, mode + "/" + failed);
            if (expected) {
                String html = Files.readString(fixture.directory.resolve(fixture.attachments().getFirst().getSource()));
                Assert.assertTrue(html.contains("api.get(&quot;fixture&quot;).perform()"), html);
                Assert.assertTrue(html.contains("SourceCodeAttachmentTest"), html);
            }
            fixture.write();
            String json = Files.readString(fixture.directory.resolve(fixture.result.getUuid() + "-result.json"));
            Assert.assertEquals(json.contains("Source code"), expected);
        } finally {
            io.restassured.RestAssured.replaceFiltersWith(previousFilters);
        }
    }

    @Test
    public void capturedKeyKeepsAttachmentOnItsStepWhenAnotherStepIsCurrent() throws Throwable {
        SHAFT.Properties.reporting.set().whenToAttachSourceCode("Always").disableLogging(false).alwaysLogDiscreetly(false);
        try (Fixture fixture = new Fixture(null)) {
            java.util.concurrent.atomic.AtomicReference<AllureExternalKey> outerKey = new java.util.concurrent.atomic.AtomicReference<>();
            fixture.listeners.add(new StepLifecycleListener() {
                private boolean entered;
                @Override
                public void afterStepStart(StepResult step) {
                    if (entered) {
                        return;
                    }
                    entered = true;
                    AllureExternalKey outer = fixture.lifecycle.getCurrentExecutableKey().orElseThrow();
                    outerKey.set(outer);
                    fixture.lifecycle.startStep(new StepResult().setName("ambient child").setStatus(Status.PASSED));
                    fixture.lifecycle.updateStep(outer, result -> result.setStatus(Status.FAILED));
                    // The reporting helper stops this child, leaving the outer action for explicit completion below.
                }
            });
            terminalAction();
            fixture.lifecycle.startStep(new StepResult().setName("another ambient child").setStatus(Status.PASSED));
            fixture.lifecycle.stopStep(outerKey.get());
            StepResult outer = fixture.result.getSteps().getFirst();
            Assert.assertEquals(outer.getAttachments().size(), 1,
                    "outer=" + outer.getName() + ", status=" + outer.getStatus() + ", children="
                            + outer.getSteps().stream().map(s -> s.getName() + ":" + s.getAttachments()).toList());
            Assert.assertEquals(outer.getStatus(), Status.FAILED);
            Assert.assertTrue(outer.getSteps().stream().allMatch(step -> step.getAttachments().isEmpty()));
        }
    }

    @Test
    public void parallelInvocationsKeepPoliciesAndAttachmentsSeparate() throws Throwable {
        try (Fixture fixture = new Fixture(null); var executor = Executors.newFixedThreadPool(2)) {
            List<Callable<Integer>> tasks = List.of(() -> isolatedCount(fixture.lifecycle, "Always"),
                    () -> isolatedCount(fixture.lifecycle, "Never"));
            var results = executor.invokeAll(tasks);
            Assert.assertEquals(results.get(0).get().intValue(), 1);
            Assert.assertEquals(results.get(1).get().intValue(), 0);
        }
    }

    @Test
    public void repeatedAndRetriedInvocationsKeepTheirOwnPolicy() throws Throwable {
        try (Fixture fixture = new Fixture(null)) {
            fixture.write();
            for (int attempt = 0; attempt < 3; attempt++) {
                SHAFT.Properties.reporting.set().whenToAttachSourceCode(attempt == 1 ? "Never" : "Always")
                        .disableLogging(false).alwaysLogDiscreetly(false);
                AllureExternalKey key = AllureExternalKey.random(SourceCodeAttachmentTest.class);
                TestResult retry = new TestResult().setUuid(UUID.randomUUID().toString())
                        .setHistoryId("source-retry-fixture").setName("Source retry fixture")
                        .setStatus(attempt < 2 ? Status.FAILED : Status.PASSED);
                fixture.lifecycle.scheduleTest(key, retry);
                fixture.lifecycle.startTest(key);
                if (attempt == 0) {
                    new PageFixture().open();
                    new PageFixture().open();
                } else {
                    terminalAction();
                }
                fixture.lifecycle.stopTest(key);
                fixture.lifecycle.writeTest(key);
                List<Attachment> attachments = new ArrayList<>();
                collect(retry.getSteps(), attachments);
                Assert.assertEquals(attachments.size(), attempt == 0 ? 2 : attempt == 1 ? 0 : 1);
                for (Attachment attachment : attachments) {
                    String html = Files.readString(fixture.directory.resolve(attachment.getSource()));
                    Assert.assertTrue(html.contains(attempt == 0 ? "$PageFixture.open" : "SourceCodeAttachmentTest.terminalAction"));
                }
            }
        }
    }

    private static int isolatedCount(AllureLifecycle lifecycle, String mode) throws Exception {
        try {
            SHAFT.Properties.reporting.set().whenToAttachSourceCode(mode).disableLogging(false).alwaysLogDiscreetly(false);
            try (MockedStatic<Allure> allure = Mockito.mockStatic(Allure.class)) {
                allure.when(Allure::getLifecycle).thenReturn(lifecycle);
                AllureExternalKey root = AllureExternalKey.random(SourceCodeAttachmentTest.class);
                TestResult result = new TestResult().setUuid(UUID.randomUUID().toString());
                lifecycle.scheduleTest(root, result);
                lifecycle.startTest(root);
                new PageFixture().open();
                lifecycle.stopTest(root);
                List<Attachment> attachments = new ArrayList<>();
                collect(result.getSteps(), attachments);
                lifecycle.writeTest(root);
                lifecycle.clearCurrent();
                return attachments.size();
            }
        } finally {
            Properties.clearForCurrentThread();
        }
    }

    private static void terminalAction() throws Throwable {
        var method = MethodHandles.privateLookupIn(TerminalActions.class, MethodHandles.lookup())
                .findStatic(TerminalActions.class, "reportActionResult", MethodType.methodType(String.class,
                        String.class, String.class, String.class, Boolean.class, Exception[].class));
        method.invoke("fixture", "source-proof", "", true, new Exception[0]);
    }

    private static final class PageFixture {
        private void open() {
            new com.shaft.gui.browser.internal.BrowserActionsHelper(false)
                    .passAction(null, "navigateToURL", "page-source-proof");
        }
    }

    private static final class Fixture implements AutoCloseable {
        private final Path directory = Files.createTempDirectory("shaft-source-attachment-");
        private final AllureExternalKey root = AllureExternalKey.random(SourceCodeAttachmentTest.class);
        private final TestResult result = new TestResult().setUuid(UUID.randomUUID().toString()).setName("Source attachment fixture");
        private final AllureLifecycle lifecycle;
        private final MockedStatic<Allure> allure;
        private final List<StepLifecycleListener> listeners = new ArrayList<>();

        private Fixture(Status finalStatus) throws Throwable {
            this(finalStatus, false);
        }

        private Fixture(Status finalStatus, boolean failAttachmentWrite) throws Throwable {
            var constructor = MethodHandles.privateLookupIn(AllureLifecycle.class, MethodHandles.lookup())
                    .findConstructor(AllureLifecycle.class, MethodType.methodType(void.class,
                            AllureResultsWriter.class, LifecycleNotifier.class));
            AllureResultsWriter writer = new FileSystemResultsWriter(directory);
            if (failAttachmentWrite) {
                writer = Mockito.mock(AllureResultsWriter.class);
                Mockito.doThrow(new IllegalStateException("fixture attachment write unavailable"))
                        .when(writer).write(Mockito.anyString(), Mockito.any(java.io.InputStream.class));
            }
            lifecycle = (AllureLifecycle) constructor.invoke(writer,
                    new LifecycleNotifier(List.of(), List.of(), List.of(), listeners));
            if (finalStatus != null) {
                listeners.add(new StepLifecycleListener() {
                    @Override
                    public void beforeStepStop(StepResult step) {
                        step.setStatus(finalStatus);
                    }
                });
            }
            listeners.add(new AllureListener(lifecycle));
            lifecycle.scheduleTest(root, result);
            lifecycle.startTest(root);
            allure = Mockito.mockStatic(Allure.class);
            allure.when(Allure::getLifecycle).thenReturn(lifecycle);
        }

        private List<Attachment> attachments() {
            List<Attachment> attachments = new ArrayList<>();
            collect(result.getSteps(), attachments);
            return attachments;
        }

        private void write() {
            lifecycle.stopTest(root);
            lifecycle.writeTest(root);
        }

        @Override
        public void close() throws Exception {
            try {
                lifecycle.clearCurrent();
            } finally {
                allure.close();
                try (var paths = Files.walk(directory)) {
                    for (Path path : paths.sorted(java.util.Comparator.reverseOrder()).toList()) {
                        Files.deleteIfExists(path);
                    }
                }
            }
        }
    }

    private static void collect(List<StepResult> steps, List<Attachment> attachments) {
        for (StepResult step : steps) {
            step.getAttachments().stream().filter(a -> a.getName().startsWith("Source code"))
                    .forEach(attachments::add);
            collect(step.getSteps(), attachments);
        }
    }
}
