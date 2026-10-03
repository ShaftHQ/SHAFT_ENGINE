package testPackage.unitTests;

import com.shaft.listeners.AllureListener;
import com.shaft.tools.io.internal.OpenTelemetryTracing;
import io.opentelemetry.api.OpenTelemetry;
import io.opentelemetry.api.trace.StatusCode;
import io.opentelemetry.sdk.OpenTelemetrySdk;
import io.opentelemetry.sdk.common.CompletableResultCode;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.data.SpanData;
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor;
import io.opentelemetry.sdk.trace.export.SpanExporter;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;
import io.qameta.allure.model.StepResult;
import io.qameta.allure.model.TestResult;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * OpenTelemetry span export for tests and steps (#6404).
 */
public class OpenTelemetryTracingTest {
    private final List<SpanData> spans = new CopyOnWriteArrayList<>();

    /**
     * One test with one failing step yields a test span parenting a step span with error status.
     */
    @Test
    public void testAndStepProduceNestedSpans() {
        SpanExporter memory = new SpanExporter() {
            @Override
            public CompletableResultCode export(Collection<SpanData> batch) {
                spans.addAll(batch);
                return CompletableResultCode.ofSuccess();
            }

            @Override
            public CompletableResultCode flush() {
                return CompletableResultCode.ofSuccess();
            }

            @Override
            public CompletableResultCode shutdown() {
                return CompletableResultCode.ofSuccess();
            }
        };
        OpenTelemetrySdk sdk = OpenTelemetrySdk.builder()
                .setTracerProvider(SdkTracerProvider.builder().addSpanProcessor(SimpleSpanProcessor.create(memory)).build())
                .build();
        OpenTelemetryTracing.use(sdk);
        AllureListener listener = new AllureListener(io.qameta.allure.Allure.getLifecycle());
        listener.afterTestStart(new TestResult().setName("login").setFullName("LoginTest.login"));
        OpenTelemetryTracing.start("step", "Click Go");
        OpenTelemetryTracing.stop(Status.FAILED, new StatusDetails().setMessage("boom"));
        listener.afterTestStop(new TestResult().setStatus(Status.FAILED));

        Assert.assertEquals(spans.size(), 2);
        SpanData step = spans.get(0);
        SpanData test = spans.get(1);
        Assert.assertEquals(test.getName(), "LoginTest.login");
        Assert.assertEquals(step.getName(), "Click Go");
        Assert.assertEquals(step.getParentSpanId(), test.getSpanId());
        Assert.assertEquals(step.getStatus().getStatusCode(), StatusCode.ERROR);
        Assert.assertEquals(step.getStatus().getDescription(), "boom");
    }

    /**
     * Without an SDK the API is a no-op: spans start and stop without output or errors.
     */
    @Test
    public void noSdkIsANoOp() {
        OpenTelemetryTracing.use(OpenTelemetry.noop());
        OpenTelemetryTracing.start("test", "t");
        OpenTelemetryTracing.stop(Status.PASSED, null);
        OpenTelemetryTracing.stop(Status.PASSED, null);
        Assert.assertTrue(spans.isEmpty());
    }

    /**
     * Restores the global OpenTelemetry instance.
     */
    @AfterMethod(alwaysRun = true)
    public void reset() {
        OpenTelemetryTracing.use(null);
        spans.clear();
    }
}
