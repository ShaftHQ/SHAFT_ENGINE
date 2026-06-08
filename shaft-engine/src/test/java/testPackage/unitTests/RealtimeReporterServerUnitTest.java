package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.RealtimeReporter;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.net.HttpURLConnection;
import java.net.URLEncoder;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Test(singleThreaded = true)
public class RealtimeReporterServerUnitTest {
    private String baseUrl;
    private boolean originalAllureAutoOpen;

    @BeforeMethod(alwaysRun = true)
    public void setup() throws Exception {
        RealtimeReporterTestLock.LOCK.lock();
        RealtimeReporter.stopServer();
        originalAllureAutoOpen = SHAFT.Properties.allure.automaticallyOpen();
        SHAFT.Properties.allure.set().automaticallyOpen(false);
        Method startServer = RealtimeReporter.class.getDeclaredMethod("startServer");
        startServer.setAccessible(true);
        startServer.invoke(null);
        if (!RealtimeReporter.isRunning()) {
            throw new SkipException("RealtimeReporter server is not available in this environment.");
        }
        var dashboardUrlField = RealtimeReporter.class.getDeclaredField("DASHBOARD_URL");
        dashboardUrlField.setAccessible(true);
        baseUrl = (String) dashboardUrlField.get(null);
        if (!waitForServerReady()) {
            throw new SkipException("RealtimeReporter server did not become ready in this environment.");
        }
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        try {
            RealtimeReporter.stopServer();
            SHAFT.Properties.allure.set().automaticallyOpen(originalAllureAutoOpen);
            Properties.clearForCurrentThread();
        } finally {
            RealtimeReporterTestLock.LOCK.unlock();
        }
    }

    @Test
    public void httpEndpointsShouldServeStateDetailsAndAttachment() throws Exception {
        Assert.assertTrue(RealtimeReporter.isRunning());
        String testId = "com.example.Feature#scenario";

        RealtimeReporter.onTestsPlanned(List.of(new RealtimeReporter.TestCard(testId, "com.example.Feature", "scenario", "src/test/java/com/example/Feature.java")));
        RealtimeReporter.onTestStarted(testId);
        RealtimeReporter.appendConsoleLog(testId, "sample\u001B[31m-log");
        RealtimeReporter.appendStep(testId, "step one", null);
        RealtimeReporter.appendAttachment(testId, null, null, null, "sample-content".getBytes(StandardCharsets.UTF_8));
        RealtimeReporter.onTestFinished(testId, RealtimeReporter.TestStatus.FAILED, new RuntimeException("boom"));

        HttpResponse rootResponse = request("GET", "/");
        Assert.assertEquals(rootResponse.code, 200);
        Assert.assertTrue(rootResponse.body.contains("SHAFT"));

        HttpResponse stateResponse = request("GET", "/api/state");
        Assert.assertEquals(stateResponse.code, 200);
        Assert.assertTrue(stateResponse.body.contains("\"tests\""));
        Assert.assertTrue(stateResponse.body.contains(testId));

        String encodedTestId = URLEncoder.encode(testId, StandardCharsets.UTF_8);
        HttpResponse detailsResponse = request("GET", "/api/test/" + encodedTestId);
        Assert.assertEquals(detailsResponse.code, 200);
        Assert.assertTrue(detailsResponse.body.contains("sample-log"));
        Assert.assertTrue(detailsResponse.body.contains("\"FAILED\""));
        Assert.assertTrue(detailsResponse.body.contains("\"hasAttachments\":true"));

        Matcher matcher = Pattern.compile("\"url\":\"/api/attachment/(att-\\d+)\"").matcher(detailsResponse.body);
        Assert.assertTrue(matcher.find());
        String attachmentId = matcher.group(1);

        HttpResponse attachmentResponse = request("GET", "/api/attachment/" + attachmentId);
        Assert.assertEquals(attachmentResponse.code, 200);
        Assert.assertEquals(attachmentResponse.body, "sample-content");

        HttpResponse detailsNotFound = request("GET", "/api/test/" + URLEncoder.encode("unknown#id", StandardCharsets.UTF_8));
        Assert.assertEquals(detailsNotFound.code, 404);

        HttpResponse attachmentNotFound = request("GET", "/api/attachment/att-unknown");
        Assert.assertEquals(attachmentNotFound.code, 404);
    }

    @Test
    public void nonGetMethodsShouldReturnMethodNotAllowed() throws Exception {
        Assert.assertEquals(request("POST", "/").code, 405);
        Assert.assertEquals(request("POST", "/api/state").code, 405);
        Assert.assertEquals(request("POST", "/api/events").code, 405);
        Assert.assertEquals(request("POST", "/api/test/something").code, 405);
        Assert.assertEquals(request("POST", "/api/attachment/something").code, 405);
    }

    @Test
    public void onExecutionFinishedShouldKeepServerWhenAllureAutoOpenDisabled() {
        Assert.assertTrue(RealtimeReporter.isRunning());
        RealtimeReporter.onExecutionFinished();
        Assert.assertTrue(RealtimeReporter.isRunning());
    }

    private boolean waitForServerReady() throws InterruptedException {
        for (int attempt = 0; attempt < 20; attempt++) {
            try {
                if (request("GET", "/api/state").code == 200) {
                    return true;
                }
            } catch (Exception ignored) {
                Thread.sleep(100);
            }
        }
        return false;
    }

    private HttpResponse request(String method, String path) throws Exception {
        HttpURLConnection connection = (HttpURLConnection) new URL(baseUrl + path).openConnection();
        connection.setRequestMethod(method);
        connection.setConnectTimeout(5000);
        connection.setReadTimeout(5000);
        int code = connection.getResponseCode();
        InputStream stream = code >= 400 ? connection.getErrorStream() : connection.getInputStream();
        String body = "";
        if (stream != null) {
            try (InputStream responseStream = stream; ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
                responseStream.transferTo(baos);
                body = baos.toString(StandardCharsets.UTF_8);
            }
        }
        return new HttpResponse(code, body);
    }

    private record HttpResponse(int code, String body) {
    }
}
