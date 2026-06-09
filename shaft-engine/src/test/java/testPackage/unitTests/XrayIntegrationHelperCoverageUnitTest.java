package testPackage.unitTests;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.internal.tms.XrayIntegrationHelper;
import com.sun.net.httpserver.HttpServer;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class XrayIntegrationHelperCoverageUnitTest {
    private Path tempDir;

    @BeforeMethod(alwaysRun = true)
    public void setup() throws Exception {
        tempDir = Files.createTempDirectory("xrayCoverage-");
        SHAFT.Properties.jira.set()
                .jiraUrl("http://127.0.0.1")
                .authType("Basic")
                .authorization("user:pass")
                .projectKey("PRJ")
                .assignee("demo-assignee");
        setStaticField("_JiraAuthorization", "user:pass");
        setStaticField("_TestExecutionID", null);
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        if (tempDir != null) {
            FileActions.getInstance(true).deleteFolder(tempDir.toString());
        }
        Properties.clearForCurrentThread();
        setStaticField("_JiraAuthorization", "user:pass");
        setStaticField("_TestExecutionID", null);
    }

    @Test
    public void xrayIntegrationHelperShouldCoverHappyPathsAgainstLocalServer() throws Exception {
        AtomicInteger renameRequests = new AtomicInteger(0);
        AtomicInteger createIssueRequests = new AtomicInteger(0);
        AtomicInteger attachRequests = new AtomicInteger(0);
        AtomicInteger linkRequests = new AtomicInteger(0);
        List<String> createBodies = new ArrayList<>();

        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/rest/raven/1.0/import/execution/cucumber", exchange -> {
            consumeRequest(exchange);
            byte[] body = "{\"testExecIssue\":{\"key\":\"EXEC-1\"}}".getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, body.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(body);
            }
        });

        server.createContext("/rest/raven/1.0/import/execution/testng", exchange -> {
            consumeRequest(exchange);
            byte[] body = "{\"testExecIssue\":{\"key\":\"EXEC-2\"}}".getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, body.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(body);
            }
        });

        server.createContext("/rest/api/2/issue/EXEC-1", exchange -> {
            renameRequests.incrementAndGet();
            consumeRequest(exchange);
            exchange.sendResponseHeaders(204, -1);
            exchange.close();
        });

        server.createContext("/rest/api/2/issue", exchange -> {
            if ("POST".equalsIgnoreCase(exchange.getRequestMethod())) {
                createIssueRequests.incrementAndGet();
                createBodies.add(readRequestBody(exchange));
                byte[] body = "{\"key\":\"BUG-1\"}".getBytes(StandardCharsets.UTF_8);
                exchange.getResponseHeaders().add("Content-Type", "application/json");
                exchange.sendResponseHeaders(200, body.length);
                try (OutputStream os = exchange.getResponseBody()) {
                    os.write(body);
                }
            } else {
                exchange.sendResponseHeaders(405, -1);
                exchange.close();
            }
        });

        server.createContext("/rest/api/2/issue/BUG-1/attachments", exchange -> {
            attachRequests.incrementAndGet();
            consumeRequest(exchange);
            exchange.sendResponseHeaders(200, -1);
            exchange.close();
        });

        server.createContext("/rest/api/2/issue/TKT-1", exchange -> {
            linkRequests.incrementAndGet();
            consumeRequest(exchange);
            exchange.sendResponseHeaders(200, -1);
            exchange.close();
        });

        server.start();

        Path cucumberReport = tempDir.resolve("cucumber.json");
        Path testNgReport = tempDir.resolve("testng.xml");
        Path attachment = tempDir.resolve("attachment.txt");
        Files.writeString(cucumberReport, "[{\"elements\":[]}]", StandardCharsets.UTF_8);
        Files.writeString(testNgReport, "<testng-results/>", StandardCharsets.UTF_8);
        Files.writeString(attachment, "evidence", StandardCharsets.UTF_8);

        try {
            SHAFT.Properties.jira.set().jiraUrl("http://127.0.0.1:" + server.getAddress().getPort());
            XrayIntegrationHelper.importCucumberResults(cucumberReport.toString());
            XrayIntegrationHelper.renameTestExecutionSuit("Regression", "Execution description");
            XrayIntegrationHelper.importTestNGResults(testNgReport.toString());
            String bugId = XrayIntegrationHelper.createIssue(
                    List.of(attachment.toString()),
                    "case \"A\"",
                    "line1\nline2\u0000 \"quoted\"");
            XrayIntegrationHelper.attachFilesToIssue("BUG-1", List.of(attachment.toString()));
            XrayIntegrationHelper.link2Tickets("TKT-1", "TKT-2");

            Assert.assertEquals(bugId, "BUG-1");
            Assert.assertEquals(renameRequests.get(), 1);
            Assert.assertEquals(createIssueRequests.get(), 1);
            Assert.assertEquals(attachRequests.get(), 2);
            Assert.assertEquals(linkRequests.get(), 1);
            Assert.assertTrue(createBodies.getFirst().contains("Execution Bug: case"));
            Assert.assertTrue(createBodies.getFirst().contains("line1line2 'quoted'"));
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void xrayIntegrationHelperShouldCoverGuardAndFailurePaths() throws Exception {
        AtomicInteger renameRequests = new AtomicInteger(0);
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/rest/api/2/issue/", exchange -> {
            renameRequests.incrementAndGet();
            consumeRequest(exchange);
            exchange.sendResponseHeaders(204, -1);
            exchange.close();
        });
        server.start();

        try {
            SHAFT.Properties.jira.set().jiraUrl("http://127.0.0.1:" + server.getAddress().getPort());
            XrayIntegrationHelper.renameTestExecutionSuit("ignored", "ignored");
            Assert.assertEquals(renameRequests.get(), 0);

            SHAFT.Properties.jira.set().jiraUrl("http://127.0.0.1:1");
            XrayIntegrationHelper.importTestNGResults(tempDir.resolve("does-not-exist.xml").toString());
            Assert.assertNull(XrayIntegrationHelper.createIssue(List.of(), "test", "desc"));
            XrayIntegrationHelper.attachFilesToIssue("BUG-404", List.of(tempDir.resolve("missing.log").toString()));
            XrayIntegrationHelper.link2Tickets("TKT-404", "TKT-405");
        } finally {
            server.stop(0);
        }
    }

    private static void setStaticField(String fieldName, Object value) {
        try {
            Field field = XrayIntegrationHelper.class.getDeclaredField(fieldName);
            field.setAccessible(true);
            field.set(null, value);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static void consumeRequest(com.sun.net.httpserver.HttpExchange exchange) throws IOException {
        readRequestBody(exchange);
    }

    private static String readRequestBody(com.sun.net.httpserver.HttpExchange exchange) throws IOException {
        try (InputStream requestBody = exchange.getRequestBody()) {
            return new String(requestBody.readAllBytes(), StandardCharsets.UTF_8);
        }
    }
}
