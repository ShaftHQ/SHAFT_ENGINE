package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserType;
import com.microsoft.playwright.FrameLocator;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.options.ColorScheme;
import com.microsoft.playwright.Playwright;
import com.sun.net.httpserver.HttpServer;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.net.InetSocketAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * Opens the generated trace viewer from inside a real Allure 3 report (issue #6731): the report is
 * generated with the cached Allure CLI, served over HTTP and driven in headless Chromium.
 */
public class TraceViewerAllureAcceptanceTest {
    private static final String RESULT = """
            {"uuid":"aaaa","historyId":"h1","name":"traceViewer","fullName":"customer.CheckoutTest.traceViewer",
             "status":"failed","stage":"finished","start":1000,"stop":2000,"statusDetails":{"message":"boom"},
             "attachments":[{"name":"SHAFT trace viewer","source":"aaaa-attachment.html","type":"text/html"}],
             "labels":[{"name":"suite","value":"CheckoutTest"}]}
            """;

    @Test(groups = "trace-viewer-browser-acceptance")
    public void viewerShouldBeUsableInsideARealAllureReport() throws Exception {
        Path cli = Path.of(System.getProperty("user.home"), ".m2", "repository", "allure", "allure-cli",
                System.getProperty("shaft.allure.cli.version", "3.20.1"), "node_modules", "allure", "cli.js");
        if (!Files.isRegularFile(cli)) {
            throw new SkipException("The Allure CLI is not cached at " + cli);
        }
        Path chrome = TraceViewerBrowserAcceptanceTest.chromeExecutable();
        TraceViewerBrowserAcceptanceTest.ViewerFixture fixture = TraceViewerBrowserAcceptanceTest.generateViewerFixture();
        Path work = Files.createTempDirectory("trace-viewer-allure");
        Path results = Files.createDirectories(work.resolve("results"));
        Path report = work.resolve("report");
        Files.copy(fixture.html(), results.resolve("aaaa-attachment.html"));
        Files.writeString(results.resolve("aaaa-result.json"), RESULT);
        Process process = new ProcessBuilder("node", cli.toString(), "generate", results.toString(), "-o",
                report.toString()).redirectErrorStream(true).start();
        String output = new String(process.getInputStream().readAllBytes());
        Assert.assertTrue(process.waitFor(120, TimeUnit.SECONDS) && process.exitValue() == 0, output);

        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/", exchange -> {
            String path = exchange.getRequestURI().getPath();
            Path file = report.resolve(path.equals("/") ? "index.html" : path.substring(1)).normalize();
            if (!file.startsWith(report) || !Files.isRegularFile(file)) {
                exchange.sendResponseHeaders(404, -1);
            } else {
                byte[] body = Files.readAllBytes(file);
                String name = file.getFileName().toString();
                exchange.getResponseHeaders().add("Content-Type", name.endsWith(".html") ? "text/html"
                        : name.endsWith(".js") ? "text/javascript" : name.endsWith(".json") ? "application/json"
                        : "application/octet-stream");
                exchange.sendResponseHeaders(200, body.length);
                exchange.getResponseBody().write(body);
            }
            exchange.close();
        });
        server.start();
        try (Playwright playwright = Playwright.create();
             Browser browser = playwright.chromium().launch(new BrowserType.LaunchOptions()
                     .setExecutablePath(chrome).setHeadless(true))) {
            String origin = "http://127.0.0.1:" + server.getAddress().getPort() + "/";
            for (ColorScheme scheme : List.of(ColorScheme.LIGHT, ColorScheme.DARK)) {
                Page page = browser.newContext(new Browser.NewContextOptions().setViewportSize(1440, 900)
                        .setColorScheme(scheme)).newPage();
                List<String> problems = new ArrayList<>();
                page.onPageError(problems::add);
                page.onRequest(request -> {
                    String url = request.url();
                    if (request.frame().parentFrame() != null && !url.startsWith(origin) && !url.startsWith("data:") && !url.startsWith("blob:")
                            && !url.startsWith("about:")) {
                        problems.add("viewer external request " + url);
                    }
                });
                page.navigate(origin);
                page.getByText("traceViewer").first().click();
                page.getByText("SHAFT trace viewer").first().click();
                page.locator("[data-testid*=attachment], [class*=ttachment]").first().click();
                FrameLocator frame = page.frameLocator("iframe").first();
                frame.locator("#theme-toggle").waitFor();
                frame.locator("#action-search").waitFor();
                Assert.assertTrue(frame.locator("#action-search").isVisible(), "The viewer must render in Allure.");
                Assert.assertTrue(frame.locator("button[data-tab=console]").isVisible());
                Assert.assertTrue(frame.locator("button[data-tab=comparison]").isVisible(),
                        "Snapshot tabs carry captured evidence, so they stay visible.");
                Assert.assertTrue(frame.locator("button[data-tab=mobile]").isHidden(),
                        "#6730: a web trace has no mobile evidence, so that tab stays hidden in Allure too.");
                double luminance = ((Number) frame.locator("body").evaluate("""
                        body => { const m = getComputedStyle(body).backgroundColor.match(/\\d+/g).map(Number);
                          return (0.2126 * m[0] + 0.7152 * m[1] + 0.0722 * m[2]) / 255; }""")).doubleValue();
                if (scheme == ColorScheme.DARK) {
                    Assert.assertTrue(luminance < 0.25, "The viewer must follow Allure's dark theme: " + luminance);
                } else {
                    Assert.assertTrue(luminance > 0.75, "The viewer must follow Allure's light theme: " + luminance);
                }
                String pressed = frame.locator("#theme-toggle").getAttribute("aria-pressed");
                frame.locator("#theme-toggle").click();
                Assert.assertNotEquals(frame.locator("#theme-toggle").getAttribute("aria-pressed"), pressed,
                        "The manual theme toggle must work inside Allure's sandboxed frame.");
                frame.locator("#theme-toggle").click();
                keepScreenshot(page, scheme == ColorScheme.DARK ? "-allure-dark" : "-allure-light");
                Assert.assertTrue(problems.isEmpty(), "The viewer must not error or reach the network: " + problems);
                page.context().close();
            }
        } finally {
            server.stop(0);
        }
    }

    private static void keepScreenshot(Page page, String suffix) {
        String target = System.getProperty("shaft.trace.viewer.screenshot", "");
        if (target.isBlank()) {
            return;
        }
        Path base = Path.of(target).toAbsolutePath().normalize();
        String name = base.getFileName().toString();
        int dot = name.lastIndexOf('.');
        page.screenshot(new Page.ScreenshotOptions().setFullPage(true).setPath(base.resolveSibling(
                dot < 0 ? name + suffix : name.substring(0, dot) + suffix + name.substring(dot))));
    }
}
