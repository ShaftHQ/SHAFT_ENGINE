package testPackage.unitTests;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.gui.driver.BrowserActionsContract;
import com.shaft.gui.driver.ShaftLocator;
import com.shaft.api.RestActions;
import io.qameta.allure.model.Status;
import io.restassured.http.Cookie;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ShaftFacadeOfflineContractTest {
    private String savedAuthCache;
    private Path tempDir;

    @BeforeMethod(alwaysRun = true)
    public void rememberPaths() throws IOException {
        savedAuthCache = SHAFT.Properties.paths.authCache();
        tempDir = Files.createTempDirectory("shaft-facade-");
    }

    @AfterMethod(alwaysRun = true)
    public void restorePaths() throws IOException {
        if (savedAuthCache != null) {
            SHAFT.Properties.paths.set().authCache(savedAuthCache);
        }
        if (tempDir != null && Files.exists(tempDir)) {
            try (var walk = Files.walk(tempDir)) {
                walk.sorted(java.util.Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (IOException exception) {
                        throw new IllegalStateException(exception);
                    }
                });
            }
        }
    }

    @Test
    public void requestFactoriesReturnBuildersThatValidateStatusAndPathPlaceholders() {
        SHAFT.API api = SHAFT.API.getInstance("http://127.0.0.1");
        SHAFT.API wrapped = new SHAFT.API(new RestActions("http://127.0.0.1"));
        wrapped.addHeader("X-From-Wrap", "yes");
        Assert.assertEquals(wrapped.getHeaders().get("X-From-Wrap"), "yes");

        var get = api.get("/items");
        Assert.assertSame(get, get.setUrlArguments("q=1"));
        var post = api.post("/items");
        Assert.assertSame(post, post.setTargetStatusCode(201));
        var patch = api.patch("/items/{id}");
        Assert.assertSame(patch, patch.setPathParameters("9"));
        var delete = api.delete("/items");
        Assert.assertSame(delete, delete.setUrlArguments("force=true"));
        var put = api.put("/items");
        Assert.assertSame(put, put.setContentType("application/json"));
        var head = api.head("/items");
        Assert.assertSame(head, head.useRelaxedHTTPSValidation());
        var options = api.options("/items");
        Assert.assertSame(options, options.appendDefaultContentCharsetToContentTypeIfUndefined(false));

        IllegalArgumentException badStatus = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> api.post("/items").setTargetStatusCode(1));
        Assert.assertTrue(badStatus.getMessage() != null && !badStatus.getMessage().isBlank(), badStatus.getMessage());

        IllegalArgumentException missingPlaceholder = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> api.get("/items").setPathParameters(Map.of("id", "1")));
        Assert.assertTrue(missingPlaceholder.getMessage().contains("{id}"), missingPlaceholder.getMessage());
        Assert.assertTrue(missingPlaceholder.getMessage().contains("/items"), missingPlaceholder.getMessage());

        var withVariables = api.sendGraphQlRequest("/graphql", "{ hero }", Map.of("id", 7), "fragment X on Hero { name }");
        var queryOnly = api.sendGraphQlRequest("/graphql", "{ hero }", null, null);
        Assert.assertNotSame(withVariables, queryOnly);
        Assert.assertSame(withVariables, withVariables.setTargetStatusCode(200));
        Assert.assertSame(queryOnly, queryOnly.setTargetStatusCode(0));
    }

    @Test
    public void cookieFiltersKeepOnlyTheMatchingDomainAndPath() {
        SHAFT.API api = new SHAFT.API("http://127.0.0.1");
        BrowserActionsContract browser = browserWith(Set.of(
                new org.openqa.selenium.Cookie("session", "abc", ".Example.com", "/app", null),
                new org.openqa.selenium.Cookie("other", "no", "elsewhere.test", "/", null),
                new org.openqa.selenium.Cookie("root", "wide", "example.com", "/other", null),
                new org.openqa.selenium.Cookie("loose", "v")));

        api.importCookiesFrom(browser, ".example.com", "/app");
        Assert.assertEquals(api.getCookies().keySet(), Set.of("session"));
        Cookie session = api.getCookies().get("session");
        Assert.assertEquals(session.getValue(), "abc");
        Assert.assertEquals(session.getDomain(), ".Example.com");
        Assert.assertEquals(session.getPath(), "/app");

        SHAFT.API everyCookie = new SHAFT.API("http://127.0.0.1");
        everyCookie.importCookiesFrom(browser, null, " ");
        Assert.assertEquals(everyCookie.getCookies().keySet(), Set.of("session", "other", "root", "loose"));

        Map<String, String> exported = new HashMap<>();
        BrowserActionsContract destination = recordingBrowser(exported);
        everyCookie.exportCookiesTo(destination, ".EXAMPLE.com", "/app");
        Assert.assertEquals(exported, Map.of("session", "abc"));

        exported.clear();
        everyCookie.exportCookiesTo(destination, "example.com", null);
        Assert.assertEquals(exported, Map.of("session", "abc", "root", "wide"));

        NullPointerException missingBrowser = Assert.expectThrows(
                NullPointerException.class,
                () -> api.importCookiesFrom(null));
        Assert.assertTrue(missingBrowser.getMessage().contains("browserActions"), missingBrowser.getMessage());
    }

    @Test
    public void storageStateFileSeedsCookiesAndAMissingFileFails() throws IOException {
        Path state = tempDir.resolve("state.json");
        Files.writeString(state, """
                {"cookies":[
                  {"name":"fromFile","value":"file-value","domain":"files.example","path":"/"},
                  {"name":" ","value":"skip"}
                ]}
                """);
        SHAFT.API api = new SHAFT.API("http://127.0.0.1", state.toString());
        Assert.assertEquals(api.getCookies().keySet(), Set.of("fromFile"));
        Assert.assertEquals(api.getCookies().get("fromFile").getValue(), "file-value");
        Assert.assertEquals(api.getCookies().get("fromFile").getDomain(), "files.example");

        api.addHeader("X-Test", "kept");
        api.addCookie("plain", "1");
        Assert.assertEquals(api.getHeaders().get("X-Test"), "kept");
        Assert.assertEquals(api.getCookies().get("plain").getValue(), "1");

        IllegalStateException missing = Assert.expectThrows(
                IllegalStateException.class,
                () -> new SHAFT.API("http://127.0.0.1", tempDir.resolve("missing.json").toString()));
        Assert.assertTrue(missing.getMessage().contains("Could not read browser storage state"), missing.getMessage());
    }

    @Test
    public void authCacheHitSkipsTheLoginFlow() throws IOException {
        SHAFT.Properties.paths.set().authCache(tempDir.toString());
        IllegalArgumentException blank = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> SHAFT.Auth.stateFile("   "));
        Assert.assertTrue(blank.getMessage().contains("must not be blank"), blank.getMessage());
        Assert.expectThrows(NullPointerException.class, () -> SHAFT.Auth.setup("someUser", null));
        Assert.expectThrows(IllegalArgumentException.class, () -> SHAFT.Auth.setup("  ", driver -> {
        }));

        String path = SHAFT.Auth.stateFile("standardUser");
        Assert.assertEquals(path, tempDir.resolve("standardUser.json").toString());
        Files.writeString(Path.of(path), "{}");
        AtomicInteger calls = new AtomicInteger();
        String result = SHAFT.Auth.setup("standardUser", driver -> calls.incrementAndGet());
        Assert.assertEquals(result, path);
        Assert.assertEquals(calls.get(), 0);
    }

    @Test
    public void locatorsReportsAndFilesStayLocal() throws IOException {
        ShaftLocator role = SHAFT.GUI.Locator.role(" button ", "Save");
        Assert.assertEquals(role.strategy(), ShaftLocator.Strategy.ROLE);
        Assert.assertEquals(role.value(), "button");
        Assert.assertEquals(role.secondaryValue(), "Save");
        ShaftLocator name = SHAFT.GUI.Locator.accessibleName("Save");
        Assert.assertEquals(name.strategy(), ShaftLocator.Strategy.ACCESSIBLE_NAME);
        Assert.assertEquals(name.value(), "Save");
        IllegalArgumentException blankRole = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> SHAFT.GUI.Locator.role(" ", "Save"));
        Assert.assertTrue(blankRole.getMessage().contains("non-blank"), blankRole.getMessage());

        SHAFT.Report.log("offline log");
        SHAFT.Report.report("offline step");
        SHAFT.Report.report("offline failed step", Status.FAILED);
        SHAFT.Report.attach("text/plain", "note.txt", "hello");
        SHAFT.Report.attach("application/octet-stream", "note.bin", new ByteArrayInputStream(new byte[] {1, 2}));

        Path folder = tempDir.resolve("files");
        Files.createDirectories(folder);
        FileActions files = SHAFT.CLI.file();
        files.writeToFile(folder.toString(), "note.txt", "kept-offline");
        Assert.assertEquals(files.readFile(folder.toString(), "note.txt"), "kept-offline");
    }

    private static BrowserActionsContract browserWith(Set<org.openqa.selenium.Cookie> cookies) {
        BrowserActionsContract browser = mock(BrowserActionsContract.class);
        when(browser.getAllCookies()).thenReturn(new LinkedHashSet<>(cookies));
        return browser;
    }

    private static BrowserActionsContract recordingBrowser(Map<String, String> received) {
        BrowserActionsContract browser = mock(BrowserActionsContract.class);
        when(browser.addCookie(anyString(), anyString())).thenAnswer(invocation -> {
            received.put(invocation.getArgument(0), invocation.getArgument(1));
            return browser;
        });
        return browser;
    }
}
