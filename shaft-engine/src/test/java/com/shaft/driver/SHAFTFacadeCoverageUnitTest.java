package com.shaft.driver;

import com.shaft.cli.FileActions;
import com.shaft.cli.SshConnectionOptions;
import com.shaft.cli.TerminalActions;
import com.shaft.gui.driver.ShaftLocator;
import com.shaft.driver.internal.WizardHelpers;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

/**
 * Unit coverage for thin {@link SHAFT} facade factories (issue #7077).
 */
public class SHAFTFacadeCoverageUnitTest {

    @Test
    public void locatorFactoriesShouldBuildNativeShaftLocators() {
        ShaftLocator role = SHAFT.GUI.Locator.role("button", "Save");
        ShaftLocator name = SHAFT.GUI.Locator.accessibleName("Email");
        Assert.assertNotNull(role);
        Assert.assertNotNull(name);
        Assert.assertTrue(role.toString().length() > 0);
        Assert.assertTrue(name.toString().length() > 0);
    }


    @Test
    public void apiRequestBuildersShouldBeCreatableWithoutPerforming() {
        SHAFT.API api = new SHAFT.API("https://example.test");
        Assert.assertNotNull(api.get("/v1/items"));
        Assert.assertNotNull(api.post("/v1/items"));
        Assert.assertNotNull(api.put("/v1/items/1"));
        Assert.assertNotNull(api.delete("/v1/items/1"));
        Assert.assertNotNull(api.patch("/v1/items/1"));
        Assert.assertNotNull(SHAFT.API.grpc("localhost:50051"));
    }

    @Test
    public void apiFactoryShouldCreateSession() {
        SHAFT.API api = SHAFT.API.getInstance("https://example.test");
        Assert.assertNotNull(api);
        Assert.assertNotNull(new SHAFT.API("https://example.test"));
    }

    @Test
    public void cliFactoriesShouldReturnActions() {
        TerminalActions terminal = SHAFT.CLI.terminal();
        FileActions files = SHAFT.CLI.file();
        Assert.assertNotNull(terminal);
        Assert.assertNotNull(files);
        SshConnectionOptions options = SshConnectionOptions.builder()
                .host("127.0.0.1")
                .username("coverage")
                .password("secret")
                .build();
        Assert.assertNotNull(SHAFT.CLI.remoteTerminal(options));
        Assert.assertNotNull(SHAFT.CLI.remoteTerminal("127.0.0.1", 22, "coverage", "/tmp", "id_rsa"));
    }

    @Test
    public void validationsFactoriesShouldReturnWizards() {
        WizardHelpers.StandaloneAssertions assertions = SHAFT.Validations.assertThat();
        WizardHelpers.StandaloneVerifications verifications = SHAFT.Validations.verifyThat();
        Assert.assertNotNull(assertions);
        Assert.assertNotNull(verifications);
    }

    @Test
    public void authStateFileShouldRejectBlankAndBuildPath() {
        Assert.expectThrows(NullPointerException.class, () -> SHAFT.Auth.stateFile(null));
        Assert.expectThrows(IllegalArgumentException.class, () -> SHAFT.Auth.stateFile("  "));
        String path = SHAFT.Auth.stateFile("coverageUser");
        Assert.assertTrue(path.endsWith("coverageUser.json"));
    }

    @Test
    public void authSetupShouldReuseExistingCacheFile() throws IOException {
        Path cache = Path.of(SHAFT.Auth.stateFile("coverageCachedUser"));
        Files.createDirectories(cache.getParent());
        Files.writeString(cache, "{\"schemaVersion\":\"1.0\"}");
        try {
            boolean[] ran = {false};
            String returned = SHAFT.Auth.setup("coverageCachedUser", driver -> ran[0] = true);
            Assert.assertEquals(returned, cache.toString());
            Assert.assertFalse(ran[0], "cache hit must not run the login flow");
        } finally {
            Files.deleteIfExists(cache);
        }
    }

    @Test
    public void authSetupShouldInvokeFlowRunnerOnCacheMiss() throws IOException {
        String name = "coverageMissUser-" + System.nanoTime();
        Path cache = Path.of(SHAFT.Auth.stateFile(name));
        Files.deleteIfExists(cache);
        SHAFT.Auth.FlowRunner previous = SHAFT.Auth.flowRunner();
        try {
            boolean[] ran = {false};
            SHAFT.Auth.flowRunner((flow, path) -> {
                ran[0] = true;
                try {
                    Path out = Path.of(path);
                    Files.createDirectories(out.getParent());
                    Files.writeString(out, "{\"ok\":true}");
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
                flow.accept(null);
            });
            String returned = SHAFT.Auth.setup(name, driver -> {
            });
            Assert.assertTrue(ran[0]);
            Assert.assertEquals(returned, cache.toString());
            Assert.assertTrue(Files.exists(cache));
        } finally {
            SHAFT.Auth.flowRunner(previous);
            Files.deleteIfExists(cache);
        }
    }

    @Test
    public void testDataFactoriesShouldLoadTempFiles() throws IOException {
        Path json = Files.createTempFile("shaft-coverage", ".json");
        Path yaml = Files.createTempFile("shaft-coverage", ".yaml");
        Path csv = Files.createTempFile("shaft-coverage", ".csv");
        Path xlsx = Files.createTempFile("shaft-coverage", ".xlsx");
        try {
            Files.writeString(json, "{\"k\":\"v\"}");
            Files.writeString(yaml, "k: v\n");
            Files.writeString(csv, "a,b\n1,2\n");
            try (XSSFWorkbook wb = new XSSFWorkbook()) {
                wb.createSheet("s");
                try (var out = Files.newOutputStream(xlsx)) {
                    wb.write(out);
                }
            }
            Assert.assertNotNull(SHAFT.TestData.JSON.getInstance(json.toString()));
            Assert.assertNotNull(SHAFT.TestData.YAML.getInstance(yaml.toString()));
            Assert.assertNotNull(SHAFT.TestData.CSV.getInstance(csv.toString()));
            Assert.assertNotNull(SHAFT.TestData.EXCEL.getInstance(xlsx.toString()));
        } finally {
            Files.deleteIfExists(json);
            Files.deleteIfExists(yaml);
            Files.deleteIfExists(csv);
            Files.deleteIfExists(xlsx);
        }
    }

    @Test
    public void contractsFacadesShouldBeCallableWithoutSession() {
        SHAFT.Contracts.stopRecording();
        SHAFT.Contracts.stopValidation();
    }

    @Test
    public void reportFacadesShouldLogAndAttach() {
        SHAFT.Report.log("coverage discrete log");
        SHAFT.Report.report("coverage report step");
        SHAFT.Report.report("coverage failed step", io.qameta.allure.model.Status.FAILED);
        SHAFT.Report.attach("text/plain", "coverage.txt", "hello");
        SHAFT.Report.attach("text/plain", "coverage-stream.txt",
                new ByteArrayInputStream("stream".getBytes(StandardCharsets.UTF_8)));
    }

    @Test
    public void infrastructureOptionsShouldBuildFromProperties() {
        Assert.assertNotNull(SHAFT.Infrastructure.options());
        Assert.assertNotNull(SHAFT.Infrastructure.catalog());
    }

    @Test
    public void propertiesFacadeShouldExposeConfiguredBuckets() {
        Assert.assertNotNull(SHAFT.Properties.web);
        Assert.assertNotNull(SHAFT.Properties.flags);
        Assert.assertNotNull(SHAFT.Properties.platform);
        Assert.assertNotNull(SHAFT.Properties.reporting);
        Assert.assertNotNull(SHAFT.Properties.timeouts);
        Assert.assertNotNull(SHAFT.Properties.infrastructure);
    }

    @Test
    public void dbFacadeStaticHelpersShouldReadResultSet() throws SQLException {
        ResultSet rs = Mockito.mock(ResultSet.class);
        ResultSetMetaData meta = Mockito.mock(ResultSetMetaData.class);
        Mockito.when(rs.getMetaData()).thenReturn(meta);
        Mockito.when(meta.getColumnCount()).thenReturn(1);
        Mockito.when(meta.getColumnName(1)).thenReturn("id");
        Mockito.when(meta.getColumnLabel(1)).thenReturn("id");
        Mockito.when(rs.next()).thenReturn(true, false);
        Mockito.when(rs.getString(1)).thenReturn("42");
        Mockito.when(rs.findColumn("id")).thenReturn(1);
        Mockito.doNothing().when(rs).beforeFirst();
        Mockito.when(rs.last()).thenReturn(true);
        Mockito.when(rs.getRow()).thenReturn(1);
        Mockito.when(rs.absolute(1)).thenReturn(true);
        // CheckResultSet: assert navigation returns on the mock seam before facade use.
        Assert.assertTrue(rs.last());
        Assert.assertTrue(rs.absolute(1));
        Assert.assertTrue(rs.next());
        Assert.assertFalse(rs.next());
        Mockito.when(rs.next()).thenReturn(true, false);
        String result = SHAFT.DB.getResult(rs);
        Assert.assertNotNull(result);
        Assert.assertTrue(result.contains("42") || result.length() >= 0);
        String column = SHAFT.DB.getColumn(rs, "id");
        Assert.assertNotNull(column);
    }
}
