package testPackage.CopilotGeneratedTests;

import com.shaft.api.RequestBuilder;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.AsyncElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.Actions;
import com.shaft.tools.io.CSVFileManager;
import com.shaft.tools.io.ExcelFileManager;
import com.shaft.tools.io.JSONFileManager;
import com.shaft.tools.io.YAMLFileManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.RestValidationsBuilder;
import io.restassured.response.Response;
import org.mockito.Mockito;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

import java.io.InputStream;
import java.util.List;

public class UnitTestsSHAFT {
    @Test
    public void testSHAFTGUIWebDriverConstructorsAndMethods() {
        DriverFactory factory = Mockito.mock(DriverFactory.class);
        WebDriver seleniumDriver = Mockito.mock(WebDriver.class);
        MutableCapabilities capabilities = Mockito.mock(MutableCapabilities.class);
        Actions actions = Mockito.mock(Actions.class);
        TouchActions touchActions = Mockito.mock(TouchActions.class);
        BrowserActions browserActions = Mockito.mock(BrowserActions.class);
        AlertActions alertActions = Mockito.mock(AlertActions.class);
        AsyncElementActions asyncElementActions = Mockito.mock(AsyncElementActions.class);
        DriverFactoryHelper helper = Mockito.mock(DriverFactoryHelper.class);

        SHAFT.GUI.WebDriver driver = Mockito.mock(SHAFT.GUI.WebDriver.class);
        Mockito.when(driver.element()).thenReturn(actions);
        Mockito.when(driver.touch()).thenReturn(touchActions);
        Mockito.when(driver.browser()).thenReturn(browserActions);
        Mockito.when(driver.alert()).thenReturn(alertActions);
        Mockito.when(driver.getDriver()).thenReturn(seleniumDriver);
        Mockito.when(driver.async()).thenReturn(driver.new Async());
//        Mockito.when(driver.async().element()).thenReturn(new AsyncElementActions(helper));
        driver.quit();
    }

    @Test
    public void testSHAFTAPIConstructorsAndMethods() {
        RestActions restActions = Mockito.mock(RestActions.class);
        Response response = Mockito.mock(Response.class);
        RequestBuilder requestBuilder = Mockito.mock(RequestBuilder.class);
        RestValidationsBuilder validationsBuilder = Mockito.mock(RestValidationsBuilder.class);
        Mockito.when(restActions.getResponse()).thenReturn(response);
        Mockito.when(restActions.buildNewRequest(Mockito.anyString(), Mockito.any())).thenReturn(requestBuilder);
        SHAFT.API api = Mockito.mock(SHAFT.API.class);
        Mockito.when(api.get("service")).thenReturn(requestBuilder);
        Mockito.when(api.post("service")).thenReturn(requestBuilder);
        Mockito.when(api.patch("service")).thenReturn(requestBuilder);
        Mockito.when(api.delete("service")).thenReturn(requestBuilder);
        Mockito.when(api.put("service")).thenReturn(requestBuilder);
        Mockito.when(api.assertThatResponse()).thenReturn(validationsBuilder);
        Mockito.when(api.verifyThatResponse()).thenReturn(validationsBuilder);
        Mockito.when(api.getResponse()).thenReturn(response);
        Mockito.when(api.getResponseBody()).thenReturn("body");
        Mockito.when(api.getResponseStatusCode()).thenReturn(200);
        Mockito.when(api.getResponseTime()).thenReturn(100L);
        Mockito.when(api.getResponseJSONValue("jsonPath")).thenReturn("value");
        Mockito.when(api.getResponseJSONValueAsList("jsonPath")).thenReturn(List.of("value"));
        Mockito.when(api.getResponseXMLValue("xmlPath")).thenReturn("value");
        Mockito.when(api.getResponseXMLValueAsList("xmlPath")).thenReturn(List.of("value"));
        api.addHeader("key", "value");
        api.addCookie("key", "value");
    }

    @Test
    public void testSHAFTCLIConstructorsAndMethods() {
        // Do NOT instantiate or mock SHAFT.CLI (utility class)
        // Only call static methods directly
        TerminalActions terminal = SHAFT.CLI.terminal();
        FileActions file = SHAFT.CLI.file();
    }

    @Test
    public void testSHAFTValidationsConstructorsAndMethods() {
        // Do NOT instantiate or mock SHAFT.Validations (utility class)
        // Only call static methods directly
        SHAFT.Validations.assertThat();
        SHAFT.Validations.verifyThat();
    }

    @Test
    public void testSHAFTTestDataConstructorsAndMethods() {
        JSONFileManager jsonFileManager = Mockito.mock(JSONFileManager.class);
        ExcelFileManager excelFileManager = Mockito.mock(ExcelFileManager.class);
        CSVFileManager csvFileManager = Mockito.mock(CSVFileManager.class);
        YAMLFileManager yamlFileManager = Mockito.mock(YAMLFileManager.class);
        SHAFT.TestData.JSON json = Mockito.mock(SHAFT.TestData.JSON.class);
        SHAFT.TestData.EXCEL excel = Mockito.mock(SHAFT.TestData.EXCEL.class);
        SHAFT.TestData.CSV csv = Mockito.mock(SHAFT.TestData.CSV.class);
        SHAFT.TestData.YAML yaml = Mockito.mock(SHAFT.TestData.YAML.class);
        SHAFT.TestData.JSON.getInstance("credentials.json");
        SHAFT.TestData.EXCEL.getInstance("testSuite01/TestData.xlsx");
        SHAFT.TestData.CSV.getInstance("TestData.csv");
        SHAFT.TestData.YAML.getInstance("yaml/yaml_test_data.yaml");
    }

    @Test
    public void testSHAFTPropertiesConstructorsAndMethods() {
        SHAFT.Properties properties = Mockito.mock(SHAFT.Properties.class);
    }

    @Test
    public void testSHAFTReportConstructorsAndMethods() {
        // Do NOT instantiate or mock SHAFT.Report (utility class)
        ReportManagerHelper reportManagerHelper = Mockito.mock(ReportManagerHelper.class);
        InputStream inputStream = Mockito.mock(InputStream.class);
        SHAFT.Report.log("message");
        SHAFT.Report.report("message");
        SHAFT.Report.attach("type", "name", "content");
        SHAFT.Report.attach("type", "name", inputStream);
    }
}
