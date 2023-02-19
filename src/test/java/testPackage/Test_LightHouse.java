package testPackage;

import com.shaft.cli.TerminalActions;
import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.edge.EdgeOptions;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class Test_LightHouse {
    WebDriver driver;
    @BeforeClass
    public void beforeClass() {

        if(System.getProperty("targetBrowserName").contains("GoogleChrome")) {
            ChromeOptions chromeOption = new ChromeOptions();
            chromeOption.addArguments("--remote-debugging-port=" + System.getProperty("googleChromePort") + "");
            chromeOption.addArguments("--start-maximized");
            chromeOption.addArguments("--no-sandbox");
            chromeOption.setHeadless(Boolean.parseBoolean(System.getProperty("headlessExecution")));

            driver = DriverFactory.getDriver(DriverFactory.DriverType.DESKTOP_CHROME, chromeOption);
        }

        else if(System.getProperty("targetBrowserName").contains("MicrosoftEdge"))
        {
            EdgeOptions edgeOptions = new EdgeOptions();
            edgeOptions.addArguments("--remote-debugging-port=" + System.getProperty("googleChromePort") + "");
            edgeOptions.addArguments("--start-maximized");
            edgeOptions.setHeadless(Boolean.parseBoolean(System.getProperty("headlessExecution")));

            driver = DriverFactory.getDriver(DriverFactory.DriverType.DESKTOP_EDGE,edgeOptions);
        }

    }

    @Test (description = " Generate Lighthouse report for Google.com ")
    public void RunLightHouseGoogleSearch() {
        BrowserActions.navigateToURL(driver, "https://www.google.com/search?q=shaft_engine");

        (new TerminalActions())
         .performTerminalCommand("node GenerateLHScript.js  --url=" + driver.getCurrentUrl() + " --port=" + System.getProperty("googleChromePort") + "  --outputType=" + ReportType_HTML() + " --reportName=" + getPageName() + " ");
    }

    @AfterClass
    public void afterClass() {
        driver.close();
    }


    public String getPageName(){
        String Pagename="";
        String CurrentUrl;

        DateFormat dateFormat = new SimpleDateFormat("MM-dd-yyyy-HH-mm-ss");
        //get current date time with Date()
        Date date = new Date();
        // Now format the date
        String CurrentDate = dateFormat.format(date);
        CurrentUrl = driver.getCurrentUrl();

        try {
            URL url = new URL(CurrentUrl);
            Pagename = url.getPath();
            Pagename = Pagename.replace("/", "-");

            return  CurrentDate+ "-" + Pagename;
        } catch (MalformedURLException e) {
            e.printStackTrace();
            return  CurrentDate+ "-" + Pagename;
        }
    }

    public String ReportType_HTML(){
        String reportType="html";
        return reportType;
    }

    public String ReportType_Json() {
        String reportType="json";
        return reportType;
    }
}
