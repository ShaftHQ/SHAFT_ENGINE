package io.github.shafthq.shaft.performance.lighthouse;

import com.shaft.cli.TerminalActions;
import org.apache.commons.lang3.SystemUtils;
import org.openqa.selenium.WebDriver;

import java.net.MalformedURLException;
import java.net.URL;
import java.text.SimpleDateFormat;

public class LHGenerateReport {
    WebDriver driver;
    public LHGenerateReport(WebDriver driver) {
        this.driver = driver;
    }

    public void generateLightHouseReport() {
        String commandToGenerateLightHouseReport;
        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("lightHouseExeution").trim()))) {
            if (SystemUtils.IS_OS_WINDOWS) {
                commandToGenerateLightHouseReport = ("cmd.exe /c GenerateLHScript.js  --url=" + driver.getCurrentUrl() + " --port=" + System.getProperty("lightHouseExeution.Port") + "  --outputType=html --reportName=" + getPageName() + " ");
            } else {
                commandToGenerateLightHouseReport = ("node GenerateLHScript.js  --url=" + driver.getCurrentUrl() + " --port=" + System.getProperty("lightHouseExeution.Port") + "  --outputType=html --reportName=" + getPageName() + "");

            }
            //TerminalActions.getInstance(true, true).performTerminalCommand(commandToGenerateLightHouseReport);
            ( new TerminalActions()).performTerminalCommand(commandToGenerateLightHouseReport);
        }
    }

    public String getPageName(){
        String Pagename="";
        String CurrentUrl;

        CurrentUrl = driver.getCurrentUrl();
        try {
            URL url = new URL(CurrentUrl);
            Pagename = url.getPath();
            Pagename = Pagename.replace("/", "-");

            return  (new SimpleDateFormat("dd-MM-yyyy_HH-mm-ss-SSSS-aaa")).format(System.currentTimeMillis())+ "-" + Pagename;

        } catch (MalformedURLException e) {
            e.printStackTrace();
            return  (new SimpleDateFormat("dd-MM-yyyy_HH-mm-ss-SSSS-aaa")).format(System.currentTimeMillis())+ "-" + Pagename;
        }
    }

}
