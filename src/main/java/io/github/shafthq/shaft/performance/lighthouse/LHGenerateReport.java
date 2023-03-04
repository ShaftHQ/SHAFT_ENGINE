package io.github.shafthq.shaft.performance.lighthouse;

import com.shaft.cli.TerminalActions;
import org.openqa.selenium.WebDriver;

import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class LHGenerateReport {
    WebDriver driver;
    public LHGenerateReport(WebDriver driver) {
        this.driver = driver;
    }

    public void GenerateReport(String ReportType){
        (new TerminalActions())
                .performTerminalCommand("cmd.exe /c node GenerateLHScript.js  --url=" + driver.getCurrentUrl() + " --port=" + System.getProperty("googleChromePort") + "  --outputType=" + ReportType + " --reportName=" + getPageName() + " ");


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

    public String HTML(){
        String reportType="html";
        return reportType;
    }

    public String Json() {
        String reportType="json";
        return reportType;
    }

}
