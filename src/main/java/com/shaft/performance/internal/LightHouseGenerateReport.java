package com.shaft.performance.internal;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import org.apache.commons.lang3.SystemUtils;
import org.openqa.selenium.WebDriver;

import java.net.MalformedURLException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.List;

public class LightHouseGenerateReport {
    WebDriver driver;
    String PortNum;
    String PageName;
    public LightHouseGenerateReport(WebDriver driver) {
        this.driver = driver;
        }

    public void generateLightHouseReport() {
        PortNum=System.getProperty("lightHouseExeution.port");
        PageName=getPageName();

        String commandToGenerateLightHouseReport;
        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("lightHouseExeution").trim()))) {
            if (SystemUtils.IS_OS_WINDOWS) {
                commandToGenerateLightHouseReport = ("cmd.exe /c node GenerateLHScript.js --url=\"" +driver.getCurrentUrl()+ "\" --port="+ PortNum + " --reportName=" + PageName + " ");
                commandToGenerateLightHouseReport=commandToGenerateLightHouseReport.replace("&","N898");
            } else {
                commandToGenerateLightHouseReport = ("node GenerateLHScript.js --url=\""+ driver.getCurrentUrl()+ "\" --port=" + PortNum + " --reportName=" + PageName +" ");
                commandToGenerateLightHouseReport=commandToGenerateLightHouseReport.replace("&","N898");
            }
            //TerminalActions.getInstance(true, true).performTerminalCommand(commandToGenerateLightHouseReport);
            ( new TerminalActions()).performTerminalCommand(commandToGenerateLightHouseReport);
            writeReportPathToFilesInProjectDirectory(PageName);
            openLighthouseReportWhileExecution();
        }
    }

        public  void openLighthouseReportWhileExecution() {
            String commandToOpenLighthouseReport;
            if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("openLighthouseReportWhileExecution").trim()))) {
                if (SystemUtils.IS_OS_WINDOWS) {
                    commandToOpenLighthouseReport = ("cmd.exe /c node OpenLHReport.js");
                } else {
                    commandToOpenLighthouseReport = ("node OpenLHReport.js");
                }
//                TerminalActions.getInstance(true, true).performTerminalCommand(commandToOpenLighthouseReport);
                ( new TerminalActions()).performTerminalCommand(commandToOpenLighthouseReport);
            }
    }

    public void writeReportPathToFilesInProjectDirectory(String pageName) {
    List<String> commandsToServeLHReport;
        // update OpenLHreport with the new report path
        commandsToServeLHReport = Arrays.asList(
                "import open from 'open';\n" +
                "import path from 'path';\n" +
                "const __dirname = path.resolve();\n" +
                "await open(__dirname +'/LH-reports/"+pageName+".html');\n" +
                "");
        FileActions.getInstance().writeToFile("", "OpenLHReport.js", commandsToServeLHReport);
    }

    public String getPageName(){
        String Pagename;
        String CurrentUrl;
        CurrentUrl = driver.getCurrentUrl();
        try {
            URL url = new URL(CurrentUrl);
            Pagename = url.getPath();
            Pagename = Pagename.replace("/", "-");
            return  (new SimpleDateFormat("dd-MM-yyyy_HH-mm-ss-SSSS-aaa")).format(System.currentTimeMillis())+ "-" + Pagename;
        } catch (MalformedURLException e) {
            e.printStackTrace();
//            return  (new SimpleDateFormat("dd-MM-yyyy_HH-mm-ss-SSSS-aaa")).format(System.currentTimeMillis())+ "-" + Pagename;
            return "Error Occurred while creating the requested page name";
        }
    }

}
