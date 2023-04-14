package com.shaft.performance.internal;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import org.apache.commons.lang3.SystemUtils;
import org.openqa.selenium.WebDriver;

import java.net.MalformedURLException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.List;

public class LightHouseGenerateReport {
    WebDriver driver;
    int PortNum;
    String PageName;
    public LightHouseGenerateReport(WebDriver driver) {
        this.driver = driver;
        }

    public void generateLightHouseReport() {
        PortNum = SHAFT.Properties.performance.port();
        PageName = getPageName();

        String commandToGenerateLightHouseReport;
        if (SHAFT.Properties.performance.isEnabled()) {
            createLighthouseReportFolderInProjectDirectory();
            writeNodeScriptFileInProjectDirectory();

            if (SystemUtils.IS_OS_WINDOWS) {
                commandToGenerateLightHouseReport = ("cmd.exe /c node GenerateLHScript.js --url=\"" + driver.getCurrentUrl() + "\" --port=" + PortNum + " --reportName=" + PageName + " ");
                commandToGenerateLightHouseReport = commandToGenerateLightHouseReport.replace("&", "N898");
            } else {
                commandToGenerateLightHouseReport = ("node GenerateLHScript.js --url=\"" + driver.getCurrentUrl() + "\" --port=" + PortNum + " --reportName=" + PageName + " ");
                commandToGenerateLightHouseReport = commandToGenerateLightHouseReport.replace("&", "N898");
            }
            //TerminalActions.getInstance(true, true).performTerminalCommand(commandToGenerateLightHouseReport);
            (new TerminalActions()).performTerminalCommand(commandToGenerateLightHouseReport);
            writeReportPathToFilesInProjectDirectory(PageName);
             openLighthouseReportWhileExecution();
            SHAFT.Report.report("Lighthouse Report Generated successfully");
            SHAFT.Report.attach("LightHouse HTML", "Report",  FileActions.getInstance().readFile("lighthouse-reports/"+PageName+".html"));
        }
    }

    public void createLighthouseReportFolderInProjectDirectory() {
           FileActions.getInstance().createFolder("lighthouse-reports");
    }

    public  void openLighthouseReportWhileExecution() {
            String commandToOpenLighthouseReport;
            if (SHAFT.Properties.reporting.openLighthouseReportWhileExecution()) {
                if (SystemUtils.IS_OS_WINDOWS) {
                    commandToOpenLighthouseReport = ("cmd.exe /c node OpenLHReport.js");
                } else {
                    commandToOpenLighthouseReport = ("node OpenLHReport.js");
                }
//                TerminalActions.getInstance(true, true).performTerminalCommand(commandToOpenLighthouseReport);
                (new TerminalActions()).performTerminalCommand(commandToOpenLighthouseReport);
                SHAFT.Report.report("Lighthouse Report Opened in new tab successfully");
            }
    }

    public void writeReportPathToFilesInProjectDirectory(String pageName) {
    List<String> commandsToServeLHReport;
        commandsToServeLHReport = List.of(
                "import open from 'open';\n" +
                        "import path from 'path';\n" +
                        "const __dirname = path.resolve();\n" +
                        "await open(__dirname +'/lighthouse-reports/" + pageName + ".html');\n");
        FileActions.getInstance().writeToFile("", "OpenLHReport.js", commandsToServeLHReport);
    }

    public void writeNodeScriptFileInProjectDirectory() {
        List<String> commandsToServeLHReport;
        if (SystemUtils.IS_OS_WINDOWS) {
            commandsToServeLHReport = List.of("import puppeteer from 'puppeteer';\n" +
                    "import fs from 'fs';\n" +
                    "import lighthouse from 'lighthouse';\n" +
                    "import optimist from 'optimist';\n" +
                    "var argv =optimist.argv;\n" +
                    "import open from 'open';\n" +
                    "import path from 'path';\n" +
                    "const __dirname = path.resolve();\n" +
                    "import desktopConfig from 'lighthouse/core/config/desktop-config.js';\n" +
                    "// -------- Configs ----------\n" +
                    "var text = argv.url;\n" +
                "var Url = text.replaceAll(\"N898\", \"&\");\n" +
                "//Url=Url.replace(\"'&'\", \"&\");\n" +
                "var Port = argv.port;\n" +
                "var LogLevel='info';\n" +
                "var OutputType='html'; // html , json\n" +
                "var ReportName=argv.reportName;\n" +
                "\n" +
                "//----------------------------\n" +
                "\n" +
                "(async() => {\n" +
                "\n" +
                "// Use Puppeteer to connect to the opened session by port\n" +
                "   const browserURL = 'http://127.0.0.1:'+Port;\n" +
                "   const browser = await puppeteer.connect({browserURL});\n" +
                "    \n" +
                "// Lighthouse connect to the opened page and genrate the report.\n" +
                "  const options = {logLevel:LogLevel ,output: OutputType, port:Port};\n" +
                "  const runnerResult = await lighthouse(Url,options,desktopConfig);\n" +
                "\n" +
                "    // `Genrate the report output as HTML or JSON\n" +
                "  const reportHtml = runnerResult.report;\n" +
                "    // save the report in node.js path\n" +
                "    fs.writeFileSync(__dirname +'/lighthouse-reports/'+ReportName+'.'+OutputType, reportHtml);\n" +
                "    // Disconnect from the session\n" +
                "    await browser.disconnect();\n" +
                "//    await open(__dirname +'/lighthouse-reports/'+ReportName+'.'+OutputType);\n" +
                "\n" +
                "})();\n"); }
        else {
            commandsToServeLHReport = List.of("import puppeteer from 'puppeteer';\n" +
                    "import fs from 'fs';\n" +
                    "import lighthouse from 'lighthouse';\n" +
                    "import optimist from 'optimist';\n" +
                    "var argv =optimist.argv;\n" +
                    "import open from 'open';\n" +
                    "import path from 'path';\n" +
                    "const __dirname = path.resolve();\n" +
                    "import desktopConfig from 'lighthouse/lighthouse-core/config/desktop-config.js';\n" +
                    "// -------- Configs ----------\n" +
                    "var text = argv.url;\n" +
                    "var Url = text.replaceAll(\"N898\", \"&\");\n" +
                    "//Url=Url.replace(\"'&'\", \"&\");\n" +
                    "var Port = argv.port;\n" +
                    "var LogLevel='info';\n" +
                    "var OutputType='html'; argv.outputType; // html , json\n" +
                    "var ReportName=argv.reportName;\n" +
                    "\n" +
                    "//----------------------------\n" +
                    "\n" +
                    "(async() => {\n" +
                    "\n" +
                    "// Use Puppeteer to connect to the opened session by port\n" +
                    "   const browserURL = 'http://127.0.0.1:'+Port;\n" +
                    "   const browser = await puppeteer.connect({browserURL});\n" +
                    "    \n" +
                    "// Lighthouse connect to the opened page and genrate the report.\n" +
                    "  const options = {logLevel:LogLevel ,output: OutputType, port:Port};\n" +
                    "  const runnerResult = await lighthouse(Url,options,desktopConfig);\n" +
                    "\n" +
                    "    // `Genrate the report output as HTML or JSON\n" +
                    "  const reportHtml = runnerResult.report;\n" +
                    "    // save the report in node.js path\n" +
                    "    fs.writeFileSync(__dirname +'/lighthouse-reports/'+ReportName+'.'+OutputType, reportHtml);\n" +
                    "    // Disconnect from the session\n" +
                    "    await browser.disconnect();\n" +
                    "//    await open(__dirname +'/lighthouse-reports/'+ReportName+'.'+OutputType);\n" +
                    "\n" +
                    "})();\n");
        }
        FileActions.getInstance().writeToFile("", "GenerateLHScript.js", commandsToServeLHReport);
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
