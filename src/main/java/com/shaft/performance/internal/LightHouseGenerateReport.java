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
            } else {
                commandToGenerateLightHouseReport = ("node GenerateLHScript.js --url=\"" + driver.getCurrentUrl() + "\" --port=" + PortNum + " --reportName=" + PageName + " ");
            }
            commandToGenerateLightHouseReport = commandToGenerateLightHouseReport.replace("&", "N898");
            //TerminalActions.getInstance(true, true).performTerminalCommand(commandToGenerateLightHouseReport);
            (new TerminalActions()).performTerminalCommand(commandToGenerateLightHouseReport);
            writeReportPathToFilesInProjectDirectory(PageName);
            openLighthouseReportWhileExecution();
            SHAFT.Report.report("Lighthouse Report Generated successfully");
            SHAFT.Report.attach("LightHouse HTML", "Report", FileActions.getInstance().readFile("lighthouse-reports/" + PageName + ".html"));
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
            commandsToServeLHReport = List.of("""
                    import puppeteer from 'puppeteer';
                    import fs from 'fs';
                    import lighthouse from 'lighthouse';
                    import optimist from 'optimist';
                    var argv =optimist.argv;
                    import open from 'open';
                    import path from 'path';
                    const __dirname = path.resolve();
                    import desktopConfig from 'lighthouse/core/config/desktop-config.js';
                    // -------- Configs ----------
                    var text = argv.url;
                    var Url = text.replaceAll("N898", "&");
                    //Url=Url.replace("'&'", "&");
                    var Port = argv.port;
                    var LogLevel='info';
                    var OutputType='html'; // html , json
                    var ReportName=argv.reportName;

                    //----------------------------

                    (async() => {

                    // Use Puppeteer to connect to the opened session by port
                       const browserURL = 'http://127.0.0.1:'+Port;
                       const browser = await puppeteer.connect({browserURL});
                       \s
                    // Lighthouse connect to the opened page and genrate the report.
                      const options = {logLevel:LogLevel ,output: OutputType, port:Port};
                      const runnerResult = await lighthouse(Url,options,desktopConfig);

                        // `Genrate the report output as HTML or JSON
                      const reportHtml = runnerResult.report;
                        // save the report in node.js path
                        fs.writeFileSync(__dirname +'/lighthouse-reports/'+ReportName+'.'+OutputType, reportHtml);
                        // Disconnect from the session
                        await browser.disconnect();
                    //    await open(__dirname +'/lighthouse-reports/'+ReportName+'.'+OutputType);

                    })();
                    """);
        }
        else {
            commandsToServeLHReport = List.of("""
                    import puppeteer from 'puppeteer';
                    import fs from 'fs';
                    import lighthouse from 'lighthouse';
                    import optimist from 'optimist';
                    var argv =optimist.argv;
                    import open from 'open';
                    import path from 'path';
                    const __dirname = path.resolve();
                    import desktopConfig from 'lighthouse/lighthouse-core/config/desktop-config.js';
                    // -------- Configs ----------
                    var text = argv.url;
                    var Url = text.replaceAll("N898", "&");
                    //Url=Url.replace("'&'", "&");
                    var Port = argv.port;
                    var LogLevel='info';
                    var OutputType='html'; argv.outputType; // html , json
                    var ReportName=argv.reportName;

                    //----------------------------

                    (async() => {

                    // Use Puppeteer to connect to the opened session by port
                       const browserURL = 'http://127.0.0.1:'+Port;
                       const browser = await puppeteer.connect({browserURL});
                       \s
                    // Lighthouse connect to the opened page and genrate the report.
                      const options = {logLevel:LogLevel ,output: OutputType, port:Port};
                      const runnerResult = await lighthouse(Url,options,desktopConfig);

                        // `Genrate the report output as HTML or JSON
                      const reportHtml = runnerResult.report;
                        // save the report in node.js path
                        fs.writeFileSync(__dirname +'/lighthouse-reports/'+ReportName+'.'+OutputType, reportHtml);
                        // Disconnect from the session
                        await browser.disconnect();
                    //    await open(__dirname +'/lighthouse-reports/'+ReportName+'.'+OutputType);

                    })();
                    """);
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
