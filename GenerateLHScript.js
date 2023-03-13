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
var Url = argv.url;
var Port = argv.port;
var LogLevel='info';
var OutputType=argv.outputType; // html , json
var ReportName=argv.reportName;

//----------------------------

(async() => {

// Use Puppeteer to connect to the opened session by port
   const browserURL = 'http://127.0.0.1:'+Port;
   const browser = await puppeteer.connect({browserURL});
    
// Lighthouse connect to the opened page and genrate the report.
  const options = {logLevel:LogLevel ,output: OutputType, port:Port};
  const runnerResult = await lighthouse(Url,options,desktopConfig);

    // `Genrate the report output as HTML or JSON
  const reportHtml = runnerResult.report;
    // save the report in node.js path
    fs.writeFileSync(__dirname +'/LH-reports/'+ReportName+'.'+OutputType, reportHtml);
    // Disconnect from the session
    await browser.disconnect();
    await open(__dirname +'/LH-reports/'+ReportName+'.'+OutputType, reportHtml);

})();
