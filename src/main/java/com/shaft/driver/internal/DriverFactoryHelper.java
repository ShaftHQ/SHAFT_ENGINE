package com.shaft.driver.internal;

import com.epam.healenium.SelfHealingDriver;
import com.google.common.base.Throwables;
import com.mysql.cj.util.StringUtils;
import com.shaft.driver.DriverFactory.DriverType;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.internal.video.RecordManager;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertiesHelper;
import com.shaft.properties.internal.PropertyFileManager;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.Setting;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.remote.options.UnhandledPromptBehavior;
import io.github.bonigarcia.wdm.WebDriverManager;
import io.github.bonigarcia.wdm.config.WebDriverManagerException;
import io.qameta.allure.Step;
import lombok.*;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.*;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.chromium.ChromiumOptions;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.v116.network.Network;
import org.openqa.selenium.edge.EdgeDriver;
import org.openqa.selenium.edge.EdgeOptions;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.firefox.FirefoxDriverLogLevel;
import org.openqa.selenium.firefox.FirefoxOptions;
import org.openqa.selenium.firefox.FirefoxProfile;
import org.openqa.selenium.ie.InternetExplorerDriver;
import org.openqa.selenium.ie.InternetExplorerOptions;
import org.openqa.selenium.logging.LogType;
import org.openqa.selenium.logging.LoggingPreferences;
import org.openqa.selenium.remote.*;
import org.openqa.selenium.safari.SafariDriver;
import org.openqa.selenium.safari.SafariOptions;
import org.testng.Reporter;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URI;
import java.time.Duration;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public class DriverFactoryHelper {
    // TODO: implement pass and fail actions to enable initial factory method screenshot and append it to animated GIF
    private static String TARGET_HUB_URL;
    private static final String WEB_DRIVER_MANAGER_MESSAGE = "Identifying OS/Driver combination. Please note that if a new browser/driver executable will be downloaded it may take some time depending on your connection...";
    private static final String WEB_DRIVER_MANAGER_DOCKERIZED_MESSAGE = "Identifying target OS/Browser and setting up the dockerized environment automatically. Please note that if a new docker container will be downloaded it may take some time depending on your connection...";
    @Getter(AccessLevel.PUBLIC)
    private static String targetBrowserName = "";
    @Getter(AccessLevel.PUBLIC)
    @Setter(AccessLevel.PUBLIC)
    private static WebDriver driver;
    private static final ThreadLocal<WebDriverManager> webDriverManager = new ThreadLocal<>();
    private static ChromeOptions chOptions;
    private static FirefoxOptions ffOptions;
    private static SafariOptions sfOptions;
    private static EdgeOptions edOptions;
    private static InternetExplorerOptions ieOptions;
    private static DesiredCapabilities appiumCapabilities;
    @Getter(AccessLevel.PUBLIC)
    private static boolean killSwitch = false;
    @Getter(AccessLevel.PUBLIC)
    private static final Dimension TARGET_WINDOW_SIZE = new Dimension(1920, 1080);

    private static final long appiumServerInitializationTimeout = TimeUnit.MINUTES.toSeconds(SHAFT.Properties.timeouts.timeoutForRemoteServerToBeUp()); // seconds
    private static final int appiumServerInitializationPollingInterval = 1; // seconds
    private static final long remoteServerInstanceCreationTimeout = TimeUnit.MINUTES.toSeconds(SHAFT.Properties.timeouts.remoteServerInstanceCreationTimeout()); // seconds
    private static final int appiumServerPreparationPollingInterval = 1; // seconds

    private DriverFactoryHelper() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Checks to see if the execution is a mobile-native execution
     *
     * @return true if it's a mobile mobile-native execution
     */
    public static boolean isMobileNativeExecution() {

        var isMobileExecution = Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())
                || Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform());
        var isNativeExecution = SHAFT.Properties.mobile.browserName().isBlank();
        return isMobileExecution && isNativeExecution;
    }

    /**
     * Checks to see if the execution is a mobile-web execution
     *
     * @return true if it's a mobile mobile-web execution
     */
    public static boolean isMobileWebExecution() {
        var isMobileExecution = Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())
                || Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform());
        var isNativeExecution = SHAFT.Properties.mobile.browserName().isBlank();
        return isMobileExecution && !isNativeExecution;
    }

    /**
     * Checks to see if the execution is a web-based execution
     *
     * @return true if it's a web-based execution
     */
    public static boolean isWebExecution() {
        var isMobileExecution = Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())
                || Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform());
        return !isMobileExecution;
    }

    public static void closeDriver() {
        if (driver != null) {
            if (SHAFT.Properties.visuals.videoParamsScope().equals("DriverSession")) {
                RecordManager.attachVideoRecording();
            }
            try {
                attachWebDriverLogs();
                //if dockerized wdm.quit the relevant one
                if (SHAFT.Properties.platform.executionAddress().toLowerCase().contains("dockerized")) {
                    var pathToRecording = webDriverManager.get().getDockerRecordingPath(driver);
                    webDriverManager.get().quit(driver);
                    RecordManager.attachVideoRecording(pathToRecording);
                } else {
                    driver.quit();
                }
            } catch (WebDriverException | NullPointerException e) {
                // driver was already closed at an earlier stage
            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e);
            } finally {
                driver = null;
                webDriverManager.remove();
                ReportManager.log("Successfully Closed Driver.");
            }
        } else {
            ReportManager.log("Driver is already closed.");
        }
    }

    private static void failAction(String testData, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        String message = "Driver Factory Action \"" + actionName + "\" failed.";
        if (testData != null) {
            message = message + " With the following test data \"" + testData + "\".";
        }
        if (rootCauseException != null && rootCauseException.length >= 1) {
            FailureReporter.fail(DriverFactoryHelper.class, message, rootCauseException[0]);
        } else {
            FailureReporter.fail(message);
        }
    }

    private static DriverType getDriverTypeFromName(String driverName) {
        int values = DriverType.values().length;
        for (var i = 0; i < values; i++) {
            if (driverName.trim().toLowerCase().contains(Arrays.asList(DriverType.values()).get(i).getValue().toLowerCase())) {
                return Arrays.asList(DriverType.values()).get(i);
            }
        }
        failAction("Unsupported Driver Type \"" + driverName + "\".");
        return DriverType.CHROME;
    }
    private static void disableCacheEdgeAndChrome() {
        if (SHAFT.Properties.flags.disableCache())
        {
            DevTools devTools = ((HasDevTools) driver).getDevTools();
            devTools.createSessionIfThereIsNotOne();
            devTools.send(Network.enable(Optional.empty(), Optional.empty(), Optional.of(100000000)));
            devTools.send(Network.setCacheDisabled(true));
            devTools.send(Network.clearBrowserCookies());
            devTools.addListener(Network.responseReceived(), responseReceived -> {
                if (responseReceived.getResponse().getFromDiskCache().isPresent()
                        && responseReceived.getResponse().getFromDiskCache().get().equals(true)) {
                    failAction("Cache wasn't cleared");
                }
            });
        }
    }
    private static void setDriverOptions(DriverType driverType, MutableCapabilities customDriverOptions) {
        // get Proxy server settings | testing behind a proxy
        String proxyServerSettings = SHAFT.Properties.platform.proxy();

        //https://github.com/GoogleChrome/chrome-launcher/blob/master/docs/chrome-flags-for-tools.md#--enable-automation
        switch (driverType) {
            case FIREFOX -> {
                // https://wiki.mozilla.org/Firefox/CommandLineOptions
                // https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities/firefoxOptions
                ffOptions = new FirefoxOptions();
                var ffProfile = new FirefoxProfile();
                ffProfile.setPreference("browser.download.dir", System.getProperty("user.dir") + File.separatorChar + SHAFT.Properties.paths.downloads());
                ffProfile.setPreference("browser.download.folderList", 2);
                //noinspection SpellCheckingInspection
                ffProfile.setPreference("browser.helperApps.neverAsk.saveToDisk",
                        "application/vnd.hzn-3d-crossword;video/3gpp;video/3gpp2;application/vnd.mseq;application/vnd.3m.post-it-notes;application/vnd.3gpp.pic-bw-large;application/vnd.3gpp.pic-bw-small;application/vnd.3gpp.pic-bw-var;application/vnd.3gp2.tcap;application/x-7z-compressed;application/x-abiword;application/x-ace-compressed;application/vnd.americandynamics.acc;application/vnd.acucobol;application/vnd.acucorp;audio/adpcm;application/x-authorware-bin;application/x-athorware-map;application/x-authorware-seg;application/vnd.adobe.air-application-installer-package+zip;application/x-shockwave-flash;application/vnd.adobe.fxp;application/pdf;application/vnd.cups-ppd;application/x-director;applicaion/vnd.adobe.xdp+xml;application/vnd.adobe.xfdf;audio/x-aac;application/vnd.ahead.space;application/vnd.airzip.filesecure.azf;application/vnd.airzip.filesecure.azs;application/vnd.amazon.ebook;application/vnd.amiga.ami;applicatin/andrew-inset;application/vnd.android.package-archive;application/vnd.anser-web-certificate-issue-initiation;application/vnd.anser-web-funds-transfer-initiation;application/vnd.antix.game-component;application/vnd.apple.installe+xml;application/applixware;application/vnd.hhe.lesson-player;application/vnd.aristanetworks.swi;text/x-asm;application/atomcat+xml;application/atomsvc+xml;application/atom+xml;application/pkix-attr-cert;audio/x-aiff;video/x-msvieo;application/vnd.audiograph;image/vnd.dxf;model/vnd.dwf;text/plain-bas;application/x-bcpio;application/octet-stream;image/bmp;application/x-bittorrent;application/vnd.rim.cod;application/vnd.blueice.multipass;application/vnd.bm;application/x-sh;image/prs.btif;application/vnd.businessobjects;application/x-bzip;application/x-bzip2;application/x-csh;text/x-c;application/vnd.chemdraw+xml;text/css;chemical/x-cdx;chemical/x-cml;chemical/x-csml;application/vn.contact.cmsg;application/vnd.claymore;application/vnd.clonk.c4group;image/vnd.dvb.subtitle;application/cdmi-capability;application/cdmi-container;application/cdmi-domain;application/cdmi-object;application/cdmi-queue;applicationvnd.cluetrust.cartomobile-config;application/vnd.cluetrust.cartomobile-config-pkg;image/x-cmu-raster;model/vnd.collada+xml;text/csv;application/mac-compactpro;application/vnd.wap.wmlc;image/cgm;x-conference/x-cooltalk;image/x-cmx;application/vnd.xara;application/vnd.cosmocaller;application/x-cpio;application/vnd.crick.clicker;application/vnd.crick.clicker.keyboard;application/vnd.crick.clicker.palette;application/vnd.crick.clicker.template;application/vn.crick.clicker.wordbank;application/vnd.criticaltools.wbs+xml;application/vnd.rig.cryptonote;chemical/x-cif;chemical/x-cmdf;application/cu-seeme;application/prs.cww;text/vnd.curl;text/vnd.curl.dcurl;text/vnd.curl.mcurl;text/vnd.crl.scurl;application/vnd.curl.car;application/vnd.curl.pcurl;application/vnd.yellowriver-custom-menu;application/dssc+der;application/dssc+xml;application/x-debian-package;audio/vnd.dece.audio;image/vnd.dece.graphic;video/vnd.dec.hd;video/vnd.dece.mobile;video/vnd.uvvu.mp4;video/vnd.dece.pd;video/vnd.dece.sd;video/vnd.dece.video;application/x-dvi;application/vnd.fdsn.seed;application/x-dtbook+xml;application/x-dtbresource+xml;application/vnd.dvb.ait;applcation/vnd.dvb.service;audio/vnd.digital-winds;image/vnd.djvu;application/xml-dtd;application/vnd.dolby.mlp;application/x-doom;application/vnd.dpgraph;audio/vnd.dra;application/vnd.dreamfactory;audio/vnd.dts;audio/vnd.dts.hd;imag/vnd.dwg;application/vnd.dynageo;application/ecmascript;application/vnd.ecowin.chart;image/vnd.fujixerox.edmics-mmr;image/vnd.fujixerox.edmics-rlc;application/exi;application/vnd.proteus.magazine;application/epub+zip;message/rfc82;application/vnd.enliven;application/vnd.is-xpr;image/vnd.xiff;application/vnd.xfdl;application/emma+xml;application/vnd.ezpix-album;application/vnd.ezpix-package;image/vnd.fst;video/vnd.fvt;image/vnd.fastbidsheet;application/vn.denovo.fcselayout-link;video/x-f4v;video/x-flv;image/vnd.fpx;image/vnd.net-fpx;text/vnd.fmi.flexstor;video/x-fli;application/vnd.fluxtime.clip;application/vnd.fdf;text/x-fortran;application/vnd.mif;application/vnd.framemaker;imae/x-freehand;application/vnd.fsc.weblaunch;application/vnd.frogans.fnc;application/vnd.frogans.ltf;application/vnd.fujixerox.ddd;application/vnd.fujixerox.docuworks;application/vnd.fujixerox.docuworks.binder;application/vnd.fujitu.oasys;application/vnd.fujitsu.oasys2;application/vnd.fujitsu.oasys3;application/vnd.fujitsu.oasysgp;application/vnd.fujitsu.oasysprs;application/x-futuresplash;application/vnd.fuzzysheet;image/g3fax;application/vnd.gmx;model/vn.gtw;application/vnd.genomatix.tuxedo;application/vnd.geogebra.file;application/vnd.geogebra.tool;model/vnd.gdl;application/vnd.geometry-explorer;application/vnd.geonext;application/vnd.geoplan;application/vnd.geospace;applicatio/x-font-ghostscript;application/x-font-bdf;application/x-gtar;application/x-texinfo;application/x-gnumeric;application/vnd.google-earth.kml+xml;application/vnd.google-earth.kmz;application/vnd.grafeq;image/gif;text/vnd.graphviz;aplication/vnd.groove-account;application/vnd.groove-help;application/vnd.groove-identity-message;application/vnd.groove-injector;application/vnd.groove-tool-message;application/vnd.groove-tool-template;application/vnd.groove-vcar;video/h261;video/h263;video/h264;application/vnd.hp-hpid;application/vnd.hp-hps;application/x-hdf;audio/vnd.rip;application/vnd.hbci;application/vnd.hp-jlyt;application/vnd.hp-pcl;application/vnd.hp-hpgl;application/vnd.yamaha.h-script;application/vnd.yamaha.hv-dic;application/vnd.yamaha.hv-voice;application/vnd.hydrostatix.sof-data;application/hyperstudio;application/vnd.hal+xml;text/html;application/vnd.ibm.rights-management;application/vnd.ibm.securecontainer;text/calendar;application/vnd.iccprofile;image/x-icon;application/vnd.igloader;image/ief;application/vnd.immervision-ivp;application/vnd.immervision-ivu;application/reginfo+xml;text/vnd.in3d.3dml;text/vnd.in3d.spot;mode/iges;application/vnd.intergeo;application/vnd.cinderella;application/vnd.intercon.formnet;application/vnd.isac.fcs;application/ipfix;application/pkix-cert;application/pkixcmp;application/pkix-crl;application/pkix-pkipath;applicaion/vnd.insors.igm;application/vnd.ipunplugged.rcprofile;application/vnd.irepository.package+xml;text/vnd.sun.j2me.app-descriptor;application/java-archive;application/java-vm;application/x-java-jnlp-file;application/java-serializd-object;text/x-java-source,java;application/javascript;application/json;application/vnd.joost.joda-archive;video/jpm;image/jpeg;video/jpeg;application/vnd.kahootz;application/vnd.chipnuts.karaoke-mmd;application/vnd.kde.karbon;aplication/vnd.kde.kchart;application/vnd.kde.kformula;application/vnd.kde.kivio;application/vnd.kde.kontour;application/vnd.kde.kpresenter;application/vnd.kde.kspread;application/vnd.kde.kword;application/vnd.kenameaapp;applicatin/vnd.kidspiration;application/vnd.kinar;application/vnd.kodak-descriptor;application/vnd.las.las+xml;application/x-latex;application/vnd.llamagraphics.life-balance.desktop;application/vnd.llamagraphics.life-balance.exchange+xml;application/vnd.jam;application/vnd.lotus-1-2-3;application/vnd.lotus-approach;application/vnd.lotus-freelance;application/vnd.lotus-notes;application/vnd.lotus-organizer;application/vnd.lotus-screencam;application/vnd.lotus-wordro;audio/vnd.lucent.voice;audio/x-mpegurl;video/x-m4v;application/mac-binhex40;application/vnd.macports.portpkg;application/vnd.osgeo.mapguide.package;application/marc;application/marcxml+xml;application/mxf;application/vnd.wolfrm.player;application/mathematica;application/mathml+xml;application/mbox;application/vnd.medcalcdata;application/mediaservercontrol+xml;application/vnd.mediastation.cdkey;application/vnd.mfer;application/vnd.mfmp;model/mesh;appliation/mads+xml;application/mets+xml;application/mods+xml;application/metalink4+xml;application/vnd.ms-powerpoint.template.macroenabled.12;application/vnd.ms-word.document.macroenabled.12;application/vnd.ms-word.template.macroenabed.12;application/vnd.mcd;application/vnd.micrografx.flo;application/vnd.micrografx.igx;application/vnd.eszigno3+xml;application/x-msaccess;video/x-ms-asf;application/x-msdownload;application/vnd.ms-artgalry;application/vnd.ms-ca-compressed;application/vnd.ms-ims;application/x-ms-application;application/x-msclip;image/vnd.ms-modi;application/vnd.ms-fontobject;application/vnd.ms-excel;application/vnd.ms-excel.addin.macroenabled.12;application/vnd.ms-excelsheet.binary.macroenabled.12;application/vnd.ms-excel.template.macroenabled.12;application/vnd.ms-excel.sheet.macroenabled.12;application/vnd.ms-htmlhelp;application/x-mscardfile;application/vnd.ms-lrm;application/x-msmediaview;aplication/x-msmoney;application/vnd.openxmlformats-officedocument.presentationml.presentation;application/vnd.openxmlformats-officedocument.presentationml.slide;application/vnd.openxmlformats-officedocument.presentationml.slideshw;application/vnd.openxmlformats-officedocument.presentationml.template;application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;application/vnd.openxmlformats-officedocument.spreadsheetml.template;application/vnd.openxmformats-officedocument.wordprocessingml.document;application/vnd.openxmlformats-officedocument.wordprocessingml.template;application/x-msbinder;application/vnd.ms-officetheme;application/onenote;audio/vnd.ms-playready.media.pya;vdeo/vnd.ms-playready.media.pyv;application/vnd.ms-powerpoint;application/vnd.ms-powerpoint.addin.macroenabled.12;application/vnd.ms-powerpoint.slide.macroenabled.12;application/vnd.ms-powerpoint.presentation.macroenabled.12;appliation/vnd.ms-powerpoint.slideshow.macroenabled.12;application/vnd.ms-project;application/x-mspublisher;application/x-msschedule;application/x-silverlight-app;application/vnd.ms-pki.stl;application/vnd.ms-pki.seccat;application/vn.visio;video/x-ms-wm;audio/x-ms-wma;audio/x-ms-wax;video/x-ms-wmx;application/x-ms-wmd;application/vnd.ms-wpl;application/x-ms-wmz;video/x-ms-wmv;video/x-ms-wvx;application/x-msmetafile;application/x-msterminal;application/msword;application/x-mswrite;application/vnd.ms-works;application/x-ms-xbap;application/vnd.ms-xpsdocument;audio/midi;application/vnd.ibm.minipay;application/vnd.ibm.modcap;application/vnd.jcp.javame.midlet-rms;application/vnd.tmobile-ivetv;application/x-mobipocket-ebook;application/vnd.mobius.mbk;application/vnd.mobius.dis;application/vnd.mobius.plc;application/vnd.mobius.mqy;application/vnd.mobius.msl;application/vnd.mobius.txf;application/vnd.mobius.daf;tex/vnd.fly;application/vnd.mophun.certificate;application/vnd.mophun.application;video/mj2;audio/mpeg;video/vnd.mpegurl;video/mpeg;application/mp21;audio/mp4;video/mp4;application/mp4;application/vnd.apple.mpegurl;application/vnd.msician;application/vnd.muvee.style;application/xv+xml;application/vnd.nokia.n-gage.data;application/vnd.nokia.n-gage.symbian.install;application/x-dtbncx+xml;application/x-netcdf;application/vnd.neurolanguage.nlu;application/vnd.na;application/vnd.noblenet-directory;application/vnd.noblenet-sealer;application/vnd.noblenet-web;application/vnd.nokia.radio-preset;application/vnd.nokia.radio-presets;text/n3;application/vnd.novadigm.edm;application/vnd.novadim.edx;application/vnd.novadigm.ext;application/vnd.flographit;audio/vnd.nuera.ecelp4800;audio/vnd.nuera.ecelp7470;audio/vnd.nuera.ecelp9600;application/oda;application/ogg;audio/ogg;video/ogg;application/vnd.oma.dd2+xml;applicatin/vnd.oasis.opendocument.text-web;application/oebps-package+xml;application/vnd.intu.qbo;application/vnd.openofficeorg.extension;application/vnd.yamaha.openscoreformat;audio/webm;video/webm;application/vnd.oasis.opendocument.char;application/vnd.oasis.opendocument.chart-template;application/vnd.oasis.opendocument.database;application/vnd.oasis.opendocument.formula;application/vnd.oasis.opendocument.formula-template;application/vnd.oasis.opendocument.grapics;application/vnd.oasis.opendocument.graphics-template;application/vnd.oasis.opendocument.image;application/vnd.oasis.opendocument.image-template;application/vnd.oasis.opendocument.presentation;application/vnd.oasis.opendocumen.presentation-template;application/vnd.oasis.opendocument.spreadsheet;application/vnd.oasis.opendocument.spreadsheet-template;application/vnd.oasis.opendocument.text;application/vnd.oasis.opendocument.text-master;application/vnd.asis.opendocument.text-template;image/ktx;application/vnd.sun.xml.calc;application/vnd.sun.xml.calc.template;application/vnd.sun.xml.draw;application/vnd.sun.xml.draw.template;application/vnd.sun.xml.impress;application/vnd.sun.xl.impress.template;application/vnd.sun.xml.math;application/vnd.sun.xml.writer;application/vnd.sun.xml.writer.global;application/vnd.sun.xml.writer.template;application/x-font-otf;application/vnd.yamaha.openscoreformat.osfpvg+xml;application/vnd.osgi.dp;application/vnd.palm;text/x-pascal;application/vnd.pawaafile;application/vnd.hp-pclxl;application/vnd.picsel;image/x-pcx;image/vnd.adobe.photoshop;application/pics-rules;image/x-pict;application/x-chat;aplication/pkcs10;application/x-pkcs12;application/pkcs7-mime;application/pkcs7-signature;application/x-pkcs7-certreqresp;application/x-pkcs7-certificates;application/pkcs8;application/vnd.pocketlearn;image/x-portable-anymap;image/-portable-bitmap;application/x-font-pcf;application/font-tdpfr;application/x-chess-pgn;image/x-portable-graymap;image/png;image/x-portable-pixmap;application/pskc+xml;application/vnd.ctc-posml;application/postscript;application/xfont-type1;application/vnd.powerbuilder6;application/pgp-encrypted;application/pgp-signature;application/vnd.previewsystems.box;application/vnd.pvi.ptid1;application/pls+xml;application/vnd.pg.format;application/vnd.pg.osasli;tex/prs.lines.tag;application/x-font-linux-psf;application/vnd.publishare-delta-tree;application/vnd.pmi.widget;application/vnd.quark.quarkxpress;application/vnd.epson.esf;application/vnd.epson.msf;application/vnd.epson.ssf;applicaton/vnd.epson.quickanime;application/vnd.intu.qfx;video/quicktime;application/x-rar-compressed;audio/x-pn-realaudio;audio/x-pn-realaudio-plugin;application/rsd+xml;application/vnd.rn-realmedia;application/vnd.realvnc.bed;applicatin/vnd.recordare.musicxml;application/vnd.recordare.musicxml+xml;application/relax-ng-compact-syntax;application/vnd.data-vision.rdz;application/rdf+xml;application/vnd.cloanto.rp9;application/vnd.jisp;application/rtf;text/richtex;application/vnd.route66.link66+xml;application/rss+xml;application/shf+xml;application/vnd.sailingtracker.track;image/svg+xml;application/vnd.sus-calendar;application/sru+xml;application/set-payment-initiation;application/set-reistration-initiation;application/vnd.sema;application/vnd.semd;application/vnd.semf;application/vnd.seemail;application/x-font-snf;application/scvp-vp-request;application/scvp-vp-response;application/scvp-cv-request;application/svp-cv-response;application/sdp;text/x-setext;video/x-sgi-movie;application/vnd.shana.informed.formdata;application/vnd.shana.informed.formtemplate;application/vnd.shana.informed.interchange;application/vnd.shana.informed.package;application/thraud+xml;application/x-shar;image/x-rgb;application/vnd.epson.salt;application/vnd.accpac.simply.aso;application/vnd.accpac.simply.imp;application/vnd.simtech-mindmapper;application/vnd.commonspace;application/vnd.ymaha.smaf-audio;application/vnd.smaf;application/vnd.yamaha.smaf-phrase;application/vnd.smart.teacher;application/vnd.svd;application/sparql-query;application/sparql-results+xml;application/srgs;application/srgs+xml;application/sml+xml;application/vnd.koan;text/sgml;application/vnd.stardivision.calc;application/vnd.stardivision.draw;application/vnd.stardivision.impress;application/vnd.stardivision.math;application/vnd.stardivision.writer;application/vnd.tardivision.writer-global;application/vnd.stepmania.stepchart;application/x-stuffit;application/x-stuffitx;application/vnd.solent.sdkm+xml;application/vnd.olpc-sugar;audio/basic;application/vnd.wqd;application/vnd.symbian.install;application/smil+xml;application/vnd.syncml+xml;application/vnd.syncml.dm+wbxml;application/vnd.syncml.dm+xml;application/x-sv4cpio;application/x-sv4crc;application/sbml+xml;text/tab-separated-values;image/tiff;application/vnd.to.intent-module-archive;application/x-tar;application/x-tcl;application/x-tex;application/x-tex-tfm;application/tei+xml;text/plain;application/vnd.spotfire.dxp;application/vnd.spotfire.sfs;application/timestamped-data;applicationvnd.trid.tpt;application/vnd.triscape.mxs;text/troff;application/vnd.trueapp;application/x-font-ttf;text/turtle;application/vnd.umajin;application/vnd.uoml+xml;application/vnd.unity;application/vnd.ufdl;text/uri-list;application/nd.uiq.theme;application/x-ustar;text/x-uuencode;text/x-vcalendar;text/x-vcard;application/x-cdlink;application/vnd.vsf;model/vrml;application/vnd.vcx;model/vnd.mts;model/vnd.vtu;application/vnd.visionary;video/vnd.vivo;applicatin/ccxml+xml,;application/voicexml+xml;application/x-wais-source;application/vnd.wap.wbxml;image/vnd.wap.wbmp;audio/x-wav;application/davmount+xml;application/x-font-woff;application/wspolicy+xml;image/webp;application/vnd.webturb;application/widget;application/winhlp;text/vnd.wap.wml;text/vnd.wap.wmlscript;application/vnd.wap.wmlscriptc;application/vnd.wordperfect;application/vnd.wt.stf;application/wsdl+xml;image/x-xbitmap;image/x-xpixmap;image/x-xwindowump;application/x-x509-ca-cert;application/x-xfig;application/xhtml+xml;application/xml;application/xcap-diff+xml;application/xenc+xml;application/patch-ops-error+xml;application/resource-lists+xml;application/rls-services+xml;aplication/resource-lists-diff+xml;application/xslt+xml;application/xop+xml;application/x-xpinstall;application/xspf+xml;application/vnd.mozilla.xul+xml;chemical/x-xyz;text/yaml;application/yang;application/yin+xml;application/vnd.ul;application/zip;application/vnd.handheld-entertainment+xml;application/vnd.zzazz.deck+xml");
                if (SHAFT.Properties.flags.disableCache()) {
                    ffProfile.setPreference("browser.cache.disk.enable", false);
                    ffProfile.setPreference("browser.cache.memory.enable", false);
                    ffProfile.setPreference("browser.cache.offline.enable", false);
                    ffProfile.setPreference("network.http.use-cache", false);
                }
                ffOptions.setProfile(ffProfile);
                if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local"))
                    ffOptions.setCapability(CapabilityType.PLATFORM_NAME, Properties.platform.targetPlatform());
                if (SHAFT.Properties.web.headlessExecution()) {
                    ffOptions.addArguments("-headless");
                }
                ffOptions.setLogLevel(FirefoxDriverLogLevel.WARN);
                ffOptions.setPageLoadStrategy(PageLoadStrategy.EAGER);
                ffOptions.setPageLoadTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.pageLoadTimeout()));
                ffOptions.setScriptTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.scriptExecutionTimeout()));
                //Add Proxy Setting if found
                if (SHAFT.Properties.platform.driverProxySettings() && !proxyServerSettings.isBlank()) {
                    Proxy proxy = new Proxy();
                    proxy.setHttpProxy(proxyServerSettings);
                    proxy.setSslProxy(proxyServerSettings);
                    ffOptions.setProxy(proxy);
                }
                // Enable BiDi
                ffOptions.setCapability("webSocketUrl", true);
                //merge customWebDriverCapabilities.properties
                ffOptions = ffOptions.merge(PropertyFileManager.getCustomWebDriverDesiredCapabilities());
                //merge hardcoded custom options
                if (customDriverOptions != null) {
                    ffOptions = ffOptions.merge(customDriverOptions);
                }
                ReportManager.logDiscrete(ffOptions.toString());
            }
            case IE -> {
                ieOptions = new InternetExplorerOptions();
                if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local"))
                    ieOptions.setCapability(CapabilityType.PLATFORM_NAME, Properties.platform.targetPlatform());
                ieOptions.setPageLoadStrategy(PageLoadStrategy.EAGER);
                ieOptions.setPageLoadTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.pageLoadTimeout()));
                ieOptions.setScriptTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.scriptExecutionTimeout()));
                //Add Proxy Setting if found
                if (SHAFT.Properties.platform.driverProxySettings() && !proxyServerSettings.isBlank()) {
                    Proxy proxy = new Proxy();
                    proxy.setHttpProxy(proxyServerSettings);
                    proxy.setSslProxy(proxyServerSettings);
                    ieOptions.setProxy(proxy);
                }
                if(SHAFT.Properties.flags.disableCache())
                {
                    ieOptions.setCapability(InternetExplorerDriver.IE_ENSURE_CLEAN_SESSION, true);
                    ieOptions.setCapability("applicationCacheEnabled",false);
                }
                //merge customWebDriverCapabilities.properties
                ieOptions = ieOptions.merge(PropertyFileManager.getCustomWebDriverDesiredCapabilities());
                //merge hardcoded custom options
                if (customDriverOptions != null) {
                    ieOptions = ieOptions.merge(customDriverOptions);
                }
                ReportManager.logDiscrete(ieOptions.toString());
            }
            case CHROME, EDGE, CHROMIUM -> {
                if (driverType.equals(DriverType.EDGE)) {
                    edOptions = (EdgeOptions) setupChromiumOptions(new EdgeOptions(), customDriverOptions);
                } else {
                    chOptions = (ChromeOptions) setupChromiumOptions(new ChromeOptions(), customDriverOptions);
                }
            }
            case SAFARI, WEBKIT -> {
                sfOptions = new SafariOptions();
                if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local"))
                    sfOptions.setCapability(CapabilityType.PLATFORM_NAME, Properties.platform.targetPlatform());
                sfOptions.setCapability(CapabilityType.UNHANDLED_PROMPT_BEHAVIOUR, UnhandledPromptBehavior.IGNORE);
                sfOptions.setPageLoadStrategy(PageLoadStrategy.EAGER);
                sfOptions.setPageLoadTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.pageLoadTimeout()));
                sfOptions.setScriptTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.scriptExecutionTimeout()));
                //Add Proxy Setting if found
                if (SHAFT.Properties.platform.driverProxySettings() && !proxyServerSettings.isBlank()) {
                    Proxy proxy = new Proxy();
                    proxy.setHttpProxy(proxyServerSettings);
                    proxy.setSslProxy(proxyServerSettings);
                    sfOptions.setProxy(proxy);
                }
                if(SHAFT.Properties.flags.disableCache()) {
                    sfOptions.setCapability("safari:cleanSession", "true");
                }
                //merge customWebDriverCapabilities.properties
                sfOptions = sfOptions.merge(PropertyFileManager.getCustomWebDriverDesiredCapabilities());
                //merge hardcoded custom options
                if (customDriverOptions != null) {
                    sfOptions = sfOptions.merge(customDriverOptions);
                }
                ReportManager.logDiscrete(sfOptions.toString());
            }
            case APPIUM_MOBILE_NATIVE, APPIUM_SAMSUNG_BROWSER, APPIUM_CHROME, APPIUM_CHROMIUM ->
                    appiumCapabilities = new DesiredCapabilities(PropertyFileManager.getCustomWebDriverDesiredCapabilities().merge(customDriverOptions));
            default ->
                    failAction("Unsupported Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\".");
        }
    }

    @SuppressWarnings("SpellCheckingInspection")
    private static ChromiumOptions<?> setupChromiumOptions(ChromiumOptions<?> options, MutableCapabilities customDriverOptions) {
        if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local"))
            options.setCapability(CapabilityType.PLATFORM_NAME, Properties.platform.targetPlatform());
        if (SHAFT.Properties.web.headlessExecution()) {
            options.addArguments("--headless=new");
            options.addArguments("--disable-gpu");
        }
        // Add if condtion to start the new session if flag=true on specific port
        if (SHAFT.Properties.performance.isEnabled()) {
            // TODO: implement lighthouse in the configuration manager and properties manager
            options.addArguments("--remote-debugging-port=" + SHAFT.Properties.performance.port());
            options.addArguments("--no-sandbox");
        }
        if (SHAFT.Properties.flags.autoMaximizeBrowserWindow()
                && !Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())
                && !Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())
                && !Platform.MAC.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            options.addArguments("--start-maximized");
        } else {
            options.addArguments("--window-position=0,0", "--window-size=" + TARGET_WINDOW_SIZE.getWidth() + "," + TARGET_WINDOW_SIZE.getHeight());
        }
        if (!SHAFT.Properties.flags.autoCloseDriverInstance())
            options.setExperimentalOption("detach", true);

//         https://github.com/GoogleChrome/chrome-launcher/blob/main/docs/chrome-flags-for-tools.md
//         https://docs.google.com/spreadsheets/d/1n-vw_PCPS45jX3Jt9jQaAhFqBY6Ge1vWF_Pa0k7dCk4/edit#gid=1265672696
        options.addArguments("--remote-allow-origins=*",
                "--enable-automation"
                , "--disable-background-timer-throttling"
                , "--disable-backgrounding-occluded-windows"
                , "--disable-features=CalculateNativeWinOcclusion"
                , "--disable-hang-monitor"
                , "--disable-domain-reliability"
                , "--disable-renderer-backgrounding"
                , "--disable-features=AutofillServerCommunication"
                , "--metrics-recording-only"
                , "--no-first-run"
                , "--no-default-browser-check"
                , "--silent-debugger-extension-api"
                , "--disable-extensions"
                , "--disable-component-extensions-with-background-pages"
                , "--disable-dev-shm-usage"
                , "--disable-features=MediaRouter"
                , "--disable-features=Translate"
                , "--disable-ipc-flooding-protection"
                , "--disable-background-networking"
                , "--mute-audio"
                , "--disable-breakpad"
                , "--ignore-certificate-errors"
                , "--disable-device-discovery-notifications"
                , "--force-color-profile=srgb"
                , "--hide-scrollbars"
                , "--host-resolver-rules"
                , "--no-pings"
                , "--disable-features=AvoidUnnecessaryBeforeUnloadCheckSync"
                , "--disable-features=CertificateTransparencyComponentUpdater"
                , "--disable-sync"
                , "--disable-features=OptimizationHints"
                , "--disable-features=DialMediaRouteProvider"
                , "--disable-features=GlobalMediaControls"
                , "--disable-features=ImprovedCookieControls"
                , "--disable-features=LazyFrameLoading"
                , "--disable-field-trial-config"
                , "--enable-features=NetworkService"
                , "--enable-features=NetworkServiceInProcess"
                , "--enable-use-zoom-for-dsf"
                , "--log-net-log"
                , "--net-log-capture-mode"
                , "--disable-client-side-phishing-detection"
                , "--disable-default-apps"
                , "--disable-features=InterestFeedContentSuggestions"
        );

        Map<String, Object> chromePreferences = new HashMap<>();
        chromePreferences.put("profile.default_content_settings.popups", 0);
        chromePreferences.put("download.prompt_for_download", "false");
        chromePreferences.put("download.default_directory", System.getProperty("user.dir") + File.separatorChar + SHAFT.Properties.paths.downloads());
        options.setExperimentalOption("prefs", chromePreferences);
        options.setUnhandledPromptBehaviour(UnexpectedAlertBehaviour.IGNORE);
        options.setCapability(CapabilityType.ACCEPT_INSECURE_CERTS, true);
        options.setPageLoadStrategy(PageLoadStrategy.NONE); // https://www.skptricks.com/2018/08/timed-out-receiving-message-from-renderer-selenium.html
        options.setPageLoadTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.pageLoadTimeout()));
        options.setScriptTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.scriptExecutionTimeout()));
        //Add Proxy Setting if found
        String proxy = Properties.platform.proxy();
        if (SHAFT.Properties.platform.driverProxySettings() && !"".equals(proxy)) {
            options.setProxy(new Proxy().setHttpProxy(proxy).setSslProxy(proxy));
        }
        //add logging preferences if enabled
        if (SHAFT.Properties.reporting.captureWebDriverLogs()) {
            options.setCapability("goog:loggingPrefs", configureLoggingPreferences());
        }
        // Mobile Emulation
        if (SHAFT.Properties.web.isMobileEmulation() && isWebExecution()) {
            Map<String, Object> mobileEmulation = new HashMap<>();
            if (!SHAFT.Properties.web.mobileEmulationIsCustomDevice() && (!SHAFT.Properties.web.mobileEmulationDeviceName().isBlank())) {
                mobileEmulation.put("deviceName", SHAFT.Properties.web.mobileEmulationDeviceName());
            } else if (SHAFT.Properties.web.mobileEmulationIsCustomDevice()) {
                if ((SHAFT.Properties.web.mobileEmulationWidth() != 0) && (SHAFT.Properties.web.mobileEmulationHeight() != 0)) {
                    Map<String, Object> deviceMetrics = new HashMap<>();
                    deviceMetrics.put("width", SHAFT.Properties.web.mobileEmulationWidth());
                    deviceMetrics.put("height", SHAFT.Properties.web.mobileEmulationHeight());
                    if (SHAFT.Properties.web.mobileEmulationPixelRatio() != 0) {
                        deviceMetrics.put("pixelRatio", SHAFT.Properties.web.mobileEmulationPixelRatio());
                    }
                    mobileEmulation.put("deviceMetrics", deviceMetrics);
                }
                if (!SHAFT.Properties.web.mobileEmulationUserAgent().isEmpty()) {
                    mobileEmulation.put("userAgent", SHAFT.Properties.web.mobileEmulationUserAgent());
                }
            }
            options.setExperimentalOption("mobileEmulation", mobileEmulation);
        }
        // Enable BiDi
        options.setCapability("webSocketUrl", true);
        //merge customWebdriverCapabilities.properties
        options = (ChromiumOptions<?>) options.merge(PropertyFileManager.getCustomWebDriverDesiredCapabilities());
        //merge hardcoded custom options
        if (customDriverOptions != null) {
            options = (ChromiumOptions<?>) options.merge(customDriverOptions);
        }
        if (!SHAFT.Properties.flags.autoCloseDriverInstance()) {
            Map<Object, Object> chromeOptions = new HashMap<>((Map<Object, Object>) options.getCapability("goog:chromeOptions"));
            chromeOptions.put("detach", true);
            options.setCapability("goog:chromeOptions", chromeOptions);
        }
        ReportManager.logDiscrete(options.toString());
        return options;
    }

    private static LoggingPreferences configureLoggingPreferences() {
        //Add logging setting if enabled
        LoggingPreferences logPrefs = new LoggingPreferences();
        logPrefs.enable(LogType.PERFORMANCE, java.util.logging.Level.ALL);
        logPrefs.enable(LogType.DRIVER, java.util.logging.Level.ALL);
        logPrefs.enable(LogType.BROWSER, java.util.logging.Level.ALL);
        logPrefs.enable(LogType.DRIVER, java.util.logging.Level.ALL);
        return logPrefs;
    }

    private static void createNewLocalDriverInstance(DriverType driverType, boolean retry) {
        String initialLog = "Attempting to run locally on: \"" + Properties.platform.targetPlatform() + " | " + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\"";
        if (SHAFT.Properties.web.headlessExecution()) {
            initialLog = initialLog + ", Headless Execution";
        }
        ReportManager.logDiscrete(initialLog + ".");
        try {
            ReportManager.logDiscrete(WEB_DRIVER_MANAGER_MESSAGE);
            switch (driverType) {
                case FIREFOX -> driver = new FirefoxDriver(ffOptions);
                case IE -> driver = new InternetExplorerDriver(ieOptions);
                case CHROME -> {
                    driver = new ChromeDriver(chOptions);
                    disableCacheEdgeAndChrome();
                }
                case EDGE -> {
                    driver = new EdgeDriver(edOptions);
                    disableCacheEdgeAndChrome();
                }
                case SAFARI -> driver = new SafariDriver(sfOptions);
                default ->
                        failAction("Unsupported Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\".");
            }
            ReportManager.log(initialLog.replace("Attempting to run locally on", "Successfully Opened") + ".");
        } catch (SessionNotCreatedException | WebDriverManagerException exception) {
            if (driverType.equals(DriverType.SAFARI)
                    && Throwables.getRootCause(exception).getMessage().toLowerCase().contains("safari instance is already paired with another webdriver session")) {
                //this issue happens when running locally via safari/mac platform
                // sample failure can be found here: https://github.com/ShaftHQ/SHAFT_ENGINE/actions/runs/4527911969/jobs/7974202314#step:4:46621
                // attempting blind fix by trying to quit existing safari instances if any
                try {
                    SHAFT.CLI.terminal().performTerminalCommand("osascript -e \"tell application \\\"Safari\\\" to quit\"\n");
                } catch (Throwable throwable) {
                    // ignore
                }
            }
            // attempting blind fix by trying to quit existing driver if any
            try {
                driver.quit();
            } catch (Throwable throwable) {
                // ignore
            } finally {
                driver = null;
            }
            if (retry) {
                try {
                    Thread.sleep(5000);
                } catch (InterruptedException e) {
                    //do nothing
                }
                createNewLocalDriverInstance(driverType, false);
            }
            failAction("Failed to create new Browser Session", exception);
        }
    }

    private static void createNewDockerizedDriverInstance(DriverType driverType) {
        String initialLog = "Attempting to run dockerized on: \"" + Properties.platform.targetPlatform() + " | " + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\"";
        if (SHAFT.Properties.web.headlessExecution()) {
            initialLog = initialLog + ", Headless Execution";
        }
        ReportManager.log(initialLog + ".");

        try {
            ReportManager.logDiscrete(WEB_DRIVER_MANAGER_DOCKERIZED_MESSAGE);
            switch (driverType) {
                case FIREFOX -> webDriverManager.set(WebDriverManager.firefoxdriver().capabilities(ffOptions));
                case CHROME -> webDriverManager.set(WebDriverManager.chromedriver().capabilities(chOptions));
                case EDGE -> webDriverManager.set(WebDriverManager.edgedriver().capabilities(edOptions));
                case SAFARI -> webDriverManager.set(WebDriverManager.safaridriver().capabilities(sfOptions));
                default ->
                        failAction("Unsupported Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\". We only support Chrome, Edge, Firefox, and Safari in this dockerized mode.");
            }
            RemoteWebDriver remoteWebDriver = (RemoteWebDriver) webDriverManager.get()
                    .proxy(SHAFT.Properties.platform.proxy())
                    .browserInDocker()
                    .dockerShmSize("2g")
                    .enableVnc()
                    .viewOnly()
                    .avoidUseChromiumDriverSnap()
                    .dockerScreenResolution(TARGET_WINDOW_SIZE.getWidth() + "x" + TARGET_WINDOW_SIZE.getHeight() + "x24")
//                    .dockerVolumes("\\local\\path:\\container\\path")
                    .enableRecording()
                    .dockerRecordingOutput(SHAFT.Properties.paths.video())
                    .create();
            remoteWebDriver.setFileDetector(new LocalFileDetector());
//            driver =ThreadGuard.protect(remoteWebDriver));
            driver = remoteWebDriver;
            ReportManager.log("Successfully Opened " + JavaHelper.convertToSentenceCase(driverType.getValue()) + ".");
        } catch (io.github.bonigarcia.wdm.config.WebDriverManagerException exception) {
            failAction("Failed to create new Dockerized Browser Session, are you sure Docker is available on your machine?", exception);
        }
    }

    private static void createNewRemoteDriverInstance(DriverType driverType) {
        var initialLog = new StringBuilder();
        initialLog.append("Attempting to run remotely on: \"").append(Properties.platform.targetPlatform());

        if (!Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())
                && !Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            initialLog.append(" | ").append(JavaHelper.convertToSentenceCase(driverType.getValue()));
        }

        initialLog.append(" | ").append(TARGET_HUB_URL).append("\"");

        if (SHAFT.Properties.web.headlessExecution()
                && !Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())
                && !Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            initialLog.append(", Headless Execution");
        }
        ReportManager.log(initialLog + ".");

        if (Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())
                || Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            if (appiumCapabilities == null) {
                appiumCapabilities = initializeMobileDesiredCapabilities(null);
            } else {
                appiumCapabilities.merge(initializeMobileDesiredCapabilities(appiumCapabilities));
                ReportManager.log(appiumCapabilities.toString());
            }
        }

        try {
            configureRemoteDriverInstance(driverType, appiumCapabilities);
        } catch (UnreachableBrowserException e) {
            killSwitch = true;
            failAction("Unreachable Browser, terminated test suite execution.", e);
        } catch (WebDriverException e) {
            if (e.getMessage().contains("Error forwarding the new session cannot find")) {
                ReportManager.logDiscrete("Failed to run remotely on: \"" + Properties.platform.targetPlatform() + "\", \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\", \""
                        + TARGET_HUB_URL + "\".");
                failAction(
                        "Error forwarding the new session: Couldn't find a node that matches the desired capabilities.", e);
            } else {
                ReportManager.logDiscrete("Failed to run remotely on: \"" + Properties.platform.targetPlatform() + "\", \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\", \""
                        + TARGET_HUB_URL + "\".");
                failAction("Unhandled Error.", e);
            }
        } catch (NoClassDefFoundError e) {
            failAction("Failed to create Remote WebDriver instance", e);
        }
    }

    @Step("Setting up remote driver instance")
    private static void setRemoteDriverInstance(Capabilities capabilities) {
        // stage 1: ensure that the server is up and running
        if (SHAFT.Properties.timeouts.waitForRemoteServerToBeUp()) {
            ReportManager.logDiscrete("Attempting to connect to remote server for up to " + TimeUnit.SECONDS.toMinutes(appiumServerInitializationTimeout) + "min.");
            try {
                TARGET_HUB_URL = TARGET_HUB_URL.contains("0.0.0.0") ? TARGET_HUB_URL.replace("0.0.0.0", "localhost") : TARGET_HUB_URL;
                if (Properties.flags.forceCheckStatusOfRemoteServer()) {
                    var statusCode = attemptRemoteServerPing();
                    ReportManager.logDiscrete("Remote server is online, established successful connection with status code: " + statusCode + ".");
                }
            } catch (Throwable throwable) {
                ReportManagerHelper.logDiscrete(throwable, Level.DEBUG);
                failAction("Failed to connect to remote server.", throwable);
            }
        }

        // stage 2: create remove driver instance (requires some time with dockerized appium)
        ReportManager.logDiscrete("Attempting to instantiate remote driver instance for up to " + TimeUnit.SECONDS.toMinutes(remoteServerInstanceCreationTimeout) + "min.");
        try {
            driver = attemptRemoteServerConnection(capabilities);
            ((RemoteWebDriver) driver).setFileDetector(new LocalFileDetector());
            if (!isWebExecution() && SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase("Android")) {
                // https://github.com/appium/appium-uiautomator2-driver#settings-api
                ((AppiumDriver) driver).setSetting(Setting.WAIT_FOR_IDLE_TIMEOUT, 5000);
                ((AppiumDriver) driver).setSetting(Setting.ALLOW_INVISIBLE_ELEMENTS, true);
                ((AppiumDriver) driver).setSetting(Setting.IGNORE_UNIMPORTANT_VIEWS, false);
                ((AppiumDriver) driver).setSetting("enableMultiWindows", true);
//        elementResponseAttributes, shouldUseCompactResponses
                ((AppiumDriver) driver).setSetting(Setting.MJPEG_SCALING_FACTOR, 25);
                ((AppiumDriver) driver).setSetting(Setting.MJPEG_SERVER_SCREENSHOT_QUALITY, 100);
                ((AppiumDriver) driver).setSetting("mjpegBilinearFiltering", true);
                // ((AppiumDriver) driver).setSetting("limitXPathContextScope", false);
//                ((AppiumDriver) driver).setSetting("disableIdLocatorAutocompletion", true);
//        https://github.com/appium/appium-uiautomator2-driver#mobile-deeplink
//        http://code2test.com/appium-tutorial/how-to-use-uiselector-in-appium/
//        https://github.com/appium/appium-uiautomator2-driver
            }
            ReportManager.logDiscrete("Successfully instantiated remote driver instance.");
        } catch (Throwable throwable) {
            failAction("Failed to instantiate remote driver instance.", throwable);
        }
    }

    @SneakyThrows(java.lang.InterruptedException.class)
    private static int attemptRemoteServerPing() {
        boolean serverReady = false;
        var session = new SHAFT.API(TARGET_HUB_URL);
        var statusCode = 500;
        var startTime = System.currentTimeMillis();
        do {
            try {
                statusCode = session.get("status/").perform().andReturn().statusCode();
                if (statusCode >= 200 && statusCode < 300) {
                    serverReady = true;
                }
            } catch (Throwable throwable1) {
                try {
                    statusCode = session.get("wd/hub/status/").perform().andReturn().statusCode();
                    if (statusCode >= 200 && statusCode < 300) {
                        serverReady = true;
                    }
                } catch (Throwable throwable2) {
                    // do nothing
                    ReportManagerHelper.logDiscrete(throwable1, Level.DEBUG);
                    ReportManagerHelper.logDiscrete(throwable2, Level.DEBUG);
                }
            }
            if (!serverReady) {
                //noinspection BusyWait
                Thread.sleep(TimeUnit.SECONDS.toMillis(appiumServerInitializationPollingInterval));
            }
        } while (!serverReady && (System.currentTimeMillis() - startTime <  TimeUnit.SECONDS.toMillis(appiumServerInitializationTimeout)));
        if (!serverReady){
            failAction("Failed to connect to remote server. It was still not ready after " + TimeUnit.SECONDS.toMinutes(appiumServerInitializationTimeout) + " minutes.");
        }
        return statusCode;
    }

    @SneakyThrows({java.net.MalformedURLException.class, InterruptedException.class})
    private static RemoteWebDriver attemptRemoteServerConnection(Capabilities capabilities) {
        RemoteWebDriver driver = null;
        boolean isRemoteConnectionEstablished = false;
        var startTime = System.currentTimeMillis();
        var exception = "";
        do {
            try {
                driver = connectToRemoteServer(capabilities, false);
                isRemoteConnectionEstablished = true;
            } catch (org.openqa.selenium.SessionNotCreatedException sessionNotCreatedException1) {
                exception = sessionNotCreatedException1.getMessage();
                try {
                    driver = connectToRemoteServer(capabilities, true);
                    isRemoteConnectionEstablished = true;
                } catch (org.openqa.selenium.SessionNotCreatedException sessionNotCreatedException2) {
                    // do nothing
                    ReportManagerHelper.logDiscrete(sessionNotCreatedException1, Level.DEBUG);
                    ReportManagerHelper.logDiscrete(sessionNotCreatedException2, Level.DEBUG);
                }
            }
            if (!isRemoteConnectionEstablished) {
                //terminate in case of any other exception
                //noinspection BusyWait
                Thread.sleep(TimeUnit.SECONDS.toMillis(appiumServerPreparationPollingInterval));
            }
        } while (!isRemoteConnectionEstablished && (System.currentTimeMillis() - startTime < TimeUnit.SECONDS.toMillis(remoteServerInstanceCreationTimeout)));
        if (!isRemoteConnectionEstablished) {
            failAction("Failed to connect to remote server. Session was still not created after " + TimeUnit.SECONDS.toMinutes(remoteServerInstanceCreationTimeout) + " minutes." + "\nOriginal Error is : " + exception);
        }
        return driver;
    }

    private static RemoteWebDriver connectToRemoteServer(Capabilities capabilities, boolean isLegacy) throws MalformedURLException {
        var targetHubUrl = isLegacy ? TARGET_HUB_URL + "wd/hub" : TARGET_HUB_URL;

        var targetLambdaTestHubURL = targetHubUrl.replace("http", "https");

        var targetPlatform = Properties.platform.targetPlatform();

        var targetMobileHubUrl = targetHubUrl.replace("@", "@mobile-").replace("http", "https");

        if (targetPlatform.equalsIgnoreCase(Platform.ANDROID.toString())) {
            if (SHAFT.Properties.platform.executionAddress().contains("lambdatest") && !isMobileWebExecution()) {
                return new AndroidDriver(URI.create(targetMobileHubUrl).toURL(), capabilities);
            } else {
                if (SHAFT.Properties.platform.executionAddress().contains("lambdatest")) {
                    return new AndroidDriver(URI.create(targetLambdaTestHubURL).toURL(), capabilities);
                } else {
                    return new AndroidDriver(URI.create(targetHubUrl).toURL(), capabilities);
                }
            }
        } else if (targetPlatform.equalsIgnoreCase(Platform.IOS.toString())) {
            if (SHAFT.Properties.platform.executionAddress().contains("lambdatest") && !isMobileWebExecution()) {
                return new IOSDriver(URI.create(targetMobileHubUrl).toURL(), capabilities);
            } else {
                if (SHAFT.Properties.platform.executionAddress().contains("lambdatest")) {
                    return new IOSDriver(URI.create(targetLambdaTestHubURL).toURL(), capabilities);
                } else {
                    return new IOSDriver(URI.create(targetHubUrl).toURL(), capabilities);
                }
            }
        } else {
            if (SHAFT.Properties.platform.executionAddress().contains("lambdatest")) {
                return new RemoteWebDriver(URI.create(targetLambdaTestHubURL).toURL(), capabilities);
            } else {
                return new RemoteWebDriver(URI.create(targetHubUrl).toURL(), capabilities);
            }
        }
    }

    private static void configureRemoteDriverInstance(DriverType driverType, DesiredCapabilities appiumDesiredCapabilities) {
        switch (driverType) {
            case FIREFOX -> setRemoteDriverInstance(ffOptions);
            case IE -> setRemoteDriverInstance(ieOptions);
            case CHROME, CHROMIUM -> setRemoteDriverInstance(chOptions);
            case EDGE -> setRemoteDriverInstance(edOptions);
            case SAFARI, WEBKIT -> {
                if (!Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())
                        && !Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
                    setRemoteDriverInstance(sfOptions);
                } else {
                    setRemoteDriverInstance(appiumDesiredCapabilities);
                }
            }
            case APPIUM_CHROME, APPIUM_CHROMIUM -> {
                WebDriverManager.chromedriver().browserVersion(SHAFT.Properties.mobile.browserVersion()).setup();
                appiumDesiredCapabilities.setCapability("chromedriverExecutable",
                        WebDriverManager.chromedriver().getDownloadedDriverPath());
                setRemoteDriverInstance(appiumDesiredCapabilities);
            }
            case APPIUM_MOBILE_NATIVE, APPIUM_SAMSUNG_BROWSER, APPIUM_BROWSER ->
                    setRemoteDriverInstance(appiumDesiredCapabilities);
            default ->
                    failAction("Unsupported Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\".");
        }
        var driverName = driverType.getValue();
        if (driverName.contains("MobileApp")) {
            driverName = driverName.replace("Mobile", Properties.platform.targetPlatform());
        }
        ReportManager.log("Successfully Opened \"" + JavaHelper.convertToSentenceCase(driverName) + "\".");
    }
    private static void attachWebDriverLogs() {
        // TODO: capture logs and record video in case of retrying failed test
        if (SHAFT.Properties.reporting.captureWebDriverLogs()) {
            try {
                var driverLogs = driver.manage().logs();
                driverLogs.getAvailableLogTypes().forEach(logType -> {
                            var logBuilder = new StringBuilder();
                            driverLogs.get(logType).getAll().forEach(logEntry -> logBuilder.append(logEntry.toString()).append(System.lineSeparator()));
                            ReportManagerHelper.attach("Selenium WebDriver Logs", logType, logBuilder.toString());
                        }
                );
            } catch (WebDriverException e) {
                // exception when the defined logging is not supported
            }
        }
    }

    @SuppressWarnings("SpellCheckingInspection")
    private static DesiredCapabilities initializeMobileDesiredCapabilities(DesiredCapabilities desiredCapabilities) {
        if (!isMobileWebExecution()) {
            Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();
            caps.forEach((capabilityName, value) -> {
                if (!value.isBlank()) {
                    if (Arrays.asList("true", "false").contains(value.trim().toLowerCase())) {
                        desiredCapabilities.setCapability(capabilityName.replace("mobile_", "appium:"), Boolean.valueOf(value));
                    } else if (StringUtils.isStrictlyNumeric(value.trim())) {
                        desiredCapabilities.setCapability(capabilityName.replace("mobile_", "appium:"), Integer.valueOf(value));
                    } else {
                        desiredCapabilities.setCapability(capabilityName.replace("mobile_", "appium:"), value);
                    }
                }
            });
        }

        if (isMobileWebExecution()) {
            //https://chromedriver.chromium.org/capabilities
            desiredCapabilities.setCapability("browserName", SHAFT.Properties.mobile.browserName());
            desiredCapabilities.setCapability("pageLoadStrategy", PageLoadStrategy.NONE);
        }

        if (!isMobileWebExecution() && Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            // experimental android capabilities
            // https://github.com/appium/appium-uiautomator2-driver
            // Check if user sent any capability then don't take the deafult

            if (desiredCapabilities.getCapability("appium:fullReset") == null)
                desiredCapabilities.setCapability("appium:fullReset", true);

            if (desiredCapabilities.getCapability("appium:appWaitActivity") == null)
                desiredCapabilities.setCapability("appium:appWaitActivity", "*");

            if (desiredCapabilities.getCapability("appium:printPageSourceOnFindFailure") == null)
                desiredCapabilities.setCapability("appium:printPageSourceOnFindFailure", true);

            if (desiredCapabilities.getCapability("appium:disableWindowAnimation") == null)
                desiredCapabilities.setCapability("appium:disableWindowAnimation", true);

            if (desiredCapabilities.getCapability("appium:forceAppLaunch") == null)
                desiredCapabilities.setCapability("appium:forceAppLaunch", true);

            if (desiredCapabilities.getCapability("appium:autoGrantPermissions") == null)
                desiredCapabilities.setCapability("appium:autoGrantPermissions", true);

//            desiredCapabilities.setCapability("appium:otherApps", ",,,");

            if (desiredCapabilities.getCapability("appium:allowTestPackages") == null)
                desiredCapabilities.setCapability("appium:allowTestPackages", true);

            if (desiredCapabilities.getCapability("appium:enforceAppInstall") == null)
                desiredCapabilities.setCapability("appium:enforceAppInstall", false);

            if (desiredCapabilities.getCapability("appium:clearDeviceLogsOnStart") == null)
                desiredCapabilities.setCapability("appium:clearDeviceLogsOnStart", true);

            if (desiredCapabilities.getCapability("appium:ignoreHiddenApiPolicyError") == null)
                desiredCapabilities.setCapability("appium:ignoreHiddenApiPolicyError", true);

            if (desiredCapabilities.getCapability("appium:isHeadless") == null)
                desiredCapabilities.setCapability("appium:isHeadless", true);

            if (desiredCapabilities.getCapability("appium:noSign") == null)
                desiredCapabilities.setCapability("appium:noSign", true);

            if (desiredCapabilities.getCapability("appium:enableWebviewDetailsCollection") == null)
                desiredCapabilities.setCapability("appium:enableWebviewDetailsCollection", true);

            if (desiredCapabilities.getCapability("appium:showChromedriverLog") == null)
                desiredCapabilities.setCapability("appium:showChromedriverLog", true);
        }
        return desiredCapabilities;
    }

    public static void initializeDriver() {
        if (Properties.mobile.selfManaged() && (Properties.platform.targetPlatform().equalsIgnoreCase(Platform.ANDROID.toString())
                || Properties.platform.targetPlatform().equalsIgnoreCase(Platform.IOS.toString()))) {
            //singleton initialization
            AppiumSelfManagementHelper.setupAppiumSelfManagedExecutionPrerequisites();
        }

        var mobile_browserName = SHAFT.Properties.mobile.browserName();
        String targetBrowserName = SHAFT.Properties.web.targetBrowserName();

        // it's null in case of native cucumber execution
        if (Reporter.getCurrentTestResult() != null) {
            var overridingBrowserName = Reporter.getCurrentTestResult().getTestContext().getCurrentXmlTest().getParameter("targetBrowserName");
            if (overridingBrowserName != null && !overridingBrowserName.isBlank()) {
                targetBrowserName = overridingBrowserName;
            }
        }
        DriverFactoryHelper.targetBrowserName = targetBrowserName;
        initializeDriver(getDriverTypeFromName((mobile_browserName.isBlank()) ? targetBrowserName : mobile_browserName), null);
    }

    public static void initializeDriver(@NonNull DriverType driverType) {
        initializeDriver(driverType, null);
    }

    public static void initializeDriver(MutableCapabilities customDriverOptions) {
        var mobile_browserName = SHAFT.Properties.mobile.browserName();
        String targetBrowserName;

        var overridingBrowserName = Reporter.getCurrentTestResult().getTestContext().getCurrentXmlTest().getParameter("targetBrowserName");
        if (overridingBrowserName != null && !overridingBrowserName.isBlank()) {
            targetBrowserName = overridingBrowserName;
        } else {
            targetBrowserName = SHAFT.Properties.web.targetBrowserName();
        }
        DriverFactoryHelper.targetBrowserName = (mobile_browserName == null || mobile_browserName.isBlank()) ? targetBrowserName : mobile_browserName;
        initializeDriver((getDriverTypeFromName(DriverFactoryHelper.targetBrowserName)), customDriverOptions);
    }

    public static void initializeDriver(@NonNull DriverType driverType, MutableCapabilities customDriverOptions) {
        initializeSystemProperties();
        try {
            var isMobileExecution = Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())
                    || Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform());

            if (isMobileExecution) {
                //mobile execution
                if (isMobileWebExecution()) {
                    // org.openqa.selenium.InvalidArgumentException: Parameters were incorrect. We wanted {"required":["x","y","width","height"]} and you sent ["width","height"]
                    SHAFT.Properties.web.set().headlessExecution(false);
                    if (SHAFT.Properties.mobile.browserName().equalsIgnoreCase(DriverType.APPIUM_SAMSUNG_BROWSER.getValue())) {
                        driverType = DriverType.APPIUM_SAMSUNG_BROWSER;
                    } else {
                        driverType = DriverType.APPIUM_CHROME;
                    }
                } else {
                    driverType = DriverType.APPIUM_MOBILE_NATIVE;
                }
                setDriverOptions(driverType, customDriverOptions);
                createNewRemoteDriverInstance(driverType);
            } else {
                //desktop execution
                setDriverOptions(driverType, customDriverOptions);
                switch (SHAFT.Properties.platform.executionAddress()) {
                    case "local" -> createNewLocalDriverInstance(driverType, true);
                    case "dockerized" -> createNewDockerizedDriverInstance(driverType);
                    default -> createNewRemoteDriverInstance(driverType);
                }
            }

            if (SHAFT.Properties.web.headlessExecution()) {
                driver.manage().window().setSize(new Dimension(TARGET_WINDOW_SIZE.getWidth(), TARGET_WINDOW_SIZE.getHeight()));
            } else {
                Dimension browserWindowSize = new Dimension(
                        SHAFT.Properties.web.browserWindowWidth(),
                        SHAFT.Properties.web.browserWindowHeight()
                );
                if (!isMobileExecution && !SHAFT.Properties.flags.autoMaximizeBrowserWindow()) {
                    driver.manage().window().setSize(browserWindowSize);
                }
            }

            if (!isMobileExecution) {
                var targetBrowserName = SHAFT.Properties.web.targetBrowserName().toLowerCase();
                if (SHAFT.Properties.flags.autoMaximizeBrowserWindow()
                        && (
                        targetBrowserName.contains(Browser.SAFARI.browserName().toLowerCase())
                                || targetBrowserName.contains(Browser.FIREFOX.browserName().toLowerCase()))) {
                    BrowserActions.getInstance().maximizeWindow();
                }
            }
            // start session recording
            RecordManager.startVideoRecording(driver);
        } catch (NullPointerException e) {
            FailureReporter.fail(DriverFactoryHelper.class, "Unhandled Exception with Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\".", e);
        }

        if (SHAFT.Properties.healenium.healEnabled()) {
            ReportManager.logDiscrete("Initializing Healenium's Self Healing Driver...");
//            driver =ThreadGuard.protect(SelfHealingDriver.create(driver)));
            driver = SelfHealingDriver.create(driver);
        }
    }

    public static void initializeSystemProperties() {
        PropertiesHelper.postProcessing();
        TARGET_HUB_URL = (SHAFT.Properties.platform.executionAddress().trim().toLowerCase().startsWith("http")) ? SHAFT.Properties.platform.executionAddress() : "http://" + SHAFT.Properties.platform.executionAddress() + "/";
    }
}
