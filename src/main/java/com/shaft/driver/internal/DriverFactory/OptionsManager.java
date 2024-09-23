package com.shaft.driver.internal.DriverFactory;

import com.mysql.cj.util.StringUtils;
import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertyFileManager;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import io.appium.java_client.remote.options.UnhandledPromptBehavior;
import lombok.Getter;
import lombok.Setter;
import org.openqa.selenium.*;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.chromium.ChromiumOptions;
import org.openqa.selenium.edge.EdgeOptions;
import org.openqa.selenium.firefox.FirefoxDriverLogLevel;
import org.openqa.selenium.firefox.FirefoxOptions;
import org.openqa.selenium.firefox.FirefoxProfile;
import org.openqa.selenium.ie.InternetExplorerDriver;
import org.openqa.selenium.ie.InternetExplorerOptions;
import org.openqa.selenium.logging.LogType;
import org.openqa.selenium.logging.LoggingPreferences;
import org.openqa.selenium.remote.Browser;
import org.openqa.selenium.remote.CapabilityType;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.safari.SafariOptions;

import java.io.File;
import java.time.Duration;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

@Getter
@Setter
public class OptionsManager {

    private ChromeOptions chOptions;
    private FirefoxOptions ffOptions;
    private SafariOptions sfOptions;
    private EdgeOptions edOptions;
    private InternetExplorerOptions ieOptions;
    private DesiredCapabilities appiumCapabilities;

    protected void setDriverOptions(DriverFactory.DriverType driverType, MutableCapabilities customDriverOptions) {
        // get Proxy server settings | testing behind a proxy
        String proxyServerSettings = SHAFT.Properties.platform.proxy();

        //https://github.com/GoogleChrome/chrome-launcher/blob/master/docs/chrome-flags-for-tools.md#--enable-automation
        switch (driverType) {
            case FIREFOX -> {
                // https://wiki.mozilla.org/Firefox/CommandLineOptions
                // https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities/firefoxOptions
                ffOptions = new FirefoxOptions();
                var ffProfile = new FirefoxProfile();
                ffProfile.setPreference("browser.download.dir", System.getProperty("user.dir") + File.separatorChar + SHAFT.Properties.paths.downloads().replace("/", File.separator));
                ffProfile.setPreference("browser.download.folderList", 2);
                //noinspection SpellCheckingInspection
                ffProfile.setPreference("browser.helperApps.neverAsk.saveToDisk", "application/vnd.hzn-3d-crossword;video/3gpp;video/3gpp2;application/vnd.mseq;application/vnd.3m.post-it-notes;application/vnd.3gpp.pic-bw-large;application/vnd.3gpp.pic-bw-small;application/vnd.3gpp.pic-bw-var;application/vnd.3gp2.tcap;application/x-7z-compressed;application/x-abiword;application/x-ace-compressed;application/vnd.americandynamics.acc;application/vnd.acucobol;application/vnd.acucorp;audio/adpcm;application/x-authorware-bin;application/x-athorware-map;application/x-authorware-seg;application/vnd.adobe.air-application-installer-package+zip;application/x-shockwave-flash;application/vnd.adobe.fxp;application/pdf;application/vnd.cups-ppd;application/x-director;applicaion/vnd.adobe.xdp+xml;application/vnd.adobe.xfdf;audio/x-aac;application/vnd.ahead.space;application/vnd.airzip.filesecure.azf;application/vnd.airzip.filesecure.azs;application/vnd.amazon.ebook;application/vnd.amiga.ami;applicatin/andrew-inset;application/vnd.android.package-archive;application/vnd.anser-web-certificate-issue-initiation;application/vnd.anser-web-funds-transfer-initiation;application/vnd.antix.game-component;application/vnd.apple.installe+xml;application/applixware;application/vnd.hhe.lesson-player;application/vnd.aristanetworks.swi;text/x-asm;application/atomcat+xml;application/atomsvc+xml;application/atom+xml;application/pkix-attr-cert;audio/x-aiff;video/x-msvieo;application/vnd.audiograph;image/vnd.dxf;model/vnd.dwf;text/plain-bas;application/x-bcpio;application/octet-stream;image/bmp;application/x-bittorrent;application/vnd.rim.cod;application/vnd.blueice.multipass;application/vnd.bm;application/x-sh;image/prs.btif;application/vnd.businessobjects;application/x-bzip;application/x-bzip2;application/x-csh;text/x-c;application/vnd.chemdraw+xml;text/css;chemical/x-cdx;chemical/x-cml;chemical/x-csml;application/vn.contact.cmsg;application/vnd.claymore;application/vnd.clonk.c4group;image/vnd.dvb.subtitle;application/cdmi-capability;application/cdmi-container;application/cdmi-domain;application/cdmi-object;application/cdmi-queue;applicationvnd.cluetrust.cartomobile-config;application/vnd.cluetrust.cartomobile-config-pkg;image/x-cmu-raster;model/vnd.collada+xml;text/csv;application/mac-compactpro;application/vnd.wap.wmlc;image/cgm;x-conference/x-cooltalk;image/x-cmx;application/vnd.xara;application/vnd.cosmocaller;application/x-cpio;application/vnd.crick.clicker;application/vnd.crick.clicker.keyboard;application/vnd.crick.clicker.palette;application/vnd.crick.clicker.template;application/vn.crick.clicker.wordbank;application/vnd.criticaltools.wbs+xml;application/vnd.rig.cryptonote;chemical/x-cif;chemical/x-cmdf;application/cu-seeme;application/prs.cww;text/vnd.curl;text/vnd.curl.dcurl;text/vnd.curl.mcurl;text/vnd.crl.scurl;application/vnd.curl.car;application/vnd.curl.pcurl;application/vnd.yellowriver-custom-menu;application/dssc+der;application/dssc+xml;application/x-debian-package;audio/vnd.dece.audio;image/vnd.dece.graphic;video/vnd.dec.hd;video/vnd.dece.mobile;video/vnd.uvvu.mp4;video/vnd.dece.pd;video/vnd.dece.sd;video/vnd.dece.video;application/x-dvi;application/vnd.fdsn.seed;application/x-dtbook+xml;application/x-dtbresource+xml;application/vnd.dvb.ait;applcation/vnd.dvb.service;audio/vnd.digital-winds;image/vnd.djvu;application/xml-dtd;application/vnd.dolby.mlp;application/x-doom;application/vnd.dpgraph;audio/vnd.dra;application/vnd.dreamfactory;audio/vnd.dts;audio/vnd.dts.hd;imag/vnd.dwg;application/vnd.dynageo;application/ecmascript;application/vnd.ecowin.chart;image/vnd.fujixerox.edmics-mmr;image/vnd.fujixerox.edmics-rlc;application/exi;application/vnd.proteus.magazine;application/epub+zip;message/rfc82;application/vnd.enliven;application/vnd.is-xpr;image/vnd.xiff;application/vnd.xfdl;application/emma+xml;application/vnd.ezpix-album;application/vnd.ezpix-package;image/vnd.fst;video/vnd.fvt;image/vnd.fastbidsheet;application/vn.denovo.fcselayout-link;video/x-f4v;video/x-flv;image/vnd.fpx;image/vnd.net-fpx;text/vnd.fmi.flexstor;video/x-fli;application/vnd.fluxtime.clip;application/vnd.fdf;text/x-fortran;application/vnd.mif;application/vnd.framemaker;imae/x-freehand;application/vnd.fsc.weblaunch;application/vnd.frogans.fnc;application/vnd.frogans.ltf;application/vnd.fujixerox.ddd;application/vnd.fujixerox.docuworks;application/vnd.fujixerox.docuworks.binder;application/vnd.fujitu.oasys;application/vnd.fujitsu.oasys2;application/vnd.fujitsu.oasys3;application/vnd.fujitsu.oasysgp;application/vnd.fujitsu.oasysprs;application/x-futuresplash;application/vnd.fuzzysheet;image/g3fax;application/vnd.gmx;model/vn.gtw;application/vnd.genomatix.tuxedo;application/vnd.geogebra.file;application/vnd.geogebra.tool;model/vnd.gdl;application/vnd.geometry-explorer;application/vnd.geonext;application/vnd.geoplan;application/vnd.geospace;applicatio/x-font-ghostscript;application/x-font-bdf;application/x-gtar;application/x-texinfo;application/x-gnumeric;application/vnd.google-earth.kml+xml;application/vnd.google-earth.kmz;application/vnd.grafeq;image/gif;text/vnd.graphviz;aplication/vnd.groove-account;application/vnd.groove-help;application/vnd.groove-identity-message;application/vnd.groove-injector;application/vnd.groove-tool-message;application/vnd.groove-tool-template;application/vnd.groove-vcar;video/h261;video/h263;video/h264;application/vnd.hp-hpid;application/vnd.hp-hps;application/x-hdf;audio/vnd.rip;application/vnd.hbci;application/vnd.hp-jlyt;application/vnd.hp-pcl;application/vnd.hp-hpgl;application/vnd.yamaha.h-script;application/vnd.yamaha.hv-dic;application/vnd.yamaha.hv-voice;application/vnd.hydrostatix.sof-data;application/hyperstudio;application/vnd.hal+xml;text/html;application/vnd.ibm.rights-management;application/vnd.ibm.securecontainer;text/calendar;application/vnd.iccprofile;image/x-icon;application/vnd.igloader;image/ief;application/vnd.immervision-ivp;application/vnd.immervision-ivu;application/reginfo+xml;text/vnd.in3d.3dml;text/vnd.in3d.spot;mode/iges;application/vnd.intergeo;application/vnd.cinderella;application/vnd.intercon.formnet;application/vnd.isac.fcs;application/ipfix;application/pkix-cert;application/pkixcmp;application/pkix-crl;application/pkix-pkipath;applicaion/vnd.insors.igm;application/vnd.ipunplugged.rcprofile;application/vnd.irepository.package+xml;text/vnd.sun.j2me.app-descriptor;application/java-archive;application/java-vm;application/x-java-jnlp-file;application/java-serializd-object;text/x-java-source,java;application/javascript;application/json;application/vnd.joost.joda-archive;video/jpm;image/jpeg;video/jpeg;application/vnd.kahootz;application/vnd.chipnuts.karaoke-mmd;application/vnd.kde.karbon;aplication/vnd.kde.kchart;application/vnd.kde.kformula;application/vnd.kde.kivio;application/vnd.kde.kontour;application/vnd.kde.kpresenter;application/vnd.kde.kspread;application/vnd.kde.kword;application/vnd.kenameaapp;applicatin/vnd.kidspiration;application/vnd.kinar;application/vnd.kodak-descriptor;application/vnd.las.las+xml;application/x-latex;application/vnd.llamagraphics.life-balance.desktop;application/vnd.llamagraphics.life-balance.exchange+xml;application/vnd.jam;application/vnd.lotus-1-2-3;application/vnd.lotus-approach;application/vnd.lotus-freelance;application/vnd.lotus-notes;application/vnd.lotus-organizer;application/vnd.lotus-screencam;application/vnd.lotus-wordro;audio/vnd.lucent.voice;audio/x-mpegurl;video/x-m4v;application/mac-binhex40;application/vnd.macports.portpkg;application/vnd.osgeo.mapguide.package;application/marc;application/marcxml+xml;application/mxf;application/vnd.wolfrm.player;application/mathematica;application/mathml+xml;application/mbox;application/vnd.medcalcdata;application/mediaservercontrol+xml;application/vnd.mediastation.cdkey;application/vnd.mfer;application/vnd.mfmp;model/mesh;appliation/mads+xml;application/mets+xml;application/mods+xml;application/metalink4+xml;application/vnd.ms-powerpoint.template.macroenabled.12;application/vnd.ms-word.document.macroenabled.12;application/vnd.ms-word.template.macroenabed.12;application/vnd.mcd;application/vnd.micrografx.flo;application/vnd.micrografx.igx;application/vnd.eszigno3+xml;application/x-msaccess;video/x-ms-asf;application/x-msdownload;application/vnd.ms-artgalry;application/vnd.ms-ca-compressed;application/vnd.ms-ims;application/x-ms-application;application/x-msclip;image/vnd.ms-modi;application/vnd.ms-fontobject;application/vnd.ms-excel;application/vnd.ms-excel.addin.macroenabled.12;application/vnd.ms-excelsheet.binary.macroenabled.12;application/vnd.ms-excel.template.macroenabled.12;application/vnd.ms-excel.sheet.macroenabled.12;application/vnd.ms-htmlhelp;application/x-mscardfile;application/vnd.ms-lrm;application/x-msmediaview;aplication/x-msmoney;application/vnd.openxmlformats-officedocument.presentationml.presentation;application/vnd.openxmlformats-officedocument.presentationml.slide;application/vnd.openxmlformats-officedocument.presentationml.slideshw;application/vnd.openxmlformats-officedocument.presentationml.template;application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;application/vnd.openxmlformats-officedocument.spreadsheetml.template;application/vnd.openxmformats-officedocument.wordprocessingml.document;application/vnd.openxmlformats-officedocument.wordprocessingml.template;application/x-msbinder;application/vnd.ms-officetheme;application/onenote;audio/vnd.ms-playready.media.pya;vdeo/vnd.ms-playready.media.pyv;application/vnd.ms-powerpoint;application/vnd.ms-powerpoint.addin.macroenabled.12;application/vnd.ms-powerpoint.slide.macroenabled.12;application/vnd.ms-powerpoint.presentation.macroenabled.12;appliation/vnd.ms-powerpoint.slideshow.macroenabled.12;application/vnd.ms-project;application/x-mspublisher;application/x-msschedule;application/x-silverlight-app;application/vnd.ms-pki.stl;application/vnd.ms-pki.seccat;application/vn.visio;video/x-ms-wm;audio/x-ms-wma;audio/x-ms-wax;video/x-ms-wmx;application/x-ms-wmd;application/vnd.ms-wpl;application/x-ms-wmz;video/x-ms-wmv;video/x-ms-wvx;application/x-msmetafile;application/x-msterminal;application/msword;application/x-mswrite;application/vnd.ms-works;application/x-ms-xbap;application/vnd.ms-xpsdocument;audio/midi;application/vnd.ibm.minipay;application/vnd.ibm.modcap;application/vnd.jcp.javame.midlet-rms;application/vnd.tmobile-ivetv;application/x-mobipocket-ebook;application/vnd.mobius.mbk;application/vnd.mobius.dis;application/vnd.mobius.plc;application/vnd.mobius.mqy;application/vnd.mobius.msl;application/vnd.mobius.txf;application/vnd.mobius.daf;tex/vnd.fly;application/vnd.mophun.certificate;application/vnd.mophun.application;video/mj2;audio/mpeg;video/vnd.mpegurl;video/mpeg;application/mp21;audio/mp4;video/mp4;application/mp4;application/vnd.apple.mpegurl;application/vnd.msician;application/vnd.muvee.style;application/xv+xml;application/vnd.nokia.n-gage.data;application/vnd.nokia.n-gage.symbian.install;application/x-dtbncx+xml;application/x-netcdf;application/vnd.neurolanguage.nlu;application/vnd.na;application/vnd.noblenet-directory;application/vnd.noblenet-sealer;application/vnd.noblenet-web;application/vnd.nokia.radio-preset;application/vnd.nokia.radio-presets;text/n3;application/vnd.novadigm.edm;application/vnd.novadim.edx;application/vnd.novadigm.ext;application/vnd.flographit;audio/vnd.nuera.ecelp4800;audio/vnd.nuera.ecelp7470;audio/vnd.nuera.ecelp9600;application/oda;application/ogg;audio/ogg;video/ogg;application/vnd.oma.dd2+xml;applicatin/vnd.oasis.opendocument.text-web;application/oebps-package+xml;application/vnd.intu.qbo;application/vnd.openofficeorg.extension;application/vnd.yamaha.openscoreformat;audio/webm;video/webm;application/vnd.oasis.opendocument.char;application/vnd.oasis.opendocument.chart-template;application/vnd.oasis.opendocument.database;application/vnd.oasis.opendocument.formula;application/vnd.oasis.opendocument.formula-template;application/vnd.oasis.opendocument.grapics;application/vnd.oasis.opendocument.graphics-template;application/vnd.oasis.opendocument.image;application/vnd.oasis.opendocument.image-template;application/vnd.oasis.opendocument.presentation;application/vnd.oasis.opendocumen.presentation-template;application/vnd.oasis.opendocument.spreadsheet;application/vnd.oasis.opendocument.spreadsheet-template;application/vnd.oasis.opendocument.text;application/vnd.oasis.opendocument.text-master;application/vnd.asis.opendocument.text-template;image/ktx;application/vnd.sun.xml.calc;application/vnd.sun.xml.calc.template;application/vnd.sun.xml.draw;application/vnd.sun.xml.draw.template;application/vnd.sun.xml.impress;application/vnd.sun.xl.impress.template;application/vnd.sun.xml.math;application/vnd.sun.xml.writer;application/vnd.sun.xml.writer.global;application/vnd.sun.xml.writer.template;application/x-font-otf;application/vnd.yamaha.openscoreformat.osfpvg+xml;application/vnd.osgi.dp;application/vnd.palm;text/x-pascal;application/vnd.pawaafile;application/vnd.hp-pclxl;application/vnd.picsel;image/x-pcx;image/vnd.adobe.photoshop;application/pics-rules;image/x-pict;application/x-chat;aplication/pkcs10;application/x-pkcs12;application/pkcs7-mime;application/pkcs7-signature;application/x-pkcs7-certreqresp;application/x-pkcs7-certificates;application/pkcs8;application/vnd.pocketlearn;image/x-portable-anymap;image/-portable-bitmap;application/x-font-pcf;application/font-tdpfr;application/x-chess-pgn;image/x-portable-graymap;image/png;image/x-portable-pixmap;application/pskc+xml;application/vnd.ctc-posml;application/postscript;application/xfont-type1;application/vnd.powerbuilder6;application/pgp-encrypted;application/pgp-signature;application/vnd.previewsystems.box;application/vnd.pvi.ptid1;application/pls+xml;application/vnd.pg.format;application/vnd.pg.osasli;tex/prs.lines.tag;application/x-font-linux-psf;application/vnd.publishare-delta-tree;application/vnd.pmi.widget;application/vnd.quark.quarkxpress;application/vnd.epson.esf;application/vnd.epson.msf;application/vnd.epson.ssf;applicaton/vnd.epson.quickanime;application/vnd.intu.qfx;video/quicktime;application/x-rar-compressed;audio/x-pn-realaudio;audio/x-pn-realaudio-plugin;application/rsd+xml;application/vnd.rn-realmedia;application/vnd.realvnc.bed;applicatin/vnd.recordare.musicxml;application/vnd.recordare.musicxml+xml;application/relax-ng-compact-syntax;application/vnd.data-vision.rdz;application/rdf+xml;application/vnd.cloanto.rp9;application/vnd.jisp;application/rtf;text/richtex;application/vnd.route66.link66+xml;application/rss+xml;application/shf+xml;application/vnd.sailingtracker.track;image/svg+xml;application/vnd.sus-calendar;application/sru+xml;application/set-payment-initiation;application/set-reistration-initiation;application/vnd.sema;application/vnd.semd;application/vnd.semf;application/vnd.seemail;application/x-font-snf;application/scvp-vp-request;application/scvp-vp-response;application/scvp-cv-request;application/svp-cv-response;application/sdp;text/x-setext;video/x-sgi-movie;application/vnd.shana.informed.formdata;application/vnd.shana.informed.formtemplate;application/vnd.shana.informed.interchange;application/vnd.shana.informed.package;application/thraud+xml;application/x-shar;image/x-rgb;application/vnd.epson.salt;application/vnd.accpac.simply.aso;application/vnd.accpac.simply.imp;application/vnd.simtech-mindmapper;application/vnd.commonspace;application/vnd.ymaha.smaf-audio;application/vnd.smaf;application/vnd.yamaha.smaf-phrase;application/vnd.smart.teacher;application/vnd.svd;application/sparql-query;application/sparql-results+xml;application/srgs;application/srgs+xml;application/sml+xml;application/vnd.koan;text/sgml;application/vnd.stardivision.calc;application/vnd.stardivision.draw;application/vnd.stardivision.impress;application/vnd.stardivision.math;application/vnd.stardivision.writer;application/vnd.tardivision.writer-global;application/vnd.stepmania.stepchart;application/x-stuffit;application/x-stuffitx;application/vnd.solent.sdkm+xml;application/vnd.olpc-sugar;audio/basic;application/vnd.wqd;application/vnd.symbian.install;application/smil+xml;application/vnd.syncml+xml;application/vnd.syncml.dm+wbxml;application/vnd.syncml.dm+xml;application/x-sv4cpio;application/x-sv4crc;application/sbml+xml;text/tab-separated-values;image/tiff;application/vnd.to.intent-module-archive;application/x-tar;application/x-tcl;application/x-tex;application/x-tex-tfm;application/tei+xml;text/plain;application/vnd.spotfire.dxp;application/vnd.spotfire.sfs;application/timestamped-data;applicationvnd.trid.tpt;application/vnd.triscape.mxs;text/troff;application/vnd.trueapp;application/x-font-ttf;text/turtle;application/vnd.umajin;application/vnd.uoml+xml;application/vnd.unity;application/vnd.ufdl;text/uri-list;application/nd.uiq.theme;application/x-ustar;text/x-uuencode;text/x-vcalendar;text/x-vcard;application/x-cdlink;application/vnd.vsf;model/vrml;application/vnd.vcx;model/vnd.mts;model/vnd.vtu;application/vnd.visionary;video/vnd.vivo;applicatin/ccxml+xml,;application/voicexml+xml;application/x-wais-source;application/vnd.wap.wbxml;image/vnd.wap.wbmp;audio/x-wav;application/davmount+xml;application/x-font-woff;application/wspolicy+xml;image/webp;application/vnd.webturb;application/widget;application/winhlp;text/vnd.wap.wml;text/vnd.wap.wmlscript;application/vnd.wap.wmlscriptc;application/vnd.wordperfect;application/vnd.wt.stf;application/wsdl+xml;image/x-xbitmap;image/x-xpixmap;image/x-xwindowump;application/x-x509-ca-cert;application/x-xfig;application/xhtml+xml;application/xml;application/xcap-diff+xml;application/xenc+xml;application/patch-ops-error+xml;application/resource-lists+xml;application/rls-services+xml;aplication/resource-lists-diff+xml;application/xslt+xml;application/xop+xml;application/x-xpinstall;application/xspf+xml;application/vnd.mozilla.xul+xml;chemical/x-xyz;text/yaml;application/yang;application/yin+xml;application/vnd.ul;application/zip;application/vnd.handheld-entertainment+xml;application/vnd.zzazz.deck+xml");
                if (SHAFT.Properties.flags.disableCache()) {
                    ffProfile.setPreference("browser.cache.disk.enable", false);
                    ffProfile.setPreference("browser.cache.memory.enable", false);
                    ffProfile.setPreference("browser.cache.offline.enable", false);
                    ffProfile.setPreference("network.http.use-cache", false);
                }

                // attempted fix for `org.openqa.selenium.WebDriverException: SecurityError: Permission denied to access property "pageXOffset" on cross-origin object`
                ffProfile.setPreference("browser.contentblocking.enabled", false);
//                ffProfile.setPreference("privacy.trackingprotection.enabled", false);

                ffOptions.setProfile(ffProfile);
                if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local"))
                    ffOptions.setCapability(CapabilityType.PLATFORM_NAME, Properties.platform.targetPlatform());
                if (SHAFT.Properties.web.headlessExecution()) {
                    ffOptions.addArguments("-headless");
                }
                //Incognito mode for Firefox
                if (SHAFT.Properties.web.incognitoMode()) {
                    ffOptions.addArguments("-private");
                }
                ffOptions.setLogLevel(FirefoxDriverLogLevel.WARN);
                ffOptions.setPageLoadStrategy(PageLoadStrategy.NORMAL);
                ffOptions.setPageLoadTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.pageLoadTimeout()));
                ffOptions.setScriptTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.scriptExecutionTimeout()));
                //Add Proxy Setting if found
                if (SHAFT.Properties.platform.driverProxySettings() && !proxyServerSettings.isBlank()) {
                    Proxy proxy = new Proxy();
                    proxy.setHttpProxy(proxyServerSettings);
                    proxy.setSslProxy(proxyServerSettings);
                    ffOptions.setProxy(proxy);
                }
                ffOptions.setCapability(CapabilityType.UNHANDLED_PROMPT_BEHAVIOUR, UnhandledPromptBehavior.IGNORE);
                // Enable BiDi
                ffOptions.setCapability("webSocketUrl", SHAFT.Properties.platform.enableBiDi());
                //merge customWebDriverCapabilities.properties
                ffOptions = ffOptions.merge(PropertyFileManager.getCustomWebDriverDesiredCapabilities());
                //merge hardcoded custom options
                if (customDriverOptions != null) {
                    ffOptions = ffOptions.merge(customDriverOptions);
                }
                setSeleniumManagerOptions(ffOptions);
                ReportManager.logDiscrete(ffOptions.toString());
            }
            case IE -> {
                ieOptions = new InternetExplorerOptions();
                if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local"))
                    ieOptions.setCapability(CapabilityType.PLATFORM_NAME, Properties.platform.targetPlatform());
                ieOptions.setPageLoadStrategy(PageLoadStrategy.NORMAL);
                ieOptions.setPageLoadTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.pageLoadTimeout()));
                ieOptions.setScriptTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.scriptExecutionTimeout()));
                //Add Proxy Setting if found
                if (SHAFT.Properties.platform.driverProxySettings() && !proxyServerSettings.isBlank()) {
                    Proxy proxy = new Proxy();
                    proxy.setHttpProxy(proxyServerSettings);
                    proxy.setSslProxy(proxyServerSettings);
                    ieOptions.setProxy(proxy);
                }
                if (SHAFT.Properties.flags.disableCache()) {
                    ieOptions.setCapability(InternetExplorerDriver.IE_ENSURE_CLEAN_SESSION, true);
                    ieOptions.setCapability("applicationCacheEnabled", false);
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
                if (driverType.equals(DriverFactory.DriverType.EDGE)) {
                    edOptions = (EdgeOptions) setupChromiumOptions(new EdgeOptions(), customDriverOptions);
                    ReportManager.logDiscrete(edOptions.toString());
                } else {
                    chOptions = (ChromeOptions) setupChromiumOptions(new ChromeOptions(), customDriverOptions);
                    setSeleniumManagerOptions(chOptions);
                    ReportManager.logDiscrete(chOptions.toString());
                }
            }
            case SAFARI, WEBKIT -> {
                sfOptions = new SafariOptions();
                if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local"))
                    sfOptions.setCapability(CapabilityType.PLATFORM_NAME, Properties.platform.targetPlatform());
                sfOptions.setCapability(CapabilityType.UNHANDLED_PROMPT_BEHAVIOUR, UnhandledPromptBehavior.IGNORE);
                sfOptions.setPageLoadStrategy(PageLoadStrategy.NORMAL);
                sfOptions.setPageLoadTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.pageLoadTimeout()));
                sfOptions.setScriptTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.scriptExecutionTimeout()));
                //Add Proxy Setting if found
                if (SHAFT.Properties.platform.driverProxySettings() && !proxyServerSettings.isBlank()) {
                    Proxy proxy = new Proxy();
                    proxy.setHttpProxy(proxyServerSettings);
                    proxy.setSslProxy(proxyServerSettings);
                    sfOptions.setProxy(proxy);
                }
                if (SHAFT.Properties.flags.disableCache()) {
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
                    DriverFactoryHelper.failAction("Unsupported Driver Type \"" + JavaHelper.convertToSentenceCase(driverType.getValue()) + "\".");
        }
    }

    private synchronized void setSeleniumManagerOptions(MutableCapabilities options) {
        var fileActions = FileActions.getInstance(true);
        // configure selenium manager to force download chrome binaries
        String folderPath = System.getProperty("user.home") + File.separatorChar + ".cache" + File.separatorChar + "selenium" + File.separatorChar;
        String fileName = "se-config.toml";

        if (SHAFT.Properties.web.forceBrowserDownload()) {
            if (options instanceof ChromeOptions chromeOptions) {
                chromeOptions.setBrowserVersion("stable");
            } else if (options instanceof FirefoxOptions firefoxOptions) {
                firefoxOptions.setBrowserVersion("stable");
            }

            if (fileActions.doesFileExist(folderPath, fileName, 1)) {
                String configFileContent = fileActions.readFile(folderPath, fileName);
                if (!configFileContent.contains("force-browser-download = true"))
                    fileActions.writeToFile(folderPath, fileName
                            , configFileContent + System.lineSeparator() + "force-browser-download = true");
            } else {
                fileActions.createFile(folderPath, fileName);
                fileActions.writeToFile(folderPath, fileName, "force-browser-download = true");
            }
        } else {
            if (fileActions.doesFileExist(folderPath, fileName, 1)) {
                fileActions.deleteFile(folderPath + fileName);
            }
        }
    }

    @SuppressWarnings("SpellCheckingInspection")
    protected void initializeMobileDesiredCapabilities() {
        switch (Properties.platform.targetPlatform().toLowerCase()) {
            case "android" -> appiumCapabilities.setPlatform(Platform.ANDROID);
            case "ios" -> appiumCapabilities.setPlatform(Platform.IOS);
            default -> appiumCapabilities.setPlatform(Platform.ANY);
        }
        if (!DriverFactoryHelper.isMobileWebExecution()) {
            Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();
            caps.forEach((capabilityName, value) -> {
                if (!value.isBlank()) {
                    if (Arrays.asList("true", "false").contains(value.trim().toLowerCase())) {
                        appiumCapabilities.setCapability(capabilityName.replace("mobile_", "appium:"), Boolean.valueOf(value));
                    } else if (StringUtils.isStrictlyNumeric(value.trim())) {
                        appiumCapabilities.setCapability(capabilityName.replace("mobile_", "appium:"), Integer.valueOf(value));
                    } else {
                        appiumCapabilities.setCapability(capabilityName.replace("mobile_", "appium:"), value);
                    }
                }
            });
        }

        if (DriverFactoryHelper.isMobileWebExecution()) {
            //https://chromedriver.chromium.org/capabilities
            appiumCapabilities.setBrowserName(SHAFT.Properties.mobile.browserName());
            appiumCapabilities.setCapability("pageLoadStrategy", PageLoadStrategy.NONE);
        }
        /*
        if (!DriverFactoryHelper.isMobileWebExecution() && Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            // experimental android capabilities
            // https://github.com/appium/appium-uiautomator2-driver
            // Check if user sent any capability then don't take the deafult

            if (appiumCapabilities.getCapability("appium:fullReset") == null)
                appiumCapabilities.setCapability("appium:fullReset", true);

            if (appiumCapabilities.getCapability("appium:appWaitActivity") == null)
                appiumCapabilities.setCapability("appium:appWaitActivity", "*");

            if (appiumCapabilities.getCapability("appium:printPageSourceOnFindFailure") == null)
                appiumCapabilities.setCapability("appium:printPageSourceOnFindFailure", true);

            if (appiumCapabilities.getCapability("appium:disableWindowAnimation") == null)
                appiumCapabilities.setCapability("appium:disableWindowAnimation", true);

            if (appiumCapabilities.getCapability("appium:forceAppLaunch") == null)
                appiumCapabilities.setCapability("appium:forceAppLaunch", true);

            if (appiumCapabilities.getCapability("appium:autoGrantPermissions") == null)
                appiumCapabilities.setCapability("appium:autoGrantPermissions", true);

//            appiumCapabilities.setCapability("appium:otherApps", ",,,");

            if (appiumCapabilities.getCapability("appium:allowTestPackages") == null)
                appiumCapabilities.setCapability("appium:allowTestPackages", true);

            if (appiumCapabilities.getCapability("appium:enforceAppInstall") == null)
                appiumCapabilities.setCapability("appium:enforceAppInstall", false);

            if (appiumCapabilities.getCapability("appium:clearDeviceLogsOnStart") == null)
                appiumCapabilities.setCapability("appium:clearDeviceLogsOnStart", true);

            if (appiumCapabilities.getCapability("appium:ignoreHiddenApiPolicyError") == null)
                appiumCapabilities.setCapability("appium:ignoreHiddenApiPolicyError", true);

            if (appiumCapabilities.getCapability("appium:isHeadless") == null)
                appiumCapabilities.setCapability("appium:isHeadless", true);

            if (appiumCapabilities.getCapability("appium:noSign") == null)
                appiumCapabilities.setCapability("appium:noSign", true);

            if (appiumCapabilities.getCapability("appium:enableWebviewDetailsCollection") == null)
                appiumCapabilities.setCapability("appium:enableWebviewDetailsCollection", true);

            if (appiumCapabilities.getCapability("appium:showChromedriverLog") == null)
                appiumCapabilities.setCapability("appium:showChromedriverLog", true);
        }
         */
        
        ReportManager.log(appiumCapabilities.toString());
    }

    @SuppressWarnings("SpellCheckingInspection")
    private ChromiumOptions<?> setupChromiumOptions(ChromiumOptions<?> options, MutableCapabilities customDriverOptions) {
        String executionAddress = SHAFT.Properties.platform.executionAddress().toLowerCase();
        if (!executionAddress.equalsIgnoreCase("local"))
            options.setCapability(CapabilityType.PLATFORM_NAME, Properties.platform.targetPlatform());
        if (SHAFT.Properties.web.headlessExecution()) {
            options.addArguments("--headless=new");
            // https://github.com/GoogleChrome/chrome-launcher/blob/main/docs/chrome-flags-for-tools.md#headless
        }
        // Fix "org.openqa.selenium.TimeoutException: timeout: Timed out receiving message from renderer: 10.000" on chrome/mac
        // https://github.com/ultrafunkamsterdam/undetected-chromedriver/issues/1280
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.CHROME.browserName())) {
            //         https://github.com/GoogleChrome/chrome-launcher/blob/main/docs/chrome-flags-for-tools.md
            //         https://docs.google.com/spreadsheets/d/1n-vw_PCPS45jX3Jt9jQaAhFqBY6Ge1vWF_Pa0k7dCk4/edit#gid=1265672696
            options.addArguments("--disable-search-engine-choice-screen"
                    , "--remote-allow-origins=*"
                    , "--enable-automation"
                    , "--disable-background-timer-throttling"
                    , "--disable-backgrounding-occluded-windows"
                    , "--disable-features=OptimizationGuideModelDownloading,OptimizationHintsFetching,OptimizationTargetPrediction,OptimizationHints,CalculateNativeWinOcclusion,AutofillServerCommunication,MediaRouter,Translate,AvoidUnnecessaryBeforeUnloadCheckSync,CertificateTransparencyComponentUpdater,OptimizationHints,DialMediaRouteProvider,GlobalMediaControls,ImprovedCookieControls,LazyFrameLoading,InterestFeedContentSuggestions"
                    , "--disable-hang-monitor"
                    , "--disable-domain-reliability"
                    , "--disable-renderer-backgrounding"
                    , "--metrics-recording-only"
                    , "--no-first-run"
                    , "--no-default-browser-check"
                    , "--silent-debugger-extension-api"
                    , "--disable-extensions"
                    , "--disable-component-extensions-with-background-pages"
                    , "--disable-dev-shm-usage"
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
                    , "--disable-sync"
                    , "--disable-field-trial-config"
                    , "--enable-features=NetworkService"
                    , "--enable-features=NetworkServiceInProcess"
                    , "--enable-use-zoom-for-dsf"
                    , "--log-net-log"
                    , "--net-log-capture-mode"
                    , "--disable-client-side-phishing-detection"
                    , "--disable-default-apps");
        }
        // Add if condtion to start the new session if flag=true on specific port
        if (SHAFT.Properties.performance.isEnabled()) {
            options.addArguments("--remote-debugging-port=" + SHAFT.Properties.performance.port());
        } else {
            options.addArguments("--remote-debugging-pipe");
        }
        options.addArguments("--no-sandbox");

        if (SHAFT.Properties.flags.autoMaximizeBrowserWindow() && !Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) && !Platform.IOS.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform()) && !Platform.MAC.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            options.addArguments("--start-maximized");
        } else {
            options.addArguments("--window-position=0,0", "--window-size=" + DriverFactoryHelper.getTARGET_WINDOW_SIZE().getWidth() + "," + DriverFactoryHelper.getTARGET_WINDOW_SIZE().getHeight());
        }
        if (!SHAFT.Properties.flags.autoCloseDriverInstance()) options.setExperimentalOption("detach", true);
        //Incognito mode for Chrome and Edge
        if (SHAFT.Properties.web.incognitoMode()) {
            if (options.getBrowserName().equals(DriverFactory.DriverType.CHROME.getValue())) {
                options.addArguments("--incognito");
            } else if (options.getBrowserName().equals(DriverFactory.DriverType.EDGE.getValue())) {
                options.addArguments("inPrivate");
            }
        }
        Map<String, Object> chromePreferences = new HashMap<>();
        chromePreferences.put("profile.default_content_settings.popups", 0);
        chromePreferences.put("download.prompt_for_download", "false");
        chromePreferences.put("download.default_directory", System.getProperty("user.dir") + File.separatorChar + SHAFT.Properties.paths.downloads().replace("/", File.separator));
        options.setExperimentalOption("prefs", chromePreferences);
        options.setUnhandledPromptBehaviour(UnexpectedAlertBehaviour.IGNORE);
        options.setCapability(CapabilityType.ACCEPT_INSECURE_CERTS, true);
        options.setPageLoadStrategy(PageLoadStrategy.NORMAL); // https://www.skptricks.com/2018/08/timed-out-receiving-message-from-renderer-selenium.html
        options.setPageLoadTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.pageLoadTimeout()));
        options.setScriptTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.scriptExecutionTimeout()));
        options.setCapability(CapabilityType.UNHANDLED_PROMPT_BEHAVIOUR, UnhandledPromptBehavior.IGNORE);
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
        if (SHAFT.Properties.web.isMobileEmulation() && DriverFactoryHelper.isNotMobileExecution()) {
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
        options.setCapability("webSocketUrl", SHAFT.Properties.platform.enableBiDi());
        //merge customWebdriverCapabilities.properties
        options = (ChromiumOptions<?>) options.merge(PropertyFileManager.getCustomWebDriverDesiredCapabilities());
        //merge hardcoded custom options
        if (customDriverOptions != null) {
            options = (ChromiumOptions<?>) options.merge(customDriverOptions);
        }

        if (!SHAFT.Properties.flags.autoCloseDriverInstance()) {
            Map<Object, Object> chromeOptions = new HashMap<>((Map<Object, Object>) options.getCapability(ChromeOptions.CAPABILITY));
            chromeOptions.put("detach", true);
            options.setCapability(ChromeOptions.CAPABILITY, chromeOptions);
        }
        return options;
    }

    private LoggingPreferences configureLoggingPreferences() {
        //Add logging setting if enabled
        LoggingPreferences logPrefs = new LoggingPreferences();
        logPrefs.enable(LogType.PERFORMANCE, java.util.logging.Level.ALL);
        logPrefs.enable(LogType.DRIVER, java.util.logging.Level.ALL);
        logPrefs.enable(LogType.BROWSER, java.util.logging.Level.ALL);
        logPrefs.enable(LogType.DRIVER, java.util.logging.Level.ALL);
        return logPrefs;
    }
}
