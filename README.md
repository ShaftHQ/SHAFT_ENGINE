<img src="src/main/resources/images/shaft.png" alt="SHAFT_ENGINE" style="display:block; margin-left:auto; margin-right:auto;"/>

<!-- Badges provided by https://shields.io/ -->
[![License](https://img.shields.io/github/license/ShaftHQ/SHAFT_Engine?color=indigo&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/blob/master/LICENSE)
 [![Contributors)](https://img.shields.io/github/contributors/ShaftHQ/SHAFT_ENGINE?color=indigo&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/graphs/contributors)
 [![JFrog release (latest by date including pre-releases)](https://img.shields.io/github/v/release/ShaftHQ/shaft_engine?include_prereleases&color=indigo&label=Latest%20Release&style=for-the-badge)](https://automatest.jfrog.io/ui/packages/gav:%2F%2Fio.github.shafthq:SHAFT_ENGINE)

[![GitHub Workflow Status (with branch)](https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/e2eTests.yml?branch=master&color=forestgreen&label=E2E%20Tests&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/e2eTests.yml)
 [![GitHub Workflow Status (with branch)](https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/jfrog_cd.yml?branch=master&color=forestgreen&label=Continuous%20Releases&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/jfrog_cd.yml)

[![Codacy grade](https://img.shields.io/codacy/grade/de526d8e36ad4021a2f8f5ed553e4976?style=for-the-badge&color=blue&label=Code%20Quality)](https://app.codacy.com/gh/ShaftHQ/SHAFT_ENGINE/dashboard)
 [![GitHub Workflow Status (with branch)](https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/codeql-analysis.yml?branch=master&label=Security&color=blue&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/codeql-analysis.yml)
 [![Codecov](https://img.shields.io/codecov/c/github/shafthq/shaft_engine?style=for-the-badge&label=Coverage&color=blue)](https://app.codecov.io/gh/ShaftHQ/SHAFT_ENGINE)
 

# SHAFT: Unified Test Automation Engine


[🦸‍♂️ Powered by the best](https://github.com/ShaftHQ/SHAFT_ENGINE#-powered-by) - [🟢 Selenium Ecosystem proud member](https://www.selenium.dev/ecosystem/) - [👨‍🏫 Fully Documented](https://ShaftHQ.github.io/SHAFT_Engine_Docusaurus/)

[🤝 Fully supported](https://github.com/ShaftHQ/SHAFT_ENGINE#-support--contributions) - [🌍 Used by thousands](https://github.com/ShaftHQ/SHAFT_ENGINE#-who-else-is-using-shaft-2) - [🤨 Oddly familiar](https://github.com/ShaftHQ/SHAFT_ENGINE#-tech-stack)

[🔋 Batteries included](https://github.com/MohabMohie/using_SHAFT_Engine) - [🪄 No magic required](https://ShaftHQ.github.io/SHAFT_ENGINE/).


<a id="quick-start-guide"></a>
## 🏃 Quick Start Guide

### Option 1: Template Project
- Use our [Template Project](https://github.com/MohabMohie/using_SHAFT_Engine) to get started with one click.

### Option 2: New Project
#### Step 1: Initial Setup
- Create a new Java/Maven project using Eclipse, IntelliJ or your favourite IDE.
- Copy the highlighted contents of this [pom.xml](https://github.com/MohabMohie/using_SHAFT_Engine/blob/main/GUI_Web/pom.xml#L11-L80) file into yours inside the ```<project>``` tag.
- Follow the steps in this footnote in case you are using IntelliJ[^1].

#### Step 2: Creating Tests
- Create a new Package ```TestPackage``` under ```src/test/java``` and create a new Java Class ```TestClass``` under that package.
- Copy the below imports into your newly created java class.
```java
import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
```
- Copy the below code snippet into your newly created java class.
```java
    SHAFT.GUI.WebDriver driver;
    SHAFT.TestData.JSON testData;

    By searchBox = By.name("q");
    By resultStats = By.id("result-stats");

    @Test
    public void test() {
        driver.browser().navigateToURL("https://www.google.com/");
        driver.verifyThat().browser().title().isEqualTo("Google").perform();
        driver.element().type(searchBox, testData.getTestData("searchQuery"))
                .keyPress(searchBox, Keys.ENTER);
        driver.assertThat().element(resultStats).text().doesNotEqual("")
                .withCustomReportMessage("Check that result stats is not empty").perform();
    }

    @BeforeClass
    public void beforeClass() {
        driver = new SHAFT.GUI.WebDriver();
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
        }

@AfterClass(alwaysRun = true)
public void afterClass(){
        driver.quit();
    }
```

#### Step 3: Managing Test Data
- Create the following file ```src/test/resources/testDataFiles/simpleJSON.json```.
- Copy the below code snippet into your newly created json file.
```json
{
  "searchQuery": "SHAFT_Engine"
}
```

#### Step 4: Running Tests
- Run your ```TestClass.java``` as a TestNG Test Class.
- The execution report will open automatically in your default web browser after the test run is completed.
- <b>Join</b> our ![GitHub Repo stars](https://img.shields.io/github/stars/shafthq/shaft_engine?logoColor=black&style=social) to get notified by email when a new release is pushed out.
- After upgrading your Engine to a new major release it is sometimes recommended to delete the properties
  folder ```src\main\resources\properties``` and allow SHAFT to regenerate the defaults by running any test method.
  [^1]: Due to a known issue with IntelliJ you need to edit your run configuration templates before running your tests
  by following these steps:
  <br/>- Open 'Edit Run/Debug Configurations' dialog > Edit Configurations... > Edit configuration templates...
  <br/>- Select <b>TestNG</b> > Listeners > and add these listeners one by one:
  <br/>`io.github.shafthq.shaft.listeners.TestNGListener`
  <br/>- Select <b>Cucumber Java</b> > Program Arguments > and add this argument:
  <br/>`--plugin io.github.shafthq.shaft.listeners.CucumberFeatureListener`
  <br/>- After saving the changes, remember to delete any old test runs you may have triggered by mistake before adding
  the needed config.


## 👨‍💻 Tech Stack
<a href="https://www.oracle.com/eg/java/technologies/downloads/" target="_blank"><img src="https://www.chrisjmendez.com/content/images/2019/01/Java_logo_icon.png" alt="Java" height="50px"></a>
<a href="https://maven.apache.org/" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/52/Apache_Maven_logo.svg/340px-Apache_Maven_logo.svg.png" alt="Maven" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.jetbrains.com/idea/" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/9c/IntelliJ_IDEA_Icon.svg/1200px-IntelliJ_IDEA_Icon.svg.png" alt="IntelliJ IDEA" height="50px"></a>


## 🦸 Powered by
<a href="https://www.selenium.dev/" target="_blank"><img src="https://www.selenium.dev/images/selenium_4_logo.png" alt="Selenium WebDriver" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://appium.github.io/appium/docs/en/2.0/" target="_blank"><img src="https://appium.github.io/appium/docs/en/2.0/assets/images/appium-logo-horiz.png" alt="Appium" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://rest-assured.io/" target="_blank"><img src="https://avatars.githubusercontent.com/u/19369327?s=280&v=4" alt="REST Assured" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://testng.org/doc/" target="_blank"><img src="https://545767148-files.gitbook.io/~/files/v0/b/gitbook-x-prod.appspot.com/o/spaces%2F-MdBdUMSCcMYTyNwZf80%2Fuploads%2Fgit-blob-7e5b23257dbb5cc3262c56840d5cf9fa85b27dce%2Ftestng.png?alt=media" alt="TestNG" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://docs.qameta.io/allure/" target="_blank"><img src="https://avatars.githubusercontent.com/u/5879127?s=200&v=4" alt="Allure Reports" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.extentreports.com/" target="_blank"><img src="https://www.extentreports.com/wp-content/uploads/2018/09/Extent_logomark_transparentbg.png" alt="Extent Reports" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://cucumber.io/tools/cucumber-open/" target="_blank"><img src="https://github.com/cucumber/cucumber-ruby/raw/main/.github/img/cucumber-open-logo.png" alt="Cucumber.io" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://bonigarcia.dev/webdrivermanager/" target="_blank"><img src="https://bonigarcia.dev/webdrivermanager/img/wdm.png" alt="WebDriverManager" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.selenium.dev/documentation/grid/" target="_blank"><img src="https://media.softwaresim.com/Selenium_Grid_mpxkym-600.webp" alt="Selenium Grid" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.browserstack.com/" target="_blank"><img src="https://www.insightpartners.com//assets/media/2021/06/browserstack.png" alt="BrowserStack" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://sikulix.com/" target="_blank"><img src="https://raw.githubusercontent.com/RaiMan/SikuliX1/master/Support/sikulix-red.png" alt="SikuliX" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://opencv.org/" target="_blank"><img src="https://opencv.org/wp-content/uploads/2022/05/logo.png" alt="OpenCV" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://applitools.com/" target="_blank"><img src="https://www.selenium.dev/images/sponsors/applitools.png" alt="Applitools" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://github.com/features/actions" target="_blank"><img src="https://github.githubassets.com/images/modules/site/features/actions-icon-actions.svg" alt="GitHub Actions" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://github.com/dependabot" target="_blank"><img src="https://miro.medium.com/max/929/1*Lqt3yQYXJ-dmVuQEgpYcXQ.png" alt="Dependabot" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://app.codacy.com/gh/ShaftHQ/SHAFT_ENGINE/dashboard" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/9e/Codacy-logo-black.svg/2560px-Codacy-logo-black.svg.png" alt="Codacy" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://codeql.github.com/" target="_blank"><img src="https://github.gallerycdn.vsassets.io/extensions/github/vscode-codeql/1.7.7/1670939628664/Microsoft.VisualStudio.Services.Icons.Default" alt="CodeQL" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.eclemma.org/jacoco/" target="_blank"><img src="https://mkyong.com/wp-content/uploads/2018/11/Jacoco.png" alt="JaCoCo" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://app.codecov.io/gh/ShaftHQ/SHAFT_ENGINE" target="_blank"><img src="https://assets-global.website-files.com/5f217a8e6bc2c82a9d803089/6387929c3810ef832471584f_codecov.png" alt="CodeCov" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://automatest.jfrog.io/ui/packages/gav:%2F%2Fio.github.shafthq:SHAFT_ENGINE" target="_blank"><img src="https://speedmedia.jfrog.com/08612fe1-9391-4cf3-ac1a-6dd49c36b276/https://media.jfrog.com/wp-content/uploads/2021/12/29113553/jfrog-logo-2022.svg/mxw_96,f_auto" alt="jFrog" height="50px"></a>


<a id="support-and-contributions"></a>
## 🤝 Support & Contributions
- Join us via Slack & Facebook
<br/><a href="https://join.slack.com/t/automatest-workspace/shared_invite/zt-oii5i2gg-0ZGnih_Y34NjK7QqDn01Dw" target="_blank"><img src="https://a.slack-edge.com/80588/marketing/img/icons/icon_slack_hash_colored.png" alt="automatest-workspace" height="50"/></a>  <a href="https://www.facebook.com/groups/Automatest" target="_blank"><img src="https://facebookbrand.com/wp-content/uploads/2019/04/f_logo_RGB-Hex-Blue_512.png" alt="Automatest" height="50"/></a>
- And feel free to create PRs directly. [This lovely tutorial](https://dev.to/genicsblog/how-to-create-a-pull-request-in-github-correctly-20np) will help.


## 🌍 Who else is using SHAFT? [^4]
<img height="50" title="_VOIS (Vodafone Intelligent Solution)" alt="_VOIS (Vodafone Intelligent Solution)" src="https://www.vodafone.com/_next/image?url=https%3A%2F%2Fcontent.vodafone.com%2Fsites%2Fdefault%2Ffiles%2Finline-images%2FgN08grNr8s9vipkhltm4sWWezExdQg5LwJrGY2Ma2ojTjCnvi2.png&w=1600&q=100" href="https://www.vodafone.com/careers/professional-career-areas/shared-services">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="GET Group Holdings" alt="GET Group Holdings" src="https://www.getgroup.com/wp-content/themes/get-group-holdings/assets/images/logo-high-res-2.png" href="https://www.getgroup.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="MOMRA (Saudi Arabia's Ministry of Municipal and Rural Affairs)" alt="MOMRA (Saudi Arabia's Ministry of Municipal and Rural Affairs)" src="https://momrah.gov.sa/themes/custom/momrah/assets/images/mh-logo-full.png" href="https://momra.gov.sa/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Vodafone (Egypt)" alt="Vodafone (Egypt)" src="https://upload.wikimedia.org/wikipedia/commons/9/90/Logonewvodafone.png" href="https://www.vodafone.com.eg">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Solutions by STC" alt="Solutions by STC" src="https://solutions.com.sa/wp-content/uploads/2019/11/logo.svg" href="https://solutions.com.sa/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="GIZA Systems" alt="GIZA Systems" src="https://gizasystems.com/img/logo.png" href="https://gizasystems.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Euronet" alt="Euronet" src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Euronet_Worldwide_logo.svg/1920px-Euronet_Worldwide_logo.svg.png" href="https://www.euronetworldwide.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Terkwaz Business Solutions" alt="Terkwaz Business Solutions" src="https://images.wuzzuf-data.net/files/company_logo/Terkwaz-Solutions-Jordan-35434-1578830823.png" href="https://www.terkwaz.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Incorta" alt="Incorta" src="https://media-exp1.licdn.com/dms/image/C560BAQHUWHhKl0xrCA/company-logo_200_200/0/1660913597037?e=2147483647&v=beta&t=CiDPUEvlIBqztN5gCre-pQ5f7M-03_02IQgJtL18wG8" href="https://www.incorta.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="BayanTech" alt="BayanTech" src="https://bayan-tech.com/wp-content/uploads/2020/01/Bayan-Logo-2.png" href="https://bayan-tech.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Adam.ai" alt="Adam.ai" src="https://images.prismic.io/adamdotai/8e6625b0-e32b-4bee-b1b8-ebdc1d30cbfe_full-logo.svg?ixlib=gatsbyFP&auto=compress%2Cformat&fit=max&q=50" href="https://adam.ai/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="ACT Global Soft" alt="ACT Global Soft" src="https://www.act.eg/wp-content/uploads/2021/01/3.png" href="https://www.act.eg/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="elmenus" alt="elmenus" src="https://assets-global.website-files.com/625d19c77d49d0aa53047be3/625d19c77d49d0142c047d36_615c77e71175c38163bca403_elmenus.png" href="https://www.elmenus.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="IDEMIA" alt="IDEMIA" src="https://wikiimg.tojsiabtv.com/wikipedia/commons/thumb/2/2e/IDEMIA_Logo.jpg/1280px-IDEMIA_Logo.jpg" href="https://www.idemia.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="iHorizons" alt="iHorizons" src="https://www.ihorizons.com/sites/all/themes/ihorizons_theme/imgs/logo.png" href="https://www.ihorizons.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Robusta" alt="Robusta" src="https://images.wuzzuf-data.net/files/company_logo/Robusta-Egypt-7927.png" href="https://robustastudio.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Paymob Solutions" alt="Paymob Solutions" src="https://www.paymob.com/images/paymobLogo.png" href="https://www.paymob.com/">

[^4]: Company names are collected via anonymous surveys and provided freely by engineers who claimed to be using SHAFT_Engine within these companies.


## 🙏 Big thanks to the following organizations for their support to the project with their open source licenses
<a href="https://www.browserstack.com/" target="_blank"><img src="https://www.insightpartners.com//assets/media/2021/06/browserstack.png" alt="BrowserStack" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://applitools.com/" target="_blank"><img src="https://www.selenium.dev/images/sponsors/applitools.png" alt="Applitools" height="50px"></a>


#### Stop Reinventing the wheel! Start using SHAFT!
