<img src="src/main/resources/images/shaft.png" alt="SHAFT_ENGINE" style="display:block; margin-left:auto; margin-right:auto;"/>

<!-- Badges provided by https://shields.io/ -->
[![Contributors)](https://img.shields.io/github/contributors/ShaftHQ/SHAFT_ENGINE?color=blue&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/graphs/contributors)
 ![License](https://img.shields.io/github/license/ShaftHQ/SHAFT_Engine?color=blue&style=for-the-badge)
 [![JFrog release (latest by date including pre-releases)](https://img.shields.io/github/v/release/ShaftHQ/shaft_engine?include_prereleases&label=Latest%20Release&style=for-the-badge)](https://automatest.jfrog.io/ui/native/SHAFT_ENGINE/io/github/ShaftHQ/SHAFT_ENGINE/)

[![Codacy grade](https://img.shields.io/codacy/grade/3579cfd02a2c4f67bd1dce5dad0b1562?style=for-the-badge)](https://app.codacy.com/gh/ShaftHQ/SHAFT_ENGINE/dashboard)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/ShaftHQ/SHAFT_Engine/CodeQL?label=CodeQL&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions?query=workflow%3ACodeQL)

# SHAFT: Unified Test Automation Engine

 
<a id="quick-start-guide"></a>
## Quick Start Guide 🏃
### Step 1: Initial Setup
- Create a new Java/Maven project using Eclipse, IntelliJ or your favourite IDE.
- Copy the highlighted contents of this [pom.xml](https://github.com/MohabMohie/using_SHAFT_ENGINE/blob/master/pom.xml#L12-L79) file into yours inside the ```<project>``` tag.
- Follow the steps in this footnote in case you are using IntelliJ[^1].
### Step 2: Creating Tests
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

    @AfterClass
    public void afterClass() {
        driver.quit();
    }
```
### Step 3: Managing Test Data
- Create the following file ```src/test/resources/testDataFiles/simpleJSON.json```.
- Copy the below code snippet into your newly created json file.
```json
{
  "searchQuery": "SHAFT_Engine"
}
```
### Step 4: Running Tests
- Run your ```TestClass.java``` as a TestNG Test Class.
- The execution report will open automatically in your default web browser after the test run is completed.
### Step 5: More Information
- You can change the target browser, operating system, timeouts, and other configurations using the ⚙️ [Configuration Manager](https://ShaftHQ.github.io/SHAFT_ENGINE/).
- [Click Here](https://github.com/ShaftHQ/SHAFT_ENGINE/tree/master/src/test/java/testPackage01/SHAFTWizard) for more GUI, API, DB, and CLI sample test classes.
- And you can learn more from the 👤 [User Guide](https://ShaftHQ.github.io/SHAFT_Engine_Docusaurus/) and 📚 [Javadocs](https://ShaftHQ.github.io/SHAFT_ENGINE/apidocs/index.html) [^2].
- Here is also a complete tutorial showing everything from creating the project to running remote, unattended, parallelized, cross-platform tests ▶️ [Youtube: Test Automation Hero++](https://www.youtube.com/playlist?list=PLlnkmUosVw9g1IK6M4kZS8a-EsP4xb0Vf) [^3].
- Make sure to <b>Star</b> ⭐ the project to get notified when a new release is pushed out.
- After upgrading your Engine it is sometimes recommended to delete the properties folder ```src\main\resources\properties``` and allow SHAFT to regenerate the defaults by running any test method.
- Lastly, feel free to [Join us via Slack & Facebook](#support-and-contributions) for support and contributions.
[^1]: Due to a known issue with IntelliJ you need to edit your run configuration templates before running your tests by following these steps:
<br/>- Open 'Edit Run/Debug Configurations' dialog > Edit Configurations... > Edit configuration templates...
<br/>- Select <b>TestNG</b> > Listeners > and add these listeners one by one:
<br/>`com.shaft.tools.listeners.AlterSuiteListener`, `com.shaft.tools.listeners.SuiteListener`, `com.shaft.tools.listeners.InvokedMethodListener`
<br/>- Select <b>Cucumber Java</b> > Program Arguments > and add this argument:
<br/>`--plugin com.shaft.tools.listeners.CucumberFeatureListener`
<br/>- After saving the changes, remember to delete any old test runs you may have triggered by mistake before adding the needed config.
[^2]: The User Guide is still a work in progress. You can find a guide to contribute to it here [SHAFT_Engine_Docusaurus](https://github.com/ShaftHQ/SHAFT_Engine_Docusaurus#readme), and feel free to [Join us via Slack & Facebook](#support-and-contributions) for support and contributions.
[^3]: The Test Automation Hero++ Playlist is in Arabic. The working code itself is in English, and it's hosted here [using_SHAFT_Engine_2](https://github.com/MohabMohie/using_SHAFT_Engine_2). Feel free to [Join us via Slack & Facebook](#support-and-contributions) for support and contributions.


## What? Why? When? How? and Who? 🤔


### What is SHAFT?
- A Unified Test Automation Engine.<br/><img src="src/main/resources/images/engine.png" alt="Generic Test Automation Architecture" style="display:block; margin-left:auto; margin-right:auto;"/>
- A source controlled Java Maven project that is easily extended and regularly enhanced with new features.
- Provides an easily understandable and user-friendly syntax for writing simple, robust, reliable, maintainable, and scalable tests.
- Provides a ton of built-in features in the Test Execution and Test Adaptation layers of the Generic Test Automation Architecture, and allows you to focus on the Test Generation and Test Definition Layers.<br/><img src="src/main/resources/images/mindmap.png" alt="SHAFT_ENGINE MindMap" style="display:block; margin-left:auto; margin-right:auto;"/>

### Why should I use SHAFT?
- MIT Licensed Open-Source project that's free to use and easy to customize.
- Frequent updates full of new features and bug fixes.
- Helps you focus on writing simple tests without wasting time on any of the underlying boilerplate code and error handling.
- Maximize your Return On Investment by eliminating framework creation time, tool selection and training costs, and minimizing ramp up time.<br/><img src="src/main/resources/images/roi.png" alt="Return On Investment Analysis" style="display:block; margin-left:auto; margin-right:auto;"/>
- Start being productive immediately!

### When should I use SHAFT?
- If you're getting started with a new test automation project that focuses on any of the supported platforms.
- If you already have an existing test automation project that uses Selenium/Appium/RestAssured/TestNG/Java then using SHAFT will be a direct upgrade with <u>ZERO refactoring required</u>.

### How can I use SHAFT?
- Start by following the above [Quick Start Guide](#quick-start-guide)
- You can also watch this 10-minute video ▶️ [YouTube - How to do cross-browser test automation in 10 minutes (from scratch)](https://www.youtube.com/watch?v=3TYGteD843M)

### Who is using SHAFT? [^4]
<img height="50" title="_VOIS (Vodafone Intelligent Solution)" alt="_VOIS (Vodafone Intelligent Solution)" src="https://www.vodafone.com/_next/image?url=https%3A%2F%2Fcontent.vodafone.com%2Fsites%2Fdefault%2Ffiles%2Finline-images%2FgN08grNr8s9vipkhltm4sWWezExdQg5LwJrGY2Ma2ojTjCnvi2.png&w=1600&q=100" href="https://www.vodafone.com/careers/professional-career-areas/shared-services">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="GET Group Holdings" alt="GET Group Holdings" src="https://www.getgroup.com/wp-content/themes/get-group-holdings/assets/images/logo-high-res-2.png" href="https://www.getgroup.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="MOMRA (Saudi Arabia's Ministry of Municipal and Rural Affairs)" alt="MOMRA (Saudi Arabia's Ministry of Municipal and Rural Affairs)" src="https://momrah.gov.sa/themes/custom/momrah/assets/images/mh-logo-full.png" href="https://momra.gov.sa/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Vodafone (Egypt)" alt="Vodafone (Egypt)" src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Vodafone_icon.svg/239px-Vodafone_icon.svg.png" href="https://www.vodafone.com.eg">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Solutions by STC" alt="Solutions by STC" src="https://solutions.com.sa/wp-content/uploads/2019/11/logo.svg" href="https://solutions.com.sa/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="GIZA Systems" alt="GIZA Systems" src="https://gizasystems.com/wp-content/themes/twentyfourteen-child/images/logo.png" href="https://gizasystems.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Euronet" alt="Euronet" src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Euronet_Worldwide_logo.svg/1920px-Euronet_Worldwide_logo.svg.png" href="https://www.euronetworldwide.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Terkwaz Business Solutions" alt="Terkwaz Business Solutions" src="https://images.wuzzuf-data.net/files/company_logo/Terkwaz-Solutions-Jordan-35434-1578830823.png" href="https://www.terkwaz.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Incorta" alt="Incorta" src="https://media-exp1.licdn.com/dms/image/C560BAQHUWHhKl0xrCA/company-logo_200_200/0/1660913597037?e=2147483647&v=beta&t=CiDPUEvlIBqztN5gCre-pQ5f7M-03_02IQgJtL18wG8" href="https://www.incorta.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="BayanTech" alt="BayanTech" src="https://bayan-tech.com/wp-content/uploads/2020/01/Bayan-Logo-2.png" href="https://bayan-tech.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Adam.ai" alt="Adam.ai" src="https://images.prismic.io/adamdotai/8e6625b0-e32b-4bee-b1b8-ebdc1d30cbfe_full-logo.svg?ixlib=gatsbyFP&auto=compress%2Cformat&fit=max&q=50" href="https://adam.ai/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="ACT Global Soft" alt="ACT Global Soft" src="https://www.act.eg/wp-content/uploads/2021/01/3.png" href="https://www.act.eg/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="elmenus" alt="elmenus" src="https://assets-global.website-files.com/625d19c77d49d0aa53047be3/625d19c77d49d0142c047d36_615c77e71175c38163bca403_elmenus.png" href="https://www.elmenus.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="IDEMIA" alt="IDEMIA" src="https://wikiimg.tojsiabtv.com/wikipedia/commons/thumb/2/2e/IDEMIA_Logo.jpg/1280px-IDEMIA_Logo.jpg" href="https://www.idemia.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="iHorizons" alt="iHorizons" src="https://www.ihorizons.com/sites/all/themes/ihorizons_theme/imgs/logo.png" href="https://www.ihorizons.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Robusta" alt="Robusta" src="https://images.wuzzuf-data.net/files/company_logo/Robusta-Egypt-7927.png" href="https://robustastudio.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Paymob Solutions" alt="Paymob Solutions" src="https://www.paymob.com/images/paymobLogo.png" href="https://www.paymob.com/">

[^4]: Company names are collected via anonymous surveys and provided freely by engineers who claimed to be using SHAFT_Engine within these companies.

<a id="support-and-contributions"></a>
## For Support & Contributions 👥
- Join us via Slack & Facebook
<br/><a href="https://join.slack.com/t/automatest-workspace/shared_invite/zt-oii5i2gg-0ZGnih_Y34NjK7QqDn01Dw" target="_blank"><img src="https://a.slack-edge.com/80588/marketing/img/icons/icon_slack_hash_colored.png" alt="automatest-workspace" width="50" height="50"/></a>  <a href="https://www.facebook.com/groups/Automatest" target="_blank"><img src="https://facebookbrand.com/wp-content/uploads/2019/04/f_logo_RGB-Hex-Blue_512.png" alt="Automatest" width="50" height="50"/></a>
- And feel free to create PRs directly. [This lovely tutorial](https://dev.to/genicsblog/how-to-create-a-pull-request-in-github-correctly-20np) will help.

#### Stop Reinventing the wheel! Start using SHAFT!
