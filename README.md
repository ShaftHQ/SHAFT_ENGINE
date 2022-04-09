<img src="src/main/resources/images/shaft.png" alt="SHAFT_ENGINE" style="display:block; margin-left:auto; margin-right:auto;"/>

<!-- Badges provided by https://shields.io/ -->
![License](https://img.shields.io/github/license/MohabMohie/SHAFT_Engine?color=blue&style=for-the-badge)
 [![JFrog release (latest by date including pre-releases)](https://img.shields.io/github/v/release/MohabMohie/shaft_engine?include_prereleases&label=Latest%20Release&style=for-the-badge)](https://automatest.jfrog.io/ui/native/SHAFT_ENGINE/io/github/mohabmohie/SHAFT_ENGINE/)
  [![Maven Central](https://img.shields.io/maven-central/v/io.github.mohabmohie/SHAFT_ENGINE?style=for-the-badge)](https://search.maven.org/artifact/io.github.mohabmohie/SHAFT_ENGINE)

[![Contributors)](https://img.shields.io/github/contributors/MohabMohie/SHAFT_ENGINE?style=for-the-badge)](https://github.com/MohabMohie/SHAFT_ENGINE/graphs/contributors)
 [![Codacy grade](https://img.shields.io/codacy/grade/3579cfd02a2c4f67bd1dce5dad0b1562?style=for-the-badge)](https://app.codacy.com/gh/MohabMohie/SHAFT_ENGINE/dashboard)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/MohabMohie/SHAFT_Engine/CodeQL?label=CodeQL&style=for-the-badge)](https://github.com/MohabMohie/SHAFT_ENGINE/actions?query=workflow%3ACodeQL)
 [![GitHub Workflow Status](https://img.shields.io/github/workflow/status/MohabMohie/SHAFT_ENGINE/Desktop%20Browsers%20Tests/master?label=Desktop%20Browsers%20Tests&style=for-the-badge)](https://github.com/MohabMohie/SHAFT_ENGINE/actions?query=workflow%3A%22Ubuntu+-+Test%22)


# SHAFT: Unified Test Automation Engine

 
<a id="quick-start-guide"></a>
## Quick Start Guide 🏃
- Create a new Java/Maven project using Eclipse, IntelliJ[^1] or your favourite IDE.
- Copy the highlighted contents of this [pom.xml](https://github.com/MohabMohie/using_SHAFT_ENGINE/blob/7bfc918b00dfd2bd674c349a07bcec3fa98913a6/pom.xml#L12-L79) file into yours inside the ```<project>``` tag.
- Create the following file ```src/test/resources/testDataFiles/simpleJSON.json```.
- Copy the below code snippet into your newly created json file.
```json
{
  "searchQuery": "SHAFT_Engine"
}
```
- Create a new Package under ```src/test/java``` and create a new Java Class under that package.
- Copy the below code snippet into your newly created java class.
```java
public class Test_Wizard_GUI {
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
}
```
- Run it as a TestNG Test Class.
- The execution report will open automatically in your default web browser after the test run is completed.
- You can change the target browser, operating system, timeouts, and other configurations using the ⚙️ [Configuration Manager](https://mohabmohie.github.io/SHAFT_ENGINE/).
- [Click Here](https://github.com/MohabMohie/SHAFT_ENGINE/tree/master/src/test/java/testPackage01/SHAFTWizard) for more GUI, API, DB, and CLI sample test classes.
- And you can learn more from the 📚 [Javadocs](https://mohabmohie.github.io/SHAFT_ENGINE/apidocs/index.html) and 👤 [User Guide](https://mohabmohie.github.io/SHAFT_Engine_Docusaurus/) [^2].
- Here is also a complete tutorial showing everything from creating the project to running remote, unattended, parallelized, cross-platform tests ▶️ [Youtube: Test Automation Hero++](https://www.youtube.com/playlist?list=PLlnkmUosVw9g1IK6M4kZS8a-EsP4xb0Vf) [^3].
- Lastly, feel free to [Join us via Slack & Facebook](#support-and-contributions) for support and contributions.
[^1]: Due to a known issue with IntelliJ you need to edit your run configuration templates before running your tests by following these steps:
<br/>- Open 'Edit Run/Debug Configurations' dialog > Edit Configurations... > Edit configuration templates...
<br/>- Select <b>TestNG</b> > Listeners > and add these listeners one by one:
<br/>`com.shaft.tools.listeners.AlterSuiteListener`, `com.shaft.tools.listeners.SuiteListener`, `com.shaft.tools.listeners.InvokedMethodListener`
<br/>- Select <b>Cucumber Java</b> > Program Arguments > and add this argument:
<br/>`--plugin com.shaft.tools.listeners.CucumberFeatureListener`
<br/>- After saving the changes, remember to delete any old test runs you may have triggered by mistake before adding the needed config.
[^2]: The User Guide is still a work in progress. You can find a guide to contribute to it here [SHAFT_Engine_Docusaurus](https://github.com/MohabMohie/SHAFT_Engine_Docusaurus#readme), and feel free to [Join us via Slack & Facebook](#support-and-contributions) for support and contributions.
[^3]: The Test Automation Hero++ Playlist is in Arabic. The working code itself is in English, and it's hosted here [using_SHAFT_Engine_2](https://github.com/MohabMohie/using_SHAFT_Engine_2). Feel free to [Join us via Slack & Facebook](#support-and-contributions) for support and contributions.


## What? Why? When? and How? 🤔


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


<a id="support-and-contributions"></a>
## For Support & Contributions 👥
- Join us via Slack & Facebook
<br/><a href="https://join.slack.com/t/automatest-workspace/shared_invite/zt-oii5i2gg-0ZGnih_Y34NjK7QqDn01Dw" target="_blank"><img src="https://a.slack-edge.com/80588/marketing/img/icons/icon_slack_hash_colored.png" alt="automatest-workspace" width="50" height="50"/></a>  <a href="https://www.facebook.com/groups/Automatest" target="_blank"><img src="https://facebookbrand.com/wp-content/uploads/2019/04/f_logo_RGB-Hex-Blue_512.png" alt="Automatest" width="50" height="50"/></a>
- And feel free to create PRs directly. [This lovely tutorial](https://dev.to/genicsblog/how-to-create-a-pull-request-in-github-correctly-20np) will help.

#### Stop Reinventing the wheel! Start using SHAFT!
