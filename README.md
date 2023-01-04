<img src="src/main/resources/images/shaft.png" alt="SHAFT_ENGINE" style="display:block; margin-left:auto; margin-right:auto;"/>

<!-- Badges provided by https://shields.io/ -->
[![License](https://img.shields.io/github/license/ShaftHQ/SHAFT_Engine?color=blue&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/blob/master/LICENSE)
 [![Contributors)](https://img.shields.io/github/contributors/ShaftHQ/SHAFT_ENGINE?color=blue&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/graphs/contributors)
 
 [![JFrog release (latest by date including pre-releases)](https://img.shields.io/github/v/release/ShaftHQ/shaft_engine?include_prereleases&color=terkwaz&label=Latest%20Release&style=for-the-badge)](https://automatest.jfrog.io/ui/native/SHAFT_ENGINE/io/github/ShaftHQ/SHAFT_ENGINE/)
 [![Codacy grade](https://img.shields.io/codacy/grade/de526d8e36ad4021a2f8f5ed553e4976?style=for-the-badge&label=Codacy%20Code%20Quality)](https://app.codacy.com/gh/ShaftHQ/SHAFT_ENGINE/dashboard)
 [![GitHub Workflow Status (with branch)](https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/codeql-analysis.yml?branch=master&label=CodeQL%20Security%20Scans&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/codeql-analysis.yml)

[![GitHub Workflow Status (with branch)](https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/test.yml?branch=master&color=purple&label=Desktop%20Tests&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/test.yml)
 [![GitHub Workflow Status (with branch)](https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/test_mobile.yml?branch=master&color=purple&label=Mobile%20Tests&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/test_mobile.yml)
 [![GitHub Workflow Status (with branch)](https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/jfrog_cd.yml?branch=master&color=purple&label=Continuous%20Releases&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/jfrog_cd.yml)


# SHAFT: Unified Test Automation Engine

 
<a id="quick-start-guide"></a>
## Quick Start Guide 🏃

### Option 1: Template Project
- Simply use this [Template Project](https://github.com/MohabMohie/using_SHAFT_Engine) to get started with one click.

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

    @AfterClass
    public void afterClass() {
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
- Make sure to <b>Star</b> ⭐ the project to get notified when a new release is pushed out.
- After upgrading your Engine it is sometimes recommended to delete the properties
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


## Important Links
- You can change the target browser, operating system, timeouts, and other configurations using the ⚙️ [Configuration Manager](https://ShaftHQ.github.io/SHAFT_ENGINE/).
- And you can learn more from the 👤 [User Guide](https://ShaftHQ.github.io/SHAFT_Engine_Docusaurus/) and 📚 [Javadocs](https://ShaftHQ.github.io/SHAFT_ENGINE/apidocs/index.html).


## Who else is using SHAFT? [^4]
<img height="50" title="_VOIS (Vodafone Intelligent Solution)" alt="_VOIS (Vodafone Intelligent Solution)" src="https://www.vodafone.com/_next/image?url=https%3A%2F%2Fcontent.vodafone.com%2Fsites%2Fdefault%2Ffiles%2Finline-images%2FgN08grNr8s9vipkhltm4sWWezExdQg5LwJrGY2Ma2ojTjCnvi2.png&w=1600&q=100" href="https://www.vodafone.com/careers/professional-career-areas/shared-services">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="GET Group Holdings" alt="GET Group Holdings" src="https://www.getgroup.com/wp-content/themes/get-group-holdings/assets/images/logo-high-res-2.png" href="https://www.getgroup.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="MOMRA (Saudi Arabia's Ministry of Municipal and Rural Affairs)" alt="MOMRA (Saudi Arabia's Ministry of Municipal and Rural Affairs)" src="https://momrah.gov.sa/themes/custom/momrah/assets/images/mh-logo-full.png" href="https://momra.gov.sa/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Vodafone (Egypt)" alt="Vodafone (Egypt)" src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Vodafone_icon.svg/239px-Vodafone_icon.svg.png" href="https://www.vodafone.com.eg">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Solutions by STC" alt="Solutions by STC" src="https://solutions.com.sa/wp-content/uploads/2019/11/logo.svg" href="https://solutions.com.sa/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="GIZA Systems" alt="GIZA Systems" src="https://gizasystems.com/wp-content/themes/twentyfourteen-child/images/logo.png" href="https://gizasystems.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Euronet" alt="Euronet" src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Euronet_Worldwide_logo.svg/1920px-Euronet_Worldwide_logo.svg.png" href="https://www.euronetworldwide.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Terkwaz Business Solutions" alt="Terkwaz Business Solutions" src="https://images.wuzzuf-data.net/files/company_logo/Terkwaz-Solutions-Jordan-35434-1578830823.png" href="https://www.terkwaz.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Incorta" alt="Incorta" src="https://media-exp1.licdn.com/dms/image/C560BAQHUWHhKl0xrCA/company-logo_200_200/0/1660913597037?e=2147483647&v=beta&t=CiDPUEvlIBqztN5gCre-pQ5f7M-03_02IQgJtL18wG8" href="https://www.incorta.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="BayanTech" alt="BayanTech" src="https://bayan-tech.com/wp-content/uploads/2020/01/Bayan-Logo-2.png" href="https://bayan-tech.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Adam.ai" alt="Adam.ai" src="https://images.prismic.io/adamdotai/8e6625b0-e32b-4bee-b1b8-ebdc1d30cbfe_full-logo.svg?ixlib=gatsbyFP&auto=compress%2Cformat&fit=max&q=50" href="https://adam.ai/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="ACT Global Soft" alt="ACT Global Soft" src="https://www.act.eg/wp-content/uploads/2021/01/3.png" href="https://www.act.eg/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="elmenus" alt="elmenus" src="https://assets-global.website-files.com/625d19c77d49d0aa53047be3/625d19c77d49d0142c047d36_615c77e71175c38163bca403_elmenus.png" href="https://www.elmenus.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="IDEMIA" alt="IDEMIA" src="https://wikiimg.tojsiabtv.com/wikipedia/commons/thumb/2/2e/IDEMIA_Logo.jpg/1280px-IDEMIA_Logo.jpg" href="https://www.idemia.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="iHorizons" alt="iHorizons" src="https://www.ihorizons.com/sites/all/themes/ihorizons_theme/imgs/logo.png" href="https://www.ihorizons.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Robusta" alt="Robusta" src="https://images.wuzzuf-data.net/files/company_logo/Robusta-Egypt-7927.png" href="https://robustastudio.com/">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img height="50" title="Paymob Solutions" alt="Paymob Solutions" src="https://www.paymob.com/images/paymobLogo.png" href="https://www.paymob.com/">

[^4]: Company names are collected via anonymous surveys and provided freely by engineers who claimed to be using SHAFT_Engine within these companies.


<a id="support-and-contributions"></a>
## For Support & Contributions 👥
- Join us via Slack & Facebook
<br/><a href="https://join.slack.com/t/automatest-workspace/shared_invite/zt-oii5i2gg-0ZGnih_Y34NjK7QqDn01Dw" target="_blank"><img src="https://a.slack-edge.com/80588/marketing/img/icons/icon_slack_hash_colored.png" alt="automatest-workspace" width="50" height="50"/></a>  <a href="https://www.facebook.com/groups/Automatest" target="_blank"><img src="https://facebookbrand.com/wp-content/uploads/2019/04/f_logo_RGB-Hex-Blue_512.png" alt="Automatest" width="50" height="50"/></a>
- And feel free to create PRs directly. [This lovely tutorial](https://dev.to/genicsblog/how-to-create-a-pull-request-in-github-correctly-20np) will help.


## Big thanks to the following organizations for their support to the project with their open source licenses:
<a href="https://www.browserstack.com" rel="nofollow"><img src="https://camo.githubusercontent.com/881484a291da777d1e832c6534b7ff8a0c0f79832d558e083f41b79067f0c7cf/68747470733a2f2f6d616464796e6573732d756b2e747769632e706963732f323032312f30362f53637265656e73686f742d323032312d30362d32312d61742d32302e31342e34362e706e673f747769633d76312f726573697a653d363330" alt="ATD" width="45%" align="top" data-canonical-src="https://maddyness-uk.twic.pics/2021/06/Screenshot-2021-06-21-at-20.14.46.png?twic=v1/resize=630" style="max-width: 100%;"></a>


#### Stop Reinventing the wheel! Start using SHAFT!
