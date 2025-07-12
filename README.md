![image](https://github.com/user-attachments/assets/7e9276b5-bc11-4f9d-9bc1-545c5116a8ab)<!-- Badges provided by https://shields.io/ -->

<h1>
<picture>
  <!-- User prefers light mode: -->
  <source srcset="https://github.com/user-attachments/assets/b2e8454d-97ed-4dd8-91f2-1c09c53ba94e" media="(prefers-color-scheme: light)" width="40"/>

  <!-- User prefers dark mode: -->
  <source srcset="https://github.com/user-attachments/assets/9cb4a7a8-2de7-486c-adb1-ad254af8c58b"  media="(prefers-color-scheme: dark)" width="40"/>

  <!-- User has no color preference: -->
  <img src="https://github.com/user-attachments/assets/016ebb3c-4090-4f07-a9b3-830fdf4cb696"/>
</picture> SHAFT : Unified Test Automation Engine
</h1>
 <br/><br/>

<table border="0" align="center">
 <tr>
  <td align="center" valign="top">
   Fully documented
<br/><br/><a href="https://ShaftHQ.github.io/"><img width="300" alt="User Guide" src="https://github.com/ShaftHQ/SHAFT_ENGINE/assets/19201898/bdd6db98-4121-4a86-b7db-fb94b8830d11"></a>
  <br/></td>
   <td align="center" valign="top">
   Award Winning
<br/><br/><a href="https://opensource.googleblog.com/2023/05/google-open-source-peer-bonus-program-announces-first-group-of-winners-2023.html"><img width="250" alt="Google Open Source" src="https://github.com/user-attachments/assets/a21a9e31-c63d-4712-bd9d-2bd131e5173c"/></a>
  <br/></td>
  <td align="center" valign="top">
   Selenium Ecosystem
<br/><br/><a href="https://www.selenium.dev/ecosystem/#frameworks"><img width="300" alt="Selenium Ecosystem" src="https://github.com/ShaftHQ/SHAFT_ENGINE/assets/19201898/b13d4c2c-72ce-4de6-861f-d143f905c5ab"></a>
  <br/></td>
 </tr>
  <tr>
  <td colspan="3" align="center">
   <br/>
   <a href="https://techforpalestine.org/learn-more" target="_blank"><img alt="Tech for Palestine" src="https://img.shields.io/badge/%F0%9F%87%B5%F0%9F%87%B8_Ceasefire_Now-techforpalestine.org-000000?style=for-the-badge&label=Ceasefire%20Now&color=D83838"></a>
   <br/>
   <a href="https://github.com/ShaftHQ/SHAFT_ENGINE/blob/master/LICENSE" target="_blank"><img alt="License" src="https://img.shields.io/github/license/ShaftHQ/SHAFT_Engine?color=indigo&style=for-the-badge"></a>
   <a href="https://github.com/ShaftHQ/SHAFT_ENGINE/graphs/contributors" target="_blank"><img alt="Contributors" src="https://img.shields.io/github/contributors/ShaftHQ/SHAFT_ENGINE?color=indigo&style=for-the-badge"></a>
   <a href="https://central.sonatype.com/artifact/io.github.shafthq/SHAFT_ENGINE" target="_blank"><img alt="Latest Release" src="https://img.shields.io/github/v/release/ShaftHQ/shaft_engine?include_prereleases&color=indigo&label=Latest%20Release&style=for-the-badge"></a>
   <br/>
   <a href="https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/e2eTests.yml" target="_blank"><img alt="E2E Tests" src="https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/e2eTests.yml?branch=main&color=forestgreen&label=E2E%20Tests&style=for-the-badge"></a>
   <a href="https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/codeql-analysis.yml" target="_blank"><img alt="Code QL" src="https://img.shields.io/github/actions/workflow/status/SHAFTHQ/SHAFT_Engine/codeql-analysis.yml?branch=main&label=Security&color=forestgreen&style=for-the-badge"></a>
   <br/>
   <a href="https://www.codacy.com/gh/ShaftHQ/SHAFT_ENGINE/dashboard" target="_blank"><img alt="Codacy" src="https://img.shields.io/codacy/grade/4d6d48aba396411fa3170184330ba089?style=for-the-badge&color=blue&label=Code%20Quality"></a>
   <a href="https://app.codecov.io/gh/ShaftHQ/SHAFT_ENGINE" target="_blank"><img alt="Codecov" src="https://img.shields.io/codecov/c/github/shafthq/shaft_engine?style=for-the-badge&label=Coverage&color=blue"></a>
   <br/><br/>
  </td>
 </tr>
</table>
<br/><br/>

<a id="table-of-contents"></a>

## 📜 Table of Contents

<table border="0" align="center">
 <tr>
  <td valign="top">

- [🏃 Quick start guide](#quick-start-guide)
- [📚 User guide ➡️](https://shafthq.github.io/)
- [🌍 Our success partners](#our-success-partners)
- [🚀 Features](#features)
- [👨‍💻 Tech stack](#tech-stack)
- [🤝 Support & contributions](#support-and-contributions)
- [📜 MIT license ➡️](LICENSE)

</td>
</tr></table>
<br/><br/>

<a id="quick-start-guide"></a>

## 🏃 Quick start guide [⤴](#-table-of-contents)
### Option 1: Maven archetype

> [!TIP]
> Recommended for new local sandbox projects.

- The easiest and most straightforward way to create a new project that uses SHAFT.
- Just [follow the simple steps here ➡️](https://shafthq.github.io/docs/Getting_Started/first_steps_5) to generate your new project with one command (all configurations included).

### Option 2: Start from scratch

> [!TIP]
> Recommended if you're upgrading an existing project from Native Selenium WebDriver to SHAFT.

#### Step 1: Initial setup

- Create a new Java/Maven project using the latest version from IntelliJ IDEA, Eclipse or your favourite IDE.
- Copy the highlighted contents of
  this [pom.xml](https://github.com/ShaftHQ/using_SHAFT_Engine/blob/main/GUI_Web/pom.xml#L11-L156) file into yours
  inside the ```<project>``` tag.

#### Step 2: Creating tests

##### 2.1. TestNG

- Create a new Package ```testPackage``` under ```src/test/java```
- Create a new Java class ```TestClass``` under your newly created `testPackage`.
- Copy the below imports into your newly created `TestClass` after the line that contains `package testPackage`.

```java
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
```

- Copy the below code snippet into the body of your `TestClass` after the line that contains `public class TestClass {`.

```java
SHAFT.GUI.WebDriver driver;
SHAFT.TestData.JSON testData;

String targetUrl = "https://duckduckgo.com/";

By logo = By.xpath("//div[contains(@class,'container_fullWidth__1H_L8')]//img");
By searchBox = Locator.hasAnyTagName().hasAttribute("name", "q").build(); // synonym to By.name("q");
By firstSearchResult = Locator.hasTagName("article").isFirst().build(); // synonym to By.xpath("(//article)[1]");

@Test
public void navigateToDuckDuckGoAndAssertBrowserTitleIsDisplayedCorrectly() {
  driver.browser().navigateToURL(targetUrl)
          .and().assertThat().title().contains(testData.getTestData("expectedTitle"));
}

@Test
public void navigateToDuckDuckGoAndAssertLogoIsDisplayedCorrectly() {
  driver.browser().navigateToURL(targetUrl)
          .and().element().assertThat(logo).matchesReferenceImage();
}

@Test
public void searchForQueryAndAssert() {
  driver.browser().navigateToURL(targetUrl)
          .and().element().type(searchBox, testData.getTestData("searchQuery") + Keys.ENTER)
          .and().assertThat(firstSearchResult).text().doesNotEqual(testData.getTestData("unexpectedInFirstResult"));
}

@BeforeClass
public void beforeClass() {
  testData = new SHAFT.TestData.JSON("simpleJSON.json");
}

@BeforeMethod
public void beforeMethod() {
  driver = new SHAFT.GUI.WebDriver();
}

@AfterMethod
public void afterMethod(){
  driver.quit();
}
```

##### 2.2. JUnit5

```
--TODO--
```

##### 2.3. Cucumber

```
--TODO--
```

> [!TIP]
> In case you are planning to use Cucumber with IntelliJ IDEA, due to a known issue with IntelliJ you need to edit your run configuration template before running your tests by following these steps:
> <br/>- Open 'Edit Run/Debug Configurations' dialog > Edit Configurations... > Edit configuration templates...
> <br/>- Select <b>Cucumber Java</b> > Program Arguments > and add this argument:
> <br/>`--plugin com.shaft.listeners.CucumberFeatureListener`
> <br/>- After saving the changes, remember to delete any old runs you may have triggered by mistake before adding the needed config.


#### Step 3: Managing test data
- Create the following file ```src/test/resources/testDataFiles/simpleJSON.json```.
- Copy the below code snippet into your newly created json file.
```json
{
  "searchQuery": "SHAFT_Engine",
  "expectedTitle": "DuckDuckGo",
  "unexpectedInFirstResult": "Nope"
}
```

#### Step 4: Running tests
- Run your ```TestClass.java``` either from the side menu or by pressing the run button.
- On the first test run: 
  - SHAFT will create a new folder ```src/main/resources/properties``` and generate some default properties files.
  - SHAFT will run in `minimalistic test run` mode and will self-configure its listeners under the `src/test/resources/META-INF/services` directory.
> [!NOTE]
> In case you got the following error message trying to execute your first run![image](https://github.com/user-attachments/assets/6b894234-e365-4fdd-a1d2-abd06ead7e98)
And you didn't get the option ```Shorten the command line and rerun```.
  - From Intellij IDEA main menu, go to Help/Edit Custom VM Options
  - Add the following line and click save ```-Didea.dynamic.classpath=true```
  - Restart IntelliJ to apply the changes

> [!TIP]
> You can visit the [user guide ➡️](https://shafthq.github.io/docs/Properties/PropertiesList) to learn how to configure all SHAFT's properties.
- On all following test runs:
  - After the run is complete, the Allure execution report will open automatically in your default web browser.
- <b>Join</b> our ![GitHub Repo stars](https://img.shields.io/github/stars/shafthq/shaft_engine?logoColor=black&style=social) to get notified by email when a new release is pushed out.
> [!NOTE]
> After upgrading your Engine to a new major release it is sometimes recommended to delete the properties
folder ```src\main\resources\properties``` and allow SHAFT to regenerate the defaults by running any test method.

<br/><br/>

<a id="user-guide"></a>

## 📚 [User guide ➡️](https://shafthq.github.io/)
- Check out our comprehensive and friendly [user guide](https://shafthq.github.io/) to learn why <b>SHAFT</b> should be your solution of choice if you're aiming for successful test automation.

<br/><br/>

<a id="our-success-partners"></a>

## 🌍 Our success partners [⤴](#-table-of-contents)

### Sponsors:

<table border="0" align="center">
 <tr>
  <td align="center">
   <br/>
<a href="https://www.browserstack.com/" target="_blank"><img src="https://ml.globenewswire.com/Resource/Download/745e80b7-4736-424e-b44b-850d2dc41940" alt="BrowserStack" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://applitools.com/" target="_blank"><img src="https://www.selenium.dev/images/sponsors/applitools.png" alt="Applitools" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://jb.gg/OpenSourceSupport" target="_blank"><img src="https://resources.jetbrains.com/storage/products/company/brand/logos/jetbrains.svg" alt="JetBrains" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.lambdatest.com" target="_blank"><img src="https://www.lambdatest.com/blog/wp-content/uploads/2024/10/LambdaTest-Logo.png" alt="LambdaTest" width="250px" height="50px"></a>
<br/><br/>
  </td></tr></table>

### Trusted solution of choice for: [^4]

<table border="0" align="center">
 <tr>
  <td align="center">
   <br/>
<a href="https://www.vodafone.com/careers/professional-career-areas/shared-services"><img height="50" alt="_VOIS (Vodafone Intelligent Solution)" src="https://www.vodafone.com/_ipx/w_768,q_75/https%3A%2F%2Fimages.ctfassets.net%2Fq7ob9vms4z5k%2F2u767jo6qLM730dcOVs1lN%2F110f5535a8a0505e3b0aef0934a4a07c%2FVOIS.png?url=https%3A%2F%2Fimages.ctfassets.net%2Fq7ob9vms4z5k%2F2u767jo6qLM730dcOVs1lN%2F110f5535a8a0505e3b0aef0934a4a07c%2FVOIS.png&w=768&q=75"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.getgroup.com/"><img height="50" alt="GET Group Holdings" src="https://media.licdn.com/dms/image/C510BAQFS-mP8SeyOyg/company-logo_200_200/0/1630600086112/get_group_logo?e=2147483647&v=beta&t=mhfpG9gfW0JC4afaWYeHWWrA5AgMOv6g9bP3FnsN40o"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.momah.gov.sa/en"><img height="50" alt="MOMRA (Saudi Arabia's Ministry of Municipal and Rural Affairs)" src="https://momah.gov.sa/themes/custom/momrah/assets/images/mh-logo-full.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.vodafone.com.eg"><img height="50" alt="Vodafone Egypt" src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/95/Vodafone_logo_2017.png/1200px-Vodafone_logo_2017.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://solutions.com.sa/"><img height="50" alt="Solutions by STC" src="https://solutions.com.sa/wp-content/uploads/2019/11/logo.svg"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://gizasystems.com/"><img height="50" alt="GIZA Systems" src="https://github.com/user-attachments/assets/45e7bc17-b3f3-44c7-a053-2f250ea497b6"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.euronetworldwide.com/"><img height="50" alt="Euronet" src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Euronet_Worldwide_logo.svg/1920px-Euronet_Worldwide_logo.svg.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.terkwaz.com/"><img height="50" alt="Terkwaz Business Solutions" src="https://images.wuzzuf-data.net/files/company_logo/Terkwaz-Solutions-Jordan-35434-1578830823.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.incorta.com/"><img height="50" alt="Incorta" src="https://media-exp1.licdn.com/dms/image/C560BAQHUWHhKl0xrCA/company-logo_200_200/0/1660913597037?e=2147483647&v=beta&t=CiDPUEvlIBqztN5gCre-pQ5f7M-03_02IQgJtL18wG8"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://bayan-tech.com/"><img height="50" alt="BayanTech" src="https://bayan-tech.com/wp-content/uploads/2020/01/Bayan-Logo-2.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://adam.ai/"><img height="50" alt="Adam.ai" src="https://images.prismic.io/adamdotai/8e6625b0-e32b-4bee-b1b8-ebdc1d30cbfe_full-logo.svg?ixlib=gatsbyFP&auto=compress%2Cformat&fit=max&q=50"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.act.eg/"><img height="50" alt="ACT Global Soft" src="https://www.act.eg/wp-content/uploads/2021/01/3.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.elmenus.com/"><img height="50" alt="elmenus" src="https://assets-global.website-files.com/625d19c77d49d0aa53047be3/625d19c77d49d0142c047d36_615c77e71175c38163bca403_elmenus.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.idemia.com/"><img height="50" alt="IDEMIA" src="https://github.com/user-attachments/assets/f20a6eab-a7f9-4f73-b27f-0e808c446fe9"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.ihorizons.com/"><img height="50" alt="iHorizons" src="https://www.ihorizons.com/sites/all/themes/ihorizons_theme/imgs/logo.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://robustastudio.com/"><img height="50" alt="Robusta" src="https://images.wuzzuf-data.net/files/company_logo/Robusta-Egypt-7927.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.paymob.com/"><img height="50" alt="Paymob Solutions" src="https://www.paymob.com/images/paymobLogo.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://jahezgroup.com/"><img height="50" alt="Jahez Group" src="https://github.com/user-attachments/assets/3c9f6bb3-d310-48d7-a852-47ff5537e629"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://salt.bank/"><img height="50" alt="Salt Bank" src="https://static.wikia.nocookie.net/logopedia/images/3/34/Salt_Bank.svg"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.baianat.com/"><img height="50" alt="Baianat" src="https://mir-s3-cdn-cf.behance.net/user/276/f5dc271705011.5b8c47fcee858.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://dxc.com/us/en"><img height="50" alt="DXC Technology" src="https://github.com/user-attachments/assets/84cb59da-d29d-44fa-9012-b10d2cc671ff"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.efghldg.com/en"><img height="50" alt="EFG Holding" src="https://github.com/user-attachments/assets/188f24c2-9e3c-4bcc-a2d6-40c5ab58d5e3"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href=""><img height="50" alt="" src=""></a>

  <br/><br/>
  </td></tr></table>

[^4]: Company names are collected via anonymous surveys and provided freely by engineers who claimed to be using SHAFT_Engine within these companies.

<br/><br/>
<a id="features"></a>

## 🚀 Features [⤴](#-table-of-contents)

### Smart features

- SHAFT provides a lot of out-of-the-box convenience features to facilitate your testing process and eliminate the need for boilerplate code.
- All of SHAFT's smart features target the three pillars of successful test automation:
  - **Scalability**: The ability to run tests on multiple devices and browsers in parallel.
  - **Reliability**: The ability to run tests without flakiness and with detailed reporting.
  - **Maintainability**: The ability to easily maintain and update tests as the application changes.

#### Scalability:

| CI/CD integration |Cloud device farm integration  |Headless testing  |Parallel execution  |Containerized execution  |
| :---: |:---: |:---: |:---: |:---: |
| :white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |

#### Reliability:

| Automated synchronization | Logging | Reporting | Screenshots/Attachments| Video recording|
|:-------------------------:|:---: |:---: |:---: |:---: |
|    :white_check_mark:     |:white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |

#### Maintainability:

| Fluent design | Locator builder |   Smart locators   | Native `WebDriver` access | Element/Browser validations builder| AI-powered visual validations
| :---: |:---: |:------------------:|:---: |:---: |:---: |
| :white_check_mark: |:white_check_mark: | :white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |


<br/>

### Supported platforms

#### Browsers:

|          | Linux | macOS | Windows | Android | iOS |
|   :---   | :---: | :---: | :---:   | :---: | :---:   |
| Google Chrome  | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: |
| Microsoft Edge  | :white_check_mark: | :white_check_mark: | :white_check_mark: |_ | _ |
| Mozilla Firefox  | :white_check_mark: | :white_check_mark: | :white_check_mark: |_ | _ |
| Apple Safari  | _ | :white_check_mark: | _ | _ | :white_check_mark: |

#### Apps:

|          | Android | iOS | Windows | 
|   :---   | :---: | :---: | :---:   |
| Native  |:white_check_mark: | :white_check_mark: | N/A | 
| Hybrid  | :white_check_mark: | :white_check_mark: | N/A | 
| Flutter | :white_check_mark: | :white_check_mark: | N/A | 
| WPF  |  N/A | N/A |:white_check_mark: |

#### Other:

| API | Database | CLI | PDF | JSON | YAML | Excel | Property |
| :---: | :---: | :---:|:---:|:---:|:---:|:---:|:---:|
| :white_check_mark: |:white_check_mark: | :white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |:white_check_mark: |

#### Test orchestration:

| TestNG | JUnit5 | Cucumber |
| :---: |:---: |:---: |
| :white_check_mark: |:white_check_mark: |:white_check_mark: |


<br/><br/>

<a id="tech-stack"></a>

## 👨‍💻 Tech stack [⤴](#-table-of-contents)

### Developed using:

<table border="0" align="center">
 <tr>
  <td align="center">
   <br/>
<a href="https://www.oracle.com/eg/java/technologies/downloads/" target="_blank"><img src="https://allurereport.org/images/java_ico.png" alt="Java" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://maven.apache.org/" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/52/Apache_Maven_logo.svg/340px-Apache_Maven_logo.svg.png" alt="Maven" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.jetbrains.com/idea/" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/9c/IntelliJ_IDEA_Icon.svg/1200px-IntelliJ_IDEA_Icon.svg.png" alt="IntelliJ IDEA" height="50px"></a>
  <br/><br/></td></tr></table>

### Powered by:

<table border="0" align="center">
 <tr>
  <td align="center">
   <br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://www.selenium.dev/" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Selenium_logo.svg/768px-Selenium_logo.svg.png?20210927154434" alt="Selenium" height="50px"></a>&nbsp;&nbsp;&nbsp;&nbsp;
      <a href="https://appium.io/" target="_blank"><img src="https://appium.github.io/appium/docs/en/2.0/assets/images/appium-logo-horiz.png" alt="Appium" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br/><a href="https://rest-assured.io/" target="_blank"><img src="https://rest-assured.io/img/logo-transparent.png" alt="REST Assured" height="50px"></a>&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://rest-assured.io/" target="_blank"><img alt="REST Assured" height="50px" src="https://rest-assured.io/img/name-transparent.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://opencv.org/" target="_blank"><img src="https://github.com/user-attachments/assets/944f0043-7b76-472a-8fa6-9beee1ac3a2f" alt="OpenCV" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br/><a href="https://testng.org/" target="_blank"><img src="https://545767148-files.gitbook.io/~/files/v0/b/gitbook-x-prod.appspot.com/o/spaces%2F-MdBdUMSCcMYTyNwZf80%2Fuploads%2Fgit-blob-7e5b23257dbb5cc3262c56840d5cf9fa85b27dce%2Ftestng.png?alt=media" alt="TestNG" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://junit.org/junit5/" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/5/59/JUnit_5_Banner.png" alt="JUnit5" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://cucumber.io/" target="_blank"><img src="https://raw.githubusercontent.com/cucumber/cucumber-ruby/main/docs/img/cucumber-open-logo.png" alt="Cucumber Open" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br/><a href="https://allurereport.org/" target="_blank"><img src="https://allurereport.org/svg/logo-report-sign.svg" alt="Allure Reports" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://reportportal.io/" target="_blank"><img src="https://i0.wp.com/blog.nashtechglobal.com/wp-content/uploads/2023/06/MicrosoftTeams-image-72.png" alt="ReportPortal" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br/><a href="https://www.selenium.dev/documentation/grid/" target="_blank"><img src="https://media.softwaresim.com/Selenium_Grid_mpxkym-600.webp" alt="Selenium Grid" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://github.com/features/actions" target="_blank"><img src="https://github.githubassets.com/images/modules/site/features/actions-icon-actions.svg" alt="GitHub Actions" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br/><a href="https://github.com/dependabot" target="_blank"><img src="https://miro.medium.com/max/929/1*Lqt3yQYXJ-dmVuQEgpYcXQ.png" alt="Dependabot" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://codeql.github.com/" target="_blank"><img src="https://github.gallerycdn.vsassets.io/extensions/github/vscode-codeql/1.7.7/1670939628664/Microsoft.VisualStudio.Services.Icons.Default" alt="CodeQL" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://app.codacy.com/gh/ShaftHQ/SHAFT_ENGINE/dashboard" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/9e/Codacy-logo-black.svg/2560px-Codacy-logo-black.svg.png" alt="Codacy" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.jacoco.org/jacoco/" target="_blank"><img src="https://www.jacoco.org/images/jacoco.png" alt="JaCoCo" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://app.codecov.io/gh/ShaftHQ/SHAFT_ENGINE" target="_blank"><img src="https://assets-global.website-files.com/5f217a8e6bc2c82a9d803089/6387929c3810ef832471584f_codecov.png" alt="CodeCov" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://central.sonatype.com/" target="_blank"><img src="https://central.sonatype.com/sonatype-repository-logo-reverse.svg" alt="sonatype" height="50px"></a>
  <br/><br/></td></tr></table>

<br/><br/>
<a id="support-and-contributions"></a>

## 🤝 Support & contributions [⤴](#-table-of-contents)
- Feel free to create PRs directly. [This lovely tutorial](https://dev.to/genicsblog/how-to-create-a-pull-request-in-github-correctly-20np) will help.
- Join us via Slack & Facebook
<table border="0" align="center">
 <tr>
  <td align="center">
<br/><a href="https://join.slack.com/t/shaft-engine/shared_invite/zt-oii5i2gg-0ZGnih_Y34NjK7QqDn01Dw" target="_blank"><img src="https://a.slack-edge.com/80588/marketing/img/icons/icon_slack_hash_colored.png" alt="automatest-workspace" height="50"/></a>  <a href="https://www.facebook.com/groups/Automatest" target="_blank"><img src="https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEjLMDbbgNWvnrNY3pjRSfgqZCPIbGMnVRm1jaaoGhgT2Buv-ipatDIe9zjRJIM1b8eZTZm7csh-R1vfHWwwW9nSlEC4agzoLrGqRsRWogha5oZIYS4LXXLSrAg7ekta6niiXxt5XHe_oLU/s200/f_logo_RGB-Blue_1024.png" alt="Automatest" height="50"/></a>
 <br/><br/> </td></tr></table>

<br/><br/>

> [!IMPORTANT]
> Stop Reinventing the wheel! Start using SHAFT!

<table border="0" align="center">
 <tr>
  <td align="center">
<a href="https://ShaftHQ.github.io/" target="_blank"><img width="400" alt="SHAFT_ENGINE" src="src/main/resources/images/shaft.png"></a>
</td></tr></table>
