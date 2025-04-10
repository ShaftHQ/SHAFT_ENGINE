<!-- Badges provided by https://shields.io/ -->

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
<br/><br/><a href="https://opensource.googleblog.com/2023/05/google-open-source-peer-bonus-program-announces-first-group-of-winners-2023.html"><img width="250" alt="Google Open Source" src="https://github.com/ShaftHQ/SHAFT_ENGINE/assets/19201898/1b6296b4-418d-4cb1-829f-6b15e7d17e87"/></a>
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

## 📜 Table of Contents:

<table border="0" align="center">
 <tr>
  <td valign="top">

- [🏃 Quick start guide](#quick-start-guide)
- [📚 User guide ➜](https://shafthq.github.io/)
- [🌍 Our success partners](#our-success-partners)
- [🚀 Features](#features)
- [👨‍💻 Tech stack](#tech-stack)
- [🤝 Support & contributions](#support-and-contributions)
- [📜 MIT license ➜](LICENSE)

</td>
</tr></table>
<br/><br/>

<a id="quick-start-guide"></a>

## 🏃 Quick start guide [⤴](#-table-of-contents)
### Option 1: Maven archetype
(Recommended for new local sandbox projects)

- The easiest and most straightforward way to create a new project that uses SHAFT.
- Just [follow the simple steps here](https://github.com/ShaftHQ/testng-archetype) to generate your new project with one command (all configurations included).

### Option 2: Template project
(Recommended for new source controlled projects)

- Use our [Template Project](https://github.com/ShaftHQ/using_SHAFT_Engine) to create a new project with one click.
- Follow the steps in the ReadMe to handle project configuration.

### Option 3: Start from scratch
(Recommended if you're upgrading an existing project from Native Selenium WebDriver to SHAFT)

#### Step 1: Initial setup

- Create a new Java/Maven project using Eclipse, IntelliJ or your favourite IDE.
- Copy the highlighted contents of
  this [pom.xml](https://github.com/ShaftHQ/using_SHAFT_Engine/blob/main/GUI_Web/pom.xml#L11-L200) file into yours
  inside the ```<project>``` tag.
- Follow the steps in this footnote in case you are using IntelliJ[^1].

#### Step 2: Creating tests
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

#### Step 3: Managing test data
- Create the following file ```src/test/resources/testDataFiles/simpleJSON.json```.
- Copy the below code snippet into your newly created json file.
```json
{
  "searchQuery": "SHAFT_Engine"
}
```

#### Step 4: Running tests
- Run your ```TestClass.java``` as a TestNG Test Class.
- The execution report will open automatically in your default web browser after the test run is completed.
- <b>Join</b> our ![GitHub Repo stars](https://img.shields.io/github/stars/shafthq/shaft_engine?logoColor=black&style=social) to get notified by email when a new release is pushed out.
- After upgrading your Engine to a new major release it is sometimes recommended to delete the properties
  folder ```src\main\resources\properties``` and allow SHAFT to regenerate the defaults by running any test method.
  [^1]: If you're using Cucumber due to a known issue with IntelliJ you need to edit your run configuration template before running your tests by following these steps:
  <br/>- Open 'Edit Run/Debug Configurations' dialog > Edit Configurations... > Edit configuration templates...
  <br/>- Select <b>Cucumber Java</b> > Program Arguments > and add this argument:
  <br/>`--plugin com.shaft.listeners.CucumberFeatureListener`
  <br/>- After saving the changes, remember to delete any old runs you may have triggered by mistake before adding the needed config.

<br/><br/>

<a id="user-guide"></a>

## 📚 [User guide ➜](https://shafthq.github.io/)
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
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.momah.gov.sa/en"><img height="50" alt="MOMRA (Saudi Arabia's Ministry of Municipal and Rural Affairs)" src="https://www.momah.gov.sa/themes/custom/momrah/assets/images/mh-logo.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.vodafone.com.eg"><img height="50" alt="Vodafone Egypt" src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/95/Vodafone_logo_2017.png/1200px-Vodafone_logo_2017.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://solutions.com.sa/"><img height="50" alt="Solutions by STC" src="https://solutions.com.sa/wp-content/uploads/2019/11/logo.svg"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://gizasystems.com/"><img height="50" alt="GIZA Systems" src="https://gizasystems.com/assets/landing/images/giza-white.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.euronetworldwide.com/"><img height="50" alt="Euronet" src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Euronet_Worldwide_logo.svg/1920px-Euronet_Worldwide_logo.svg.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.terkwaz.com/"><img height="50" alt="Terkwaz Business Solutions" src="https://images.wuzzuf-data.net/files/company_logo/Terkwaz-Solutions-Jordan-35434-1578830823.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.incorta.com/"><img height="50" alt="Incorta" src="https://media-exp1.licdn.com/dms/image/C560BAQHUWHhKl0xrCA/company-logo_200_200/0/1660913597037?e=2147483647&v=beta&t=CiDPUEvlIBqztN5gCre-pQ5f7M-03_02IQgJtL18wG8"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://bayan-tech.com/"><img height="50" alt="BayanTech" src="https://bayan-tech.com/wp-content/uploads/2020/01/Bayan-Logo-2.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://adam.ai/"><img height="50" alt="Adam.ai" src="https://images.prismic.io/adamdotai/8e6625b0-e32b-4bee-b1b8-ebdc1d30cbfe_full-logo.svg?ixlib=gatsbyFP&auto=compress%2Cformat&fit=max&q=50"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.act.eg/"><img height="50" alt="ACT Global Soft" src="https://www.act.eg/wp-content/uploads/2021/01/3.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.elmenus.com/"><img height="50" alt="elmenus" src="https://assets-global.website-files.com/625d19c77d49d0aa53047be3/625d19c77d49d0142c047d36_615c77e71175c38163bca403_elmenus.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.idemia.com/"><img height="50" alt="IDEMIA" src="https://www.idemia.com/wp-content/themes/idemia/assets/img/idemia-group-logo.svg"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.ihorizons.com/"><img height="50" alt="iHorizons" src="https://www.ihorizons.com/sites/all/themes/ihorizons_theme/imgs/logo.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://robustastudio.com/"><img height="50" alt="Robusta" src="https://images.wuzzuf-data.net/files/company_logo/Robusta-Egypt-7927.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.paymob.com/"><img height="50" alt="Paymob Solutions" src="https://www.paymob.com/images/paymobLogo.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://jahezgroup.com/"><img height="50" alt="Jahez Group" src="https://jahezgroup.com/wp-content/uploads/2021/10/slide-1.svg"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://salt.bank/"><img height="50" alt="Salt Bank" src="https://static.wikia.nocookie.net/logopedia/images/3/34/Salt_Bank.svg"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.baianat.com/"><img height="50" alt="Baianat" src="https://mir-s3-cdn-cf.behance.net/user/276/f5dc271705011.5b8c47fcee858.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href=""><img height="50" alt="" src=""></a>
  <br/><br/>
  </td></tr></table>

[^4]: Company names are collected via anonymous surveys and provided freely by engineers who claimed to be using SHAFT_Engine within these companies.

<br/><br/>
<a id="features"></a>

## 🚀 Features [⤴](#-table-of-contents)

### Smart features:

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
   <br/>
<a href="https://www.selenium.dev/" target="_blank"><svg height="50px" id="Layer_1" data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 139.38 34"><defs><style>.cls-1{fill:#43b02a}</style></defs><title>Selenium logo green</title><path class="cls-1" d="M46.2 26.37a18.85 18.85.0 01-2.57-.2 25 25 0 01-2.74-.53v-1.39a25.31 25.31.0 002.71.53 18 18 0 002.5.2 5.51 5.51.0 003.29-.84 2.79 2.79.0 001.14-2.39 2.85 2.85.0 00-1.24-2.49A6 6 0 0048 18.55q-.78-.29-1.67-.55A15.93 15.93.0 0144 17.13a5.92 5.92.0 01-1.58-1.05 3.6 3.6.0 01-.9-1.34A5 5 0 0141.23 13a4.46 4.46.0 01.41-1.93 4.31 4.31.0 011.17-1.5 5.26 5.26.0 011.82-1A8 8 0 0147 8.28a20.51 20.51.0 014.41.57v1.42a20 20 0 00-2.23-.44 15.2 15.2.0 00-2-.15 4.86 4.86.0 00-3.08.9A2.9 2.9.0 0042.88 13a3.25 3.25.0 00.21 1.21 2.61 2.61.0 00.7 1 4.83 4.83.0 001.27.79 14.31 14.31.0 002 .68q1.11.33 2.06.71a6.21 6.21.0 011.65.94 4.09 4.09.0 011.1 1.38 4.54 4.54.0 01.4 2 4.15 4.15.0 01-1.56 3.48A7.16 7.16.0 0146.2 26.37z"></path><path class="cls-1" d="M60.62 26.32a5.46 5.46.0 01-4.28-1.62A6.9 6.9.0 0154.88 20a7.8 7.8.0 011.43-5 5 5 0 014.14-1.75 4.24 4.24.0 013.47 1.43A6.48 6.48.0 0165.1 18.8q0 .54.0.92a3.22 3.22.0 01-.09.64H56.44a5.39 5.39.0 001.17 3.5A4.18 4.18.0 0060.8 25a10.52 10.52.0 001.82-.17 11.77 11.77.0 001.93-.52l.12 1.27a10.68 10.68.0 01-2 .55A11.47 11.47.0 0160.62 26.32zM60.4 14.43q-3.68.0-3.94 4.74h7.15a6.49 6.49.0 00-.78-3.63A2.76 2.76.0 0060.4 14.43z"></path><path class="cls-1" d="M68.64 7h1.58V26.11H68.64z"></path><path class="cls-1" d="M79.56 26.32a5.46 5.46.0 01-4.28-1.62A6.9 6.9.0 0173.83 20a7.8 7.8.0 011.43-5 5 5 0 014.14-1.75 4.24 4.24.0 013.47 1.43A6.48 6.48.0 0184 18.8q0 .54.0.92a3.22 3.22.0 01-.09.64H75.38a5.4 5.4.0 001.17 3.5A4.18 4.18.0 0079.75 25a10.52 10.52.0 001.82-.17 11.8 11.8.0 001.93-.52l.12 1.27a10.68 10.68.0 01-2 .55A11.47 11.47.0 0179.56 26.32zm-.21-11.89q-3.68.0-3.94 4.74h7.15a6.49 6.49.0 00-.78-3.63A2.76 2.76.0 0079.35 14.43z"></path><path class="cls-1" d="M87.51 13.37h1.32l.12 1.49h.12q.94-.45 1.72-.78t1.43-.54a8.42 8.42.0 011.2-.31 6.54 6.54.0 011.1-.09A3.3 3.3.0 0197 14a3.63 3.63.0 01.83 2.63v9.51H96.24v-9a3 3 0 00-.55-2 2.18 2.18.0 00-1.69-.6 7.25 7.25.0 00-2.24.41 20.1 20.1.0 00-2.67 1.12v10H87.51z"></path><path class="cls-1" d="M102.75 10.52a.93.93.0 01-1.06-1 1.06 1.06.0 012.12.0.93.93.0 01-1.06 1zm-.8 2.85h1.58V26.11h-1.58z"></path><path class="cls-1" d="M110.81 26.34q-3.14.0-3.14-3.47v-9.5h1.58v9a3.16 3.16.0 00.48 2 1.92 1.92.0 001.59.6 6.83 6.83.0 002.48-.48q1.25-.48 2.59-1.14V13.37H118V26.11h-1.32l-.12-1.58h-.09l-1.73.81q-.74.34-1.38.57a7.9 7.9.0 01-1.23.33A7.34 7.34.0 01110.81 26.34z"></path><path class="cls-1" d="M122.18 13.37h1.3l.14 1.49h.09a19.53 19.53.0 012.58-1.31 5.51 5.51.0 012-.41 2.83 2.83.0 013 1.77h.12q.8-.5 1.45-.83a12.61 12.61.0 011.2-.54 6.17 6.17.0 011-.31 5.18 5.18.0 011-.09 3.3 3.3.0 012.45.84 3.63 3.63.0 01.83 2.63v9.51h-1.56v-9a2.9 2.9.0 00-.55-2 2.21 2.21.0 00-1.69-.59 5.14 5.14.0 00-1.78.38A14.45 14.45.0 00131.6 16v10.1H130v-9a2.9 2.9.0 00-.55-2 2.21 2.21.0 00-1.69-.59 5.24 5.24.0 00-1.86.4A14 14 0 00123.76 16V26.11h-1.58z"></path><path class="cls-1" d="M21.45 21.51a2.49 2.49.0 00-2.55 2.21.08.08.0 00.08.1h4.95a.08.08.0 00.08-.09 2.41 2.41.0 00-2.56-2.22z"></path><path class="cls-1" d="M32.06 4.91 21.56 16.7a.32.32.0 01-.47.0l-5.36-5.53a.32.32.0 010-.4L17.5 8.5a.32.32.0 01.52.0l3 3.32a.32.32.0 00.49.0L29.87.36A.23.23.0 0029.69.0H.25A.25.25.0 000 .25v33.5A.25.25.0 00.25 34h32a.25.25.0 00.25-.25V5.06A.23.23.0 0032.06 4.91zm-23 25.36a8.08 8.08.0 01-5.74-2 .31.31.0 010-.41l1.25-1.75A.31.31.0 015 26a6.15 6.15.0 004.2 1.64c1.64.0 2.44-.76 2.44-1.56.0-2.48-8.08-.78-8.08-6.06.0-2.33 2-4.27 5.32-4.27a7.88 7.88.0 015.25 1.76.31.31.0 010 .43L12.9 19.65a.31.31.0 01-.45.05 6.08 6.08.0 00-3.84-1.32c-1.28.0-2 .57-2 1.41.0 2.23 8.06.74 8.06 6C14.67 28.33 12.84 30.27 9.05 30.27zM26.68 25.4a.27.27.0 01-.28.28H19a.09.09.0 00-.08.1 2.81 2.81.0 003 2.32 4.62 4.62.0 002.56-.84.27.27.0 01.4.06l.9 1.31a.28.28.0 01-.06.37 6.67 6.67.0 01-4.1 1.28 5.28 5.28.0 01-5.57-5.48 5.31 5.31.0 015.4-5.46c3.11.0 5.22 2.33 5.22 5.74z"></path></svg></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://appium.io/" target="_blank"><img src="https://appium.github.io/appium/docs/en/2.0/assets/images/appium-logo-horiz.png" alt="Appium" height="50px"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br/><a href="https://rest-assured.io/" target="_blank"><img src="https://rest-assured.io/img/logo-transparent.png" alt="REST Assured" height="50px">&nbsp;&nbsp;&nbsp;<img alt="REST Assured" height="50px" src="https://rest-assured.io/img/name-transparent.png"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://opencv.org/" target="_blank"><img src="https://opencv.org/wp-content/uploads/2022/05/logo.png" alt="OpenCV" height="50px"></a>
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

#### Stop Reinventing the wheel! Start using SHAFT!
<a href="https://ShaftHQ.github.io/" target="_blank"><img width="400" alt="SHAFT_ENGINE" src="src/main/resources/images/shaft.png"></a>
