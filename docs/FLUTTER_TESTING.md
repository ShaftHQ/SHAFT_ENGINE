# Flutter App Testing with SHAFT Engine

SHAFT Engine now supports automated testing of Flutter applications using the Appium Flutter Driver. This integration allows you to seamlessly test Flutter apps on both Android and iOS platforms.

## Prerequisites

### 1. Install Appium Server
First, install Appium with the Flutter driver plugin:

```bash
# Install Appium globally
npm install -g appium

# Install the Flutter driver plugin
appium driver install --source npm appium-flutter-driver
```

### 2. Verify Installation
Verify that the Flutter driver is installed:

```bash
appium driver list --installed
```

You should see `flutter` in the list of installed drivers.

### 3. Prepare Your Flutter App
Your Flutter app must be built in either **debug** or **profile** mode. The Appium Flutter Driver does **not** support release mode.

To enable Flutter driver integration in your app, add the following to your `main.dart`:

```dart
import 'package:flutter/material.dart';
import 'package:flutter_driver/driver_extension.dart';

void main() {
  // Enable Flutter Driver extension before calling runApp
  enableFlutterDriverExtension();
  
  runApp(MyApp());
}
```

Then build your app:

```bash
# For Android
flutter build apk --debug

# For iOS
flutter build ios --debug
```

## Usage in SHAFT Engine

### Basic Setup

To test a Flutter app using SHAFT Engine, you need to:

1. Enable Flutter support by setting the `flutter.enabled` property to `true`
2. Configure the automation name to `Flutter`
3. Specify the app path or URL
4. Set up your Appium server connection

### Example Test Class

Here's a complete example of a Flutter test using SHAFT Engine with TestNG:

```java
package com.example.tests;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class FlutterAppTest {
    private SHAFT.GUI.WebDriver driver;

    @BeforeMethod
    public void setup() {
        // Enable Flutter driver
        SHAFT.Properties.mobile.set().flutterEnabled(true);
        
        // Set platform and automation name
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.FLUTTER);
        
        // Configure Appium server
        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
        
        // Set app path (local file)
        SHAFT.Properties.mobile.set().app("path/to/your/app-debug.apk");
        
        // Or use app package and activity for installed apps
        // SHAFT.Properties.mobile.set().appPackage("com.example.myapp");
        // SHAFT.Properties.mobile.set().appActivity("MainActivity");
        
        // Initialize driver
        driver = new SHAFT.GUI.WebDriver();
    }

    @Test
    public void testFlutterApp() {
        // Your test logic here
        // Use FlutterFinder methods to locate Flutter widgets
        
        // Example: Find and tap a button
        driver.element().click(AppiumBy.accessibilityId("loginButton"));
        
        // Verify text is displayed
        driver.assertThat().element(AppiumBy.accessibilityId("welcomeMessage"))
              .text().isEqualTo("Welcome!");
    }

    @AfterMethod
    public void teardown() {
        driver.quit();
    }
}
```

### Configuration Properties

You can configure Flutter testing using properties file or programmatically:

#### Properties File (custom.properties)
```properties
# Enable Flutter driver
flutter.enabled=true

# Platform configuration
targetPlatform=Android
mobile_automationName=Flutter

# Appium server
executionAddress=localhost:4723

# App configuration
mobile_app=src/test/resources/apps/my-flutter-app.apk

# Optional: Device configuration
mobile_deviceName=Android Emulator
mobile_platformVersion=13.0
```

#### Programmatic Configuration
```java
// Enable Flutter
SHAFT.Properties.mobile.set().flutterEnabled(true);

// Platform and automation
SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
SHAFT.Properties.mobile.set().automationName(AutomationName.FLUTTER);

// Appium server
SHAFT.Properties.platform.set().executionAddress("localhost:4723");

// App path
SHAFT.Properties.mobile.set().app("path/to/app.apk");

// Optional device settings
SHAFT.Properties.mobile.set().deviceName("Android Emulator");
SHAFT.Properties.mobile.set().platformVersion("13.0");
```

## Locating Flutter Elements

When testing Flutter apps, you can use the FlutterFinder library to locate widgets. SHAFT Engine includes the `appium_flutterfinder_java` dependency automatically.

### Common Flutter Locator Strategies

```java
import pro.truongsinh.appium_flutter.FlutterFinder;

// Create a FlutterFinder instance
FlutterFinder find = new FlutterFinder(driver.getDriver());

// By value key
find.byValueKey("myButton");

// By text
find.text("Submit");

// By type
find.byType("TextField");

// By semantics label
find.bySemanticsLabel("Login Button");

// Combining finders
find.ancestor(
    find.byType("ListView"),
    find.byValueKey("item_1")
);
```

### Using with SHAFT's Fluent API

You can integrate Flutter finders with SHAFT's fluent API:

```java
// Convert FlutterFinder to By locator
By loginButton = AppiumBy.accessibilityId("loginButton");

// Use with SHAFT's fluent element actions
driver.element()
      .type(loginButton, "username")
      .and().click(submitButton)
      .and().assertThat(welcomeMessage).text().contains("Welcome");
```

## Working with Flutter Widgets

### Text Input
```java
By usernameField = AppiumBy.accessibilityId("usernameField");
driver.element().type(usernameField, "testuser");
```

### Button Clicks
```java
By loginButton = AppiumBy.accessibilityId("loginButton");
driver.element().click(loginButton);
```

### Scrolling
```java
import com.shaft.gui.element.TouchActions;

driver.touch().swipeElementIntoView(
    AppiumBy.accessibilityId("targetWidget"),
    TouchActions.SwipeDirection.DOWN
);
```

### Assertions
```java
// Text assertion
driver.assertThat()
      .element(AppiumBy.accessibilityId("statusMessage"))
      .text()
      .isEqualTo("Success");

// Visibility assertion
driver.assertThat()
      .element(AppiumBy.accessibilityId("errorDialog"))
      .exists();
```

## Cloud Execution

SHAFT Engine's Flutter integration works seamlessly with cloud providers:

### BrowserStack
```java
SHAFT.Properties.platform.set().executionAddress("browserstack");
SHAFT.Properties.browserStack.set().platformVersion("13.0");
SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
SHAFT.Properties.browserStack.set().appRelativeFilePath("path/to/app.apk");
SHAFT.Properties.mobile.set().flutterEnabled(true);
SHAFT.Properties.mobile.set().automationName(AutomationName.FLUTTER);
```

### LambdaTest
```java
SHAFT.Properties.platform.set().executionAddress("lambdatest");
SHAFT.Properties.lambdaTest.set().platformVersion("13.0");
SHAFT.Properties.lambdaTest.set().deviceName("Galaxy S21");
SHAFT.Properties.mobile.set().flutterEnabled(true);
SHAFT.Properties.mobile.set().automationName(AutomationName.FLUTTER);
```

## Troubleshooting

### Common Issues

1. **"Could not find Flutter driver"**
   - Ensure the Flutter driver is installed: `appium driver install --source npm appium-flutter-driver`
   - Verify with: `appium driver list --installed`

2. **"Flutter driver extension not found"**
   - Make sure your app includes `enableFlutterDriverExtension()` in `main.dart`
   - App must be built in debug or profile mode, not release mode

3. **"Cannot find element"**
   - Ensure Flutter widgets have proper keys or accessibility labels
   - Use Flutter's `Key` widget: `Key('myButton')`
   - Add semantics: `Semantics(label: 'Submit Button', child: MyWidget())`

4. **Session creation fails**
   - Check that Appium server is running: `appium`
   - Verify the server address matches your configuration
   - Ensure the app path is correct and accessible

### Debug Mode

Enable debug logging to troubleshoot issues:

```properties
# In custom.properties
log4j_logLevel=DEBUG
```

Or programmatically:
```java
SHAFT.Properties.log4j.set().logLevel("DEBUG");
```

## Best Practices

1. **Use Meaningful Keys**: Always add keys to important Flutter widgets for easier element identification:
   ```dart
   ElevatedButton(
     key: Key('submitButton'),
     onPressed: () {},
     child: Text('Submit'),
   )
   ```

2. **Add Semantics**: Use semantics for better accessibility and test automation:
   ```dart
   Semantics(
     label: 'User Login Form',
     child: Form(...)
   )
   ```

3. **Wait for Elements**: SHAFT automatically handles waits, but you can configure timeout:
   ```java
   SHAFT.Properties.timeouts.set().elementIdentificationTimeout(30);
   ```

4. **Use Fluent API**: Leverage SHAFT's fluent API for readable tests:
   ```java
   driver.element()
         .type(usernameField, "user")
         .and().type(passwordField, "pass")
         .and().click(loginButton)
         .and().assertThat(dashboard).exists();
   ```

5. **Clean Up Resources**: Always quit the driver in teardown:
   ```java
   @AfterMethod(alwaysRun = true)
   public void teardown() {
       driver.quit();
   }
   ```

## Example Test Suite

Complete example with multiple tests:

```java
package com.example.tests;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.testng.annotations.*;

public class FlutterAppTestSuite {
    private static SHAFT.GUI.WebDriver driver;
    
    private By usernameField = AppiumBy.accessibilityId("usernameField");
    private By passwordField = AppiumBy.accessibilityId("passwordField");
    private By loginButton = AppiumBy.accessibilityId("loginButton");
    private By dashboardTitle = AppiumBy.accessibilityId("dashboardTitle");
    private By logoutButton = AppiumBy.accessibilityId("logoutButton");

    @BeforeClass
    public void setupClass() {
        // Configure Flutter testing
        SHAFT.Properties.mobile.set().flutterEnabled(true);
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.FLUTTER);
        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
        SHAFT.Properties.mobile.set().app("src/test/resources/apps/flutter-demo.apk");
    }

    @BeforeMethod
    public void setup() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @Test(description = "Verify successful login with valid credentials")
    public void testValidLogin() {
        driver.element()
              .type(usernameField, "testuser")
              .and().type(passwordField, "testpass")
              .and().click(loginButton)
              .and().assertThat(dashboardTitle).text().isEqualTo("Dashboard");
    }

    @Test(description = "Verify error message with invalid credentials")
    public void testInvalidLogin() {
        By errorMessage = AppiumBy.accessibilityId("errorMessage");
        
        driver.element()
              .type(usernameField, "wronguser")
              .and().type(passwordField, "wrongpass")
              .and().click(loginButton)
              .and().assertThat(errorMessage).text().contains("Invalid credentials");
    }

    @Test(description = "Verify logout functionality", dependsOnMethods = "testValidLogin")
    public void testLogout() {
        // First login
        driver.element()
              .type(usernameField, "testuser")
              .and().type(passwordField, "testpass")
              .and().click(loginButton);
        
        // Then logout
        driver.element()
              .click(logoutButton)
              .and().assertThat(loginButton).exists();
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        if (driver != null) {
            driver.quit();
        }
    }
}
```

## Additional Resources

- [Appium Flutter Driver Documentation](https://github.com/appium-userland/appium-flutter-driver)
- [Flutter Testing Guide](https://flutter.dev/docs/testing)
- [SHAFT Engine Documentation](https://shafthq.github.io/)
- [Flutter Finder Java Library](https://github.com/truongsinh/appium-flutter-finder)

## Support

For issues or questions:
- Open an issue on [GitHub](https://github.com/ShaftHQ/SHAFT_ENGINE/issues)
- Join our [Slack community](https://join.slack.com/t/shaft-engine/shared_invite/zt-oii5i2gg-0ZGnih_Y34NjK7QqDn01Dw)
- Check our [documentation](https://shafthq.github.io/)
