# Logging Refresh Samples

Use these before/after samples in the pull request description. They cover the
observable impact of the logging changes in this branch.

## Full Engine Log Attachment

Before:

```text
[INFO ] Attaching full engine log.
[DEBUG] Full log was read into memory from target/logs/log4j.log.
[DEBUG] Deleted target/logs/log4j.log after attachment.
```

After:

```text
[DEBUG] Attached SHAFT engine execution log.
[DEBUG] Full log attachment used a deduplicated snapshot. Live log remains at target/logs/log4j.log.
```

## Async Log4j Appenders

Before:

```properties
rootLogger=debug, STDOUT, LOGFILE
appender.file.type=File
```

After:

```properties
rootLogger=info, ASYNC_STDOUT, ASYNC_LOGFILE, ASYNC_REPORT_PORTAL
appender.file.type=RollingFile
appender.asyncFile.appenderRef.file.ref=LOGFILE
```

## Thread-Safe Console Redirect

Before:

```text
[INFO ] worker-a started worker-b started
[INFO ] worker-a finished
worker-b finished
```

After:

```text
[INFO ] worker-a started
[INFO ] worker-b started
[INFO ] worker-a finished
[INFO ] worker-b finished
```

## Internal Debug Noise

Before:

```text
[INFO ] Preparing SHAFT project structure.
[INFO ] Attaching SHAFT property files.
[INFO ] Starting ReportPortal reporting.
```

After:

```text
[DEBUG] Preparing SHAFT project structure.
[DEBUG] Attaching SHAFT property files.
[DEBUG] Starting ReportPortal reporting.
```

## Property Updates And Secret Masking

Before:

```text
[INFO ] Updated property browserStack.accessKey to plain-secret-value.
```

After:

```text
[DEBUG] Updated property "browserStack.accessKey" to "********".
```

## Attachment Bookkeeping

Before:

```text
[INFO ] Attaching "Screenshot - Home Page".
[INFO ] Attachment created.
```

After:

```text
[DEBUG] Attached report artifact 'Screenshot - Home Page'.
```

## Retry Diagnostics

Before:

```text
[DEBUG] Retry #1/1 for test: retryLoggingTest
[DEBUG] Full log attached and target/logs/log4j.log removed.
```

After:

```text
[DEBUG] Retry #1/1 for test: retryLoggingTest
[DEBUG] Attached SHAFT engine execution log.
[DEBUG] Live retry diagnostics log remains available at target/logs/log4j.log.
```

## Update Checker

Before:

```text
[INFO ] Checking for engine updates...
[DEBUG] Skipping the engine update check because GitHub is unavailable.
```

After:

```text
[DEBUG] Checking for SHAFT engine updates.
[DEBUG] Engine update check was not completed because GitHub is unavailable.
```

## WebDriver Lifecycle

Before:

```text
[INFO ] Attempting to run locally on: "Windows | Chrome", Headless Execution.
[INFO ] Successfully Opened "Windows | Chrome". (attempt 1/7, elapsed 1842ms).
```

After:

```text
[DEBUG] Starting local WebDriver session: "Windows | Chrome", headless.
[INFO ] Started local WebDriver session: "Windows | Chrome", headless. (attempt 1/7, elapsed 1842ms).
```

## Remote WebDriver Readiness

Before:

```text
[INFO ] Attempting to connect to remote server for up to 2min.
[INFO ] Successfully instantiated remote driver instance.
```

After:

```text
[DEBUG] Waiting up to 2 minute(s) for the remote server to become ready.
[DEBUG] Remote WebDriver session was created.
```

## Cloud Provider Configuration

Before:

```text
[INFO ] BrowserStack Local command: BrowserStackLocal --key abc123
[INFO ] LambdaTest tunnel command: tunnel --accessKey secret-key
```

After:

```text
[DEBUG] BrowserStack Local command: BrowserStackLocal --key ********
[DEBUG] LambdaTest tunnel command: tunnel --accessKey ********
```

## API And GraphQL

Before:

```text
[INFO ] Sending GraphQL request with headers {Authorization=Bearer abc123}.
[INFO ] Get response status code: 200 in 342 ms.
```

After:

```text
[DEBUG] Sending GraphQL request with headers {Authorization=********}.
[DEBUG] Received API response: status=200, time=342ms.
```

## File, Terminal, And Database Actions

Before:

```text
[INFO ] Successfully executed terminal command.
[INFO ] Failed to create a connection with this string "jdbc:example".
```

After:

```text
[DEBUG] Executed local terminal command.
[ERROR] Could not create a database connection with this connection string "jdbc:example" due to an unhandled exception.
```

## Test Data Managers

Before:

```text
[INFO ] Reading test data from the following file [src/test/resources/testDataFiles/users.xlsx].
[INFO ] Loaded Test Data: "src/test/resources/testDataFiles/users.json".
[INFO ] Successfully retrieved Min cell value= 3.0 of column : age.
```

After:

```text
[DEBUG] Reading Excel test data from [src/test/resources/testDataFiles/users.xlsx].
[INFO ] Loaded JSON test data: "src/test/resources/testDataFiles/users.json".
[DEBUG] Read minimum CSV cell value 3.0 of column : age from [src/test/resources/testDataFiles/users.csv].
```

## Accessibility Reporting

Before:

```text
[INFO ] Page fully loaded, starting accessibility scan for: Checkout
[INFO ] Accessibility report generated at: accessibility-reports/AccessibilityReport_Checkout.html
[INFO ] Accessibility score for page 'Checkout' = 96.5%
```

After:

```text
[DEBUG] Page is loaded. Starting accessibility scan for page 'Checkout'.
[DEBUG] Generated accessibility HTML report: accessibility-reports/AccessibilityReport_Checkout.html.
[INFO ] Accessibility score for page 'Checkout' is 96.5%.
```

## Allure Lifecycle

Before:

```text
[INFO ] Allure report generation command exited with code 1.
[INFO ] Failed to execute Allure report generation command: node not found
[INFO ] Allure real-time monitoring skipped: Allure CLI could not be resolved.
```

After:

```text
[DEBUG] Allure report generation exited with code 1.
[DEBUG] Could not execute Allure report generation command: node not found
[DEBUG] Allure real-time monitoring skipped because the Allure CLI could not be resolved.
```

## Browser, Screenshot, Recording, And Security Helpers

Before:

```text
[INFO ] Successfully configured network interceptor.
[WARN ] Failed to take a screenshot after 5 attempts.
[INFO ] Successfully Encrypted "users.json".
```

After:

```text
[INFO ] Configured network interceptor.
[WARN ] Could not take a screenshot after 5 attempts.
[INFO ] Encrypted "users.json".
```
