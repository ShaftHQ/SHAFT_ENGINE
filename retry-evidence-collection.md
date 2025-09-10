# SHAFT Engine - Retry Evidence Collection

This feature enhances SHAFT's test retry mechanism by automatically collecting comprehensive evidence during retry attempts, including video recordings, network logs, and console logs.

## Overview

When a test fails and retry is enabled, SHAFT now automatically:
1. **Records video** of the retry attempt
2. **Captures network logs** (requests/responses)
3. **Collects console logs** (browser console messages)
4. **Implements enhanced stabilization** (progressive wait strategies)
5. **Attaches all evidence** to the test report

## Configuration

### Enable/Disable Retry Evidence Collection

```java
// Enable retry with evidence collection (default: 2 retry attempts)
SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(2);

// Control what evidence is collected during retries
SHAFT.Properties.flags.set().retryEnableVideoRecording(true);        // Default: true
SHAFT.Properties.flags.set().retryEnableNetworkLogging(true);        // Default: true  
SHAFT.Properties.flags.set().retryEnableConsoleLogging(true);        // Default: true
SHAFT.Properties.flags.set().retryEnableEnhancedStabilization(true); // Default: true
```

### Properties File Configuration

Create or update `src/main/resources/properties/PlatformFlags.properties`:

```properties
# Maximum number of retry attempts (0 = no retries)
retryMaximumNumberOfAttempts=2

# Evidence collection during retries
retryEnableVideoRecording=true
retryEnableNetworkLogging=true  
retryEnableConsoleLogging=true
retryEnableEnhancedStabilization=true
```

## Usage in Tests

### Basic Usage

No code changes needed! Evidence collection is automatic when retry is enabled:

```java
@Test
public void myTest() {
    driver.browser().navigateToURL("https://example.com");
    driver.assertThat().browser().title().contains("Example");
    // If this test fails, retry will automatically collect evidence
}
```

### Advanced Usage with Manual Driver Registration

For tests with custom driver management:

```java
@BeforeMethod
public void setUp() {
    driver = new SHAFT.GUI.WebDriver();
    // Register driver for retry evidence collection
    RetryAnalyzer.setCurrentDriver(driver.getDriver());
}

@AfterMethod  
public void tearDown() {
    RetryAnalyzer.clearCurrentDriver();
    if (driver != null) {
        driver.quit();
    }
}
```

## Evidence Collected

### Video Recording
- **Desktop browsers**: Uses Monte Carlo screen recording
- **Mobile devices**: Uses native screen recording (Android/iOS)
- **Duration**: 10 minutes max per retry (vs 30 minutes for normal tests)
- **Format**: MP4 (converted from AVI for desktop)
- **Attachment**: `Video Recording - Retry #N`

### Network Logs
- **Chrome-based browsers**: Uses DevTools Protocol for detailed network monitoring
- **Other browsers**: Falls back to standard WebDriver logging
- **Captures**: HTTP requests/responses, status codes, URLs
- **Attachment**: `Network Logs - Retry #N`

### Console Logs  
- **Chrome-based browsers**: Uses DevTools Protocol for real-time console monitoring
- **Other browsers**: Falls back to standard WebDriver logging
- **Captures**: Console messages, errors, warnings with timestamps
- **Attachment**: `Console Logs - Retry #N`

### Combined Evidence
- **Summary report**: Contains all evidence types in chronological order
- **Retry details**: Test method, attempt number, configuration status
- **Attachment**: `Combined Retry Evidence - Retry #N`

## Enhanced Stabilization

When `retryEnableEnhancedStabilization` is enabled, retry attempts include:

1. **Progressive wait strategy**: Additional wait time increases with each retry
   - Retry #1: +2 seconds
   - Retry #2: +4 seconds  
   - Retry #3: +6 seconds (etc.)

2. **Browser state reset**:
   - Dismisses any alert dialogs
   - Clears cookies on retry #2+
   - Additional stabilization can be customized

3. **Element stability checks**: Enhanced synchronization before retrying

## Browser Support

### Full Support (DevTools)
- **Chrome**: Full network and console logging via DevTools
- **Edge**: Full network and console logging via DevTools  
- **Chromium-based browsers**: Full support

### Partial Support (Standard Logging)
- **Firefox**: Basic console logging, video recording
- **Safari**: Basic console logging, video recording
- **Mobile browsers**: Native screen recording, basic logging

## Report Integration

All evidence is automatically attached to:
- **Allure Reports**: As test attachments with proper categorization
- **TestNG Reports**: As embedded artifacts
- **Custom Reports**: Via ReportManager integration

### Example Report Structure
```
Test: myFailedTest
├── Attempt #1 (Failed)
│   └── Standard test artifacts
├── Retry #1 (Failed)  
│   ├── Video Recording - Retry #1
│   ├── Network Logs - Retry #1
│   ├── Console Logs - Retry #1
│   └── Combined Retry Evidence - Retry #1
└── Retry #2 (Passed)
    ├── Video Recording - Retry #2
    ├── Network Logs - Retry #2
    ├── Console Logs - Retry #2
    └── Combined Retry Evidence - Retry #2
```

## Performance Considerations

### Minimal Impact Mode
For performance-sensitive environments:

```java
// Minimal evidence collection
SHAFT.Properties.flags.set().retryEnableVideoRecording(false);
SHAFT.Properties.flags.set().retryEnableNetworkLogging(true);   // Lightweight
SHAFT.Properties.flags.set().retryEnableConsoleLogging(true);   // Lightweight  
SHAFT.Properties.flags.set().retryEnableEnhancedStabilization(false);
```

### Storage Considerations
- Video files can be large (10-50MB per retry)
- Network/console logs are typically small (1-10KB)
- Evidence is cleaned up after test completion
- Consider storage limits for CI/CD environments

## Troubleshooting

### Common Issues

1. **No evidence collected**:
   - Verify retry is enabled: `retryMaximumNumberOfAttempts > 0`
   - Check driver registration: `RetryAnalyzer.setCurrentDriver(driver)`
   - Confirm browser support for logging

2. **Video recording fails**:
   - Ensure local execution (not headless)
   - Check video codec availability
   - Verify screen recording permissions

3. **Network/console logs empty**:
   - Chrome-based browsers required for full logging
   - DevTools session may fail in some environments
   - Falls back to standard logging automatically

### Debug Information

Enable debug logging to troubleshoot:

```java
SHAFT.Properties.reporting.set().debugMode(true);
```

This will log detailed information about evidence collection attempts.

## Examples

See `testPackage.properties.RetryFunctionalityDemoTest` for working examples of:
- Intentional test failure to demonstrate retry
- Evidence collection verification
- Configuration testing

## API Reference

### RetryAnalyzer Methods
- `setCurrentDriver(WebDriver)`: Register driver for evidence collection
- `getCurrentDriver()`: Get registered driver for current thread
- `clearCurrentDriver()`: Clean up driver registration
- `stopRetryEvidenceCollection(testMethod, attempt, success)`: Manual evidence collection

### ConsoleNetworkLogger Methods
- `startLoggingForRetry(WebDriver)`: Start evidence collection
- `stopLoggingAndAttach(testMethod, attempt)`: Stop and attach evidence
- `isLoggingActive()`: Check if logging is active
- `getCurrentConsoleLogs()`: Get current console logs
- `getCurrentNetworkLogs()`: Get current network logs

### RecordManager Methods  
- `startVideoRecordingForRetry(driver, testMethod, attempt)`: Start retry video
- `stopVideoRecordingForRetryAndAttach(testMethod, attempt)`: Stop and attach retry video