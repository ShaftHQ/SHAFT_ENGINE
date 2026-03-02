package testPackage.unitTests;

import com.shaft.tools.internal.support.JavaScriptHelper;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for JavaScriptWaitManager and JavaScriptHelper.
 * Validates the JavaScript snippets used for network and framework readiness checks
 * without requiring a running browser.
 */
public class JavaScriptWaitManagerUnitTest {

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script is defined and non-empty")
    public void testActiveNetworkRequestsCountScriptIsDefined() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertNotNull(script, "Script should not be null");
        Assert.assertFalse(script.isBlank(), "Script should not be blank");
    }

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script instruments XMLHttpRequest and fetch")
    public void testActiveNetworkRequestsCountScriptInstrumentsXHRAndFetch() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertTrue(script.contains("XMLHttpRequest"),
                "Script should reference XMLHttpRequest to track AJAX requests");
        Assert.assertTrue(script.contains("fetch"),
                "Script should reference the fetch API to track fetch requests");
        Assert.assertTrue(script.contains("_shaftNetworkRequests"),
                "Script should use _shaftNetworkRequests to count pending requests");
    }

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script is idempotent via tracker flag")
    public void testActiveNetworkRequestsCountScriptIsIdempotent() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertTrue(script.contains("_shaftNetworkTracker"),
                "Script should check _shaftNetworkTracker flag to prevent double-instrumentation");
    }

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script returns the pending request count")
    public void testActiveNetworkRequestsCountScriptReturnsCount() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertTrue(script.contains("return window._shaftNetworkRequests"),
                "Script should return the current pending request count");
    }

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script protects against negative counts")
    public void testActiveNetworkRequestsCountScriptProtectsAgainstNegativeCounts() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertTrue(script.contains("Math.max(0,"),
                "Script should use Math.max(0, ...) to prevent the counter from going negative");
    }

    @Test(description = "Verify ACTIVE_NETWORK_REQUESTS_COUNT script uses prototype patching for XHR")
    public void testActiveNetworkRequestsCountScriptUsesPrototypePatchingForXHR() {
        String script = JavaScriptHelper.ACTIVE_NETWORK_REQUESTS_COUNT.getValue();
        Assert.assertTrue(script.contains("XMLHttpRequest.prototype.open"),
                "Script should patch XMLHttpRequest.prototype.open to intercept all XHR instances");
    }

    @Test(description = "Verify ANGULAR2_READY_STATE script is defined and checks Angular 2+ testabilities")
    public void testAngular2ReadyStateScriptIsDefined() {
        String script = JavaScriptHelper.ANGULAR2_READY_STATE.getValue();
        Assert.assertNotNull(script, "Angular 2+ readiness script should not be null");
        Assert.assertFalse(script.isBlank(), "Angular 2+ readiness script should not be blank");
        Assert.assertTrue(script.contains("getAllAngularTestabilities"),
                "Script should use getAllAngularTestabilities for Angular 2+ support");
    }

    @Test(description = "Verify ANGULAR2_READY_STATE script returns 0 when Angular 2+ is absent")
    public void testAngular2ReadyStateScriptFallsBackToZeroWhenAngularAbsent() {
        String script = JavaScriptHelper.ANGULAR2_READY_STATE.getValue();
        Assert.assertTrue(script.contains("return 0"),
                "Script should return 0 (stable) when Angular 2+ is not present on the page");
    }

    @Test(description = "Verify ANGULAR2_READY_STATE script checks zone stability")
    public void testAngular2ReadyStateScriptChecksZoneStability() {
        String script = JavaScriptHelper.ANGULAR2_READY_STATE.getValue();
        Assert.assertTrue(script.contains("isStable"),
                "Script should call isStable() on each Angular testability to check for pending async tasks");
    }

    @Test(description = "Verify legacy ANGULAR_READY_STATE script is still present for AngularJS 1.x support")
    public void testLegacyAngularReadyStateScriptIsPresent() {
        String script = JavaScriptHelper.ANGULAR_READY_STATE.getValue();
        Assert.assertNotNull(script, "Legacy AngularJS 1.x script should not be null");
        Assert.assertTrue(script.contains("angular"),
                "Legacy script should reference the AngularJS global");
    }

    @Test(description = "Verify DOCUMENT_READY_STATE script checks document.readyState")
    public void testDocumentReadyStateScriptChecksReadyState() {
        String script = JavaScriptHelper.DOCUMENT_READY_STATE.getValue();
        Assert.assertNotNull(script, "DOCUMENT_READY_STATE script should not be null");
        Assert.assertTrue(script.contains("readyState"),
                "Script should check document.readyState");
    }

    @Test(description = "Verify JQUERY_ACTIVE_STATE script checks jQuery.active count")
    public void testJQueryActiveStateScriptChecksActiveCount() {
        String script = JavaScriptHelper.JQUERY_ACTIVE_STATE.getValue();
        Assert.assertNotNull(script, "JQUERY_ACTIVE_STATE script should not be null");
        Assert.assertTrue(script.contains("jQuery"),
                "Script should reference jQuery to check for active AJAX requests");
    }
}
