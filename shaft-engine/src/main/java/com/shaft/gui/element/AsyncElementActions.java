package com.shaft.gui.element;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.enums.internal.ClipboardAction;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.Beta;
import org.openqa.selenium.By;

import java.util.ArrayList;

/**
 * Provides asynchronous (non-blocking) variants of common {@link ElementActions} operations.
 *
 * <p>Each action method schedules the underlying {@link ElementActions} call on a new virtual thread
 * and returns {@code this} immediately, allowing the caller to queue multiple actions in parallel
 * before waiting for them all to finish via {@link #synchronize()}, {@link #join()}, or {@link #sync()}.
 *
 * <p><b>Thread safety:</b> each {@code AsyncElementActions} instance maintains its own internal list
 * of virtual threads, so instances must not be shared across test threads.
 *
 * <p><b>Example usage:</b>
 * <pre>{@code
 * driver.async().element()
 *     .type(firstNameField, "John")
 *     .type(lastNameField,  "Doe")
 *     .click(submitButton)
 *     .synchronize();
 * }</pre>
 *
 * @see ElementActions
 * @see org.openqa.selenium.Beta
 */
@Beta
public class AsyncElementActions extends FluentWebDriverAction {
    private final ArrayList<Thread> actionThreads = new ArrayList<>();
    private final ElementActions elementActions;

    /**
     * Constructs a new {@code AsyncElementActions} bound to the given driver helper.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * AsyncElementActions asyncActions = new AsyncElementActions(driverFactoryHelper);
     * }</pre>
     *
     * @param helper the {@link DriverFactoryHelper} that provides access to the underlying WebDriver session
     */
    public AsyncElementActions(DriverFactoryHelper helper) {
        initialize(helper);
        this.elementActions = new ElementActions(helper);
    }

    /**
     * Asynchronously types the given text into the element identified by {@code elementLocator}.
     *
     * <p>The action is executed on a new virtual thread; the current thread is not blocked.</p>
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.type(By.id("username"), "testUser");
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @param text           the text to type into the element
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions type(By elementLocator, String text) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.type(elementLocator, text)));
        return this;
    }

    /**
     * Asynchronously clicks the element identified by {@code elementLocator}.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.click(By.id("submitButton"));
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions click(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.click(elementLocator)));
        return this;
    }

    /**
     * Asynchronously selects the option matching {@code text} in the drop-down element
     * identified by {@code elementLocator}.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.select(By.id("countryDropdown"), "Egypt");
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the {@code <select>} element
     * @param text           the visible text of the option to select
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions select(By elementLocator, String text) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.select(elementLocator, text)));
        return this;
    }

    /**
     * Asynchronously clears the content of the element identified by {@code elementLocator}.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.clear(By.id("searchInput"));
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions clear(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.clear(elementLocator)));
        return this;
    }

    /**
     * Asynchronously double-clicks the element identified by {@code elementLocator}.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.doubleClick(By.id("editableCell"));
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions doubleClick(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.doubleClick(elementLocator)));
        return this;
    }

    /**
     * Asynchronously clicks the element identified by {@code elementLocator} using JavaScript.
     *
     * <p>Useful when native WebDriver click is intercepted by overlapping elements.</p>
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.clickUsingJavascript(By.id("hiddenButton"));
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions clickUsingJavascript(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.clickUsingJavascript(elementLocator)));
        return this;
    }

    /**
     * Asynchronously performs the specified clipboard action (e.g., copy, paste, cut) on the element
     * identified by {@code elementLocator}.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.clipboardActions(By.id("textArea"), ClipboardAction.COPY);
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @param action         the {@link ClipboardAction} to perform (e.g., {@code COPY}, {@code PASTE}, {@code CUT})
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions clipboardActions(By elementLocator, ClipboardAction action) {
        actionThreads.add(Thread.ofVirtual().start(() -> {
            var helper = new ElementActionsHelper(false);
            helper.performClipboardActions(driverFactoryHelper.getDriver(), action);
        }));
        return this;
    }

    /**
     * Asynchronously captures a screenshot of the element identified by {@code elementLocator}
     * and attaches it to the Allure report.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.captureScreenshot(By.id("chart"));
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions captureScreenshot(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.captureScreenshot(elementLocator)));
        return this;
    }

    /**
     * Asynchronously moves the mouse cursor over the element identified by {@code elementLocator},
     * triggering any hover-based UI state (e.g., tooltips, drop-down menus).
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.hover(By.id("menuItem"));
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions hover(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.hover(elementLocator)));
        return this;
    }

    /**
     * Asynchronously appends the given text to the current value of the element identified by
     * {@code elementLocator} without clearing its existing content first.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.typeAppend(By.id("notes"), " additional text");
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @param text           the text to append to the element's current value
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions typeAppend(By elementLocator, String text) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.typeAppend(elementLocator, text)));
        return this;
    }

    /**
     * Asynchronously clicks and holds the mouse button down on the element identified by
     * {@code elementLocator} without releasing it, which is useful for drag operations or
     * long-press interactions.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.clickAndHold(By.id("draggableHandle"));
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions clickAndHold(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.clickAndHold(elementLocator)));
        return this;
    }

    /**
     * Asynchronously sets the {@code value} attribute of the element identified by
     * {@code elementLocator} directly via JavaScript, bypassing normal input events.
     *
     * <p>This is useful for read-only or JavaScript-rendered input fields that cannot be
     * interacted with through the standard WebDriver API.</p>
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.setValueUsingJavaScript(By.id("hiddenInput"), "injectedValue");
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @param value          the value to assign to the element's {@code value} attribute
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions setValueUsingJavaScript(By elementLocator, String value) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.setValueUsingJavaScript(elementLocator, value)));
        return this;
    }

    /**
     * Asynchronously types the given text into the element identified by {@code elementLocator}
     * in a secure manner, masking the typed characters in logs and reports to protect sensitive
     * data such as passwords.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.typeSecure(By.id("passwordField"), "s3cr3tP@ssword");
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the target element
     * @param text           the sensitive text to type (will be masked in reports)
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions typeSecure(By elementLocator, String text) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.typeSecure(elementLocator, text)));
        return this;
    }

    /**
     * Asynchronously submits the form that contains the element identified by
     * {@code elementLocator} using JavaScript, bypassing standard form-submission validation.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.submitFormUsingJavaScript(By.id("loginForm"));
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find an element within the target form
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions submitFormUsingJavaScript(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.submitFormUsingJavaScript(elementLocator)));
        return this;
    }

    /**
     * Asynchronously types the absolute path of a local file into the file-upload input element
     * identified by {@code elementLocator}, triggering the browser's file selection mechanism.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.typeFileLocationForUpload(By.id("fileInput"), "/home/user/documents/report.pdf");
     * }</pre>
     *
     * @param elementLocator the {@link By} locator used to find the file-input element
     * @param filePath       the absolute path to the file to upload
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions typeFileLocationForUpload(By elementLocator, String filePath) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.typeFileLocationForUpload(elementLocator, filePath)));
        return this;
    }

    /**
     * Asynchronously drags the element identified by {@code sourceElementLocator} and drops it
     * onto the element identified by {@code destinationElementLocator}.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.dragAndDrop(By.id("draggableCard"), By.id("dropZone"));
     * }</pre>
     *
     * @param sourceElementLocator      the {@link By} locator used to find the element to drag
     * @param destinationElementLocator the {@link By} locator used to find the drop target
     * @return this {@code AsyncElementActions} instance for fluent chaining
     */
    public AsyncElementActions dragAndDrop(By sourceElementLocator, By destinationElementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.dragAndDrop(sourceElementLocator, destinationElementLocator)));
        return this;
    }

    /**
     * Alias for {@link #synchronize()}. Blocks the calling thread until all queued asynchronous
     * actions have completed.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.type(firstField, "value1")
     *             .click(submitButton)
     *             .join();
     * }</pre>
     *
     * @return this {@code AsyncElementActions} instance for fluent chaining
     * @see #synchronize()
     */
    public AsyncElementActions join() {
        return synchronize();
    }

    /**
     * Blocks the calling thread until all previously queued asynchronous actions have finished
     * executing on their respective virtual threads.
     *
     * <p>Any {@link InterruptedException} thrown while waiting for a thread to join is caught and
     * logged via SHAFT's internal reporting utilities.</p>
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.type(firstField, "value1")
     *             .type(secondField, "value2")
     *             .synchronize();
     * }</pre>
     *
     * @return this {@code AsyncElementActions} instance for fluent chaining
     * @see #join()
     * @see #sync()
     */
    public AsyncElementActions synchronize() {
        actionThreads.forEach(actionThread -> {
            try {
                actionThread.join();
            } catch (InterruptedException e) {
                ReportManagerHelper.log(e);
            }
        });
        return this;
    }

    /**
     * Alias for {@link #synchronize()}. Blocks the calling thread until all queued asynchronous
     * actions have completed.
     *
     * <p><b>Example:</b>
     * <pre>{@code
     * asyncActions.type(firstField, "value1")
     *             .click(submitButton)
     *             .sync();
     * }</pre>
     *
     * @return this {@code AsyncElementActions} instance for fluent chaining
     * @see #synchronize()
     */
    public AsyncElementActions sync() {
        return synchronize();
    }
}