package com.shaft.gui.element;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.enums.internal.ClipboardAction;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.Beta;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;

import java.util.ArrayList;

@Beta
public class AsyncElementActions extends FluentWebDriverAction {
    private final ArrayList<Thread> actionThreads = new ArrayList<>();
    private final ElementActions elementActions;

    public AsyncElementActions(DriverFactoryHelper helper) {
        initialize(helper);
        this.elementActions = new ElementActions(helper);
    }

    public AsyncElementActions type(By elementLocator, String text) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.type(elementLocator, text)));
        return this;
    }

    public AsyncElementActions click(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.click(elementLocator)));
        return this;
    }

    public AsyncElementActions select(By elementLocator, String text) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.select(elementLocator, text)));
        return this;
    }

    public AsyncElementActions clear(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.clear(elementLocator)));
        return this;
    }

    public AsyncElementActions doubleClick(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.doubleClick(elementLocator)));
        return this;
    }

    public AsyncElementActions clickUsingJavascript(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.clickUsingJavascript(elementLocator)));
        return this;
    }

    public AsyncElementActions keyPress(By elementLocator, Keys key) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.keyPress(elementLocator, key)));
        return this;
    }

    public AsyncElementActions clipboardActions(By elementLocator, ClipboardAction action) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.clipboardActions(elementLocator, action)));
        return this;
    }

    public AsyncElementActions captureScreenshot(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.captureScreenshot(elementLocator)));
        return this;
    }

    public AsyncElementActions hover(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.hover(elementLocator)));
        return this;
    }

    public AsyncElementActions typeAppend(By elementLocator, String text) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.typeAppend(elementLocator, text)));
        return this;
    }

    public AsyncElementActions clickAndHold(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.clickAndHold(elementLocator)));
        return this;
    }

    public AsyncElementActions setValueUsingJavaScript(By elementLocator, String value) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.setValueUsingJavaScript(elementLocator, value)));
        return this;
    }

    public AsyncElementActions typeSecure(By elementLocator, String text) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.typeSecure(elementLocator, text)));
        return this;
    }

    public AsyncElementActions submitFormUsingJavaScript(By elementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.submitFormUsingJavaScript(elementLocator)));
        return this;
    }

    public AsyncElementActions typeFileLocationForUpload(By elementLocator, String filePath) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.typeFileLocationForUpload(elementLocator, filePath)));
        return this;
    }

    public AsyncElementActions dragAndDrop(By sourceElementLocator, By destinationElementLocator) {
        actionThreads.add(Thread.ofVirtual().start(() -> elementActions.dragAndDrop(sourceElementLocator, destinationElementLocator)));
        return this;
    }

    public AsyncElementActions join() {
        return synchronize();
    }

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

    public AsyncElementActions sync() {
        return synchronize();
    }
}