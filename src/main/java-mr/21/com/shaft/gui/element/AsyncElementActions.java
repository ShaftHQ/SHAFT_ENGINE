package com.shaft.gui.element;

import com.shaft.enums.internal.ClipboardAction;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

public class AsyncElementActions {
    private final List<Callable<ElementActions>> actionsList;
    private final ElementActions elementActions;

    public AsyncElementActions() {
        this.elementActions = ElementActions.getInstance();
        actionsList = new ArrayList<>();
    }


    public AsyncElementActions type(By elementLocator, String text) {
        addActionBuilder(() -> elementActions.type(elementLocator, text));
        return this;
    }

    public AsyncElementActions click(By elementLocator) {
        addActionBuilder(() -> elementActions.click(elementLocator));
        return this;
    }

    public AsyncElementActions select(By elementLocator, String text) {
        addActionBuilder(() -> elementActions.select(elementLocator, text));
        return this;
    }

    public AsyncElementActions clear(By elementLocator) {
        addActionBuilder(() -> elementActions.clear(elementLocator));
        return this;
    }
    public AsyncElementActions doubleClick(By elementLocator) {
        addActionBuilder(() -> elementActions.doubleClick(elementLocator));
        return this;
    }

    public AsyncElementActions clickUsingJavascript(By elementLocator) {
        addActionBuilder(() -> elementActions.clickUsingJavascript(elementLocator));
        return this;
    }

    public AsyncElementActions keyPress(By elementLocator, Keys key) {
        addActionBuilder(() -> elementActions.keyPress(elementLocator,key));
        return this;
    }

    public AsyncElementActions clipboardActions(By elementLocator, ClipboardAction action) {
        addActionBuilder(() -> elementActions.clipboardActions(elementLocator, action));
        return this;
    }

    public AsyncElementActions captureScreenshot(By elementLocator) {
        addActionBuilder(() -> elementActions.captureScreenshot(elementLocator));
        return this;
    }

    public AsyncElementActions hover(By elementLocator) {
        addActionBuilder(() -> elementActions.hover(elementLocator));
        return this;
    }

    public AsyncElementActions setContext(String context) {
        addActionBuilder(() -> elementActions.setContext(context));
        return this;
    }

    public AsyncElementActions typeAppend(By elementLocator, String text) {
        addActionBuilder(() -> elementActions.typeAppend(elementLocator, text));
        return this;
    }

    public AsyncElementActions clickAndHold(By elementLocator) {
        addActionBuilder(() -> elementActions.clickAndHold(elementLocator));
        return this;
    }

    public AsyncElementActions setValueUsingJavaScript(By elementLocator, String value) {
        addActionBuilder(() -> elementActions.setValueUsingJavaScript(elementLocator, value));
        return this;
    }

    public AsyncElementActions typeSecure(By elementLocator, String text) {
        addActionBuilder(() -> elementActions.typeSecure(elementLocator, text));
        return this;
    }

    public AsyncElementActions submitFormUsingJavaScript(By elementLocator) {
        addActionBuilder(() -> elementActions.submitFormUsingJavaScript(elementLocator));
        return this;
    }

    public AsyncElementActions typeFileLocationForUpload(By elementLocator, String filePath) {
        addActionBuilder(() -> elementActions.typeFileLocationForUpload(elementLocator, filePath));
        return this;
    }

    public AsyncElementActions dragAndDrop(By sourceElementLocator, By destinationElementLocator) {
        addActionBuilder(() -> elementActions.dragAndDrop(sourceElementLocator, destinationElementLocator));
        return this;
    }




    private void addActionBuilder(Callable<ElementActions> action) { actionsList.add(action);}

    public AsyncElementActions perform() {
        try (ExecutorService myExecutor = Executors.newVirtualThreadPerTaskExecutor()) {
            List<Future<ElementActions>> actions = myExecutor.invokeAll(actionsList);

            for (Future<ElementActions> action : actions) {
                try {
                    action.get();
                } catch (InterruptedException | ExecutionException e) {
                    e.printStackTrace();
                }
            }
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        return new AsyncElementActions();
    }
}
