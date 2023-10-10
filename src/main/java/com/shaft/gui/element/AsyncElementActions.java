package com.shaft.gui.element;

import com.shaft.enums.internal.ElementAction;
import com.shaft.gui.element.internal.ElementActionInformation;
import org.openqa.selenium.By;

import java.util.ArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class AsyncElementActions {
    private final ArrayList<ElementActionInformation> elementActionInformationList;
    private final ElementActions elementActions;

    public AsyncElementActions() {
        this.elementActions = ElementActions.getInstance();
        elementActionInformationList = new ArrayList<>();
    }

    public AsyncElementActions perform() {
        try (ExecutorService myExecutor = Executors.newVirtualThreadPerTaskExecutor()) {
            //begin virtual thread creation fork
            ArrayList<Future<?>> actions = new ArrayList<>();
            elementActionInformationList.forEach(elementActionInformation -> {
                actions.add(myExecutor.submit(() -> {
                    switch (elementActionInformation.action) {
                        case SEND_KEYS ->
                                elementActions.type(elementActionInformation.locator, elementActionInformation.testData);
                        case CLICK -> elementActions.click(elementActionInformation.locator);
                        case CLEAR -> elementActions.clear(elementActionInformation.locator);
                    }
                }));
            });

            //synchronization point
            for (Future<?> action : actions) {
                action.get();
            }
        } catch (ExecutionException | InterruptedException e) {
            throw new RuntimeException(e);
        }
        // return a fresh object
        return new AsyncElementActions();
    }

    public AsyncElementActions and() {
        return this;
    }

    public AsyncElementActions type(By elementLocator, String text) {
        elementActionInformationList.add(new ElementActionInformation(ElementAction.SEND_KEYS, elementLocator, text));
        return this;
    }

    public AsyncElementActions click(By elementLocator) {
        elementActionInformationList.add(new ElementActionInformation(ElementAction.CLICK, elementLocator, ""));
        return this;
    }

    public AsyncElementActions clear(By elementLocator) {
        elementActionInformationList.add(new ElementActionInformation(ElementAction.CLEAR, elementLocator, ""));
        return this;
    }
}
