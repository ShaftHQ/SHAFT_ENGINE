package com.shaft.gui.internal.locator;

import lombok.Getter;
import org.openqa.selenium.By;
import org.openqa.selenium.support.locators.RelativeLocator;

import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicBoolean;

public class LocatorBuilder {
    private static final ThreadLocal<Locators> mode = new ThreadLocal<>();
    @Getter
    static ThreadLocal<By> iFrameLocator = new ThreadLocal<>();
    @Getter
    static ThreadLocal<By> shadowDomLocator = new ThreadLocal<>();
    String partialXpath;
    private String tagName = "*";
    private ArrayList<String> parameters = new ArrayList<>();
    private String order = "";

    @SuppressWarnings("unused")
    private LocatorBuilder() {
        // do nothing
    }

    private LocatorBuilder(String tagName, ArrayList<String> parameters, @SuppressWarnings("SameParameterValue") String order) {
        mode.set(Locators.XPATH);
        this.tagName = tagName;
        this.parameters = parameters;
        this.order = order;
    }

    public static LocatorBuilder hasTagName(String tagName) {
        return new LocatorBuilder(tagName, new ArrayList<>(), "");
    }

    public LocatorBuilder hasAttribute(String attribute) {
        parameters.add("[@" + attribute + "]");
        return this;
    }

    public LocatorBuilder hasAttribute(String attribute, String value) {
        parameters.add("[@" + attribute + "=\"" + value + "\"]");
        return this;
    }

    public LocatorBuilder containsAttribute(String attribute, String value) {
        parameters.add("[contains(@" + attribute + ",\"" + value + "\")]");
        return this;
    }

    public LocatorBuilder hasId(String id) {
        parameters.add("[@id=\"" + id + "\"]");
        return this;
    }

    public LocatorBuilder containsId(String id) {
        parameters.add("[contains(@id,\"" + id + "\")]");
        return this;
    }

    public LocatorBuilder hasClass(String className) {
        parameters.add("[@class=\"" + className + "\"]");
        return this;
    }

    public LocatorBuilder containsClass(String className) {
        parameters.add("[contains(@class,\"" + className + "\")]");
        return this;
    }

    public LocatorBuilder hasText(String text) {
        parameters.add("[.=\"" + text + "\"]");
        return this;
    }

    public LocatorBuilder containsText(String text) {
        parameters.add("[contains(.,\"" + text + "\")]");
        return this;
    }

    public LocatorBuilder hasIndex(int index) {
        this.order = String.valueOf(index);
        return this;
    }

    public LocatorBuilder isFirst() {
        this.order = "1";
        return this;
    }

    public LocatorBuilder isLast() {
        this.order = "last()";
        return this;
    }

    /**
     * Syntactic Sugar
     *
     * @return self reference to continue building the locator
     */
    public LocatorBuilder and() {
        return this;
    }

    public RelativeLocator.RelativeBy relativeBy() {
        return RelativeLocator.with(By.xpath(buildXpathExpression()));
    }

    public XpathAxis axisBy() {
        mode.set(Locators.XPATH);
        partialXpath = buildXpathExpression();
        return new XpathAxis(this);
    }

    public By build() {
        By locator;
        AtomicBoolean isShadowElement = new AtomicBoolean(false);
        parameters.forEach(parameter -> isShadowElement.set(parameter.toLowerCase().contains("shadow")));
        if (mode.get() == Locators.CSS || isShadowElement.get()) {
            locator = By.cssSelector(buildSelectorExpression());
        } else {
            locator = By.xpath(buildXpathExpression());
        }
        mode.set(Locators.XPATH);
        return locator;
    }

    private String buildXpathExpression() {
        StringBuilder xpathExpression;
        if (partialXpath == null) {
            xpathExpression = new StringBuilder();
            xpathExpression.append("//")
                    .append(tagName);
        } else {
            xpathExpression = new StringBuilder(partialXpath);
        }
        parameters.forEach(xpathExpression::append);
        parameters.clear(); //resetting the parameters is important only in case of using the axis and needing to build a partialXpath
        if (!order.isEmpty()) {
            return "(" + xpathExpression + ")[" + this.order + "]";
        } else {
            return xpathExpression.toString();
        }
    }

    String buildSelectorExpression() {
        StringBuilder cssExpression = new StringBuilder();
        tagName = tagName.equals("*") ? "" : tagName;
        cssExpression.append(tagName);
        parameters.forEach(parameter -> cssExpression.append(parameter.replace("@", "")));
        if (!order.isEmpty()) {
            if (order.matches("[0-9]+")) {
                order = ":nth-child(" + order + ")";
            } else if (order.equals("last()")) {
                order = ":last-child";
            }
            return "(" + cssExpression + ")" + this.order;
        } else {
            return cssExpression.toString();
        }
    }

    public ShadowLocatorBuilder insideShadowDom(By shadowDomLocator) {
        mode.set(Locators.CSS);
        return new ShadowLocatorBuilder(shadowDomLocator, By.cssSelector(buildSelectorExpression()));
    }
}
