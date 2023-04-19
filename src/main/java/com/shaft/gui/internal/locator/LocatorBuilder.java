package com.shaft.gui.internal.locator;

import lombok.Getter;
import lombok.Setter;
import org.openqa.selenium.By;
import org.openqa.selenium.support.locators.RelativeLocator;

import java.util.ArrayList;

public class LocatorBuilder {

    @Getter
    @Setter
    static By shadowDomLocator;
    private String tagName = "*";
    private ArrayList<String> parameters = new ArrayList<>();
    private String order = "";
    @Setter
    private static Locators mode = Locators.XPATH;

    @SuppressWarnings("unused")
    private LocatorBuilder() {
        // do nothing
    }

    private LocatorBuilder(String tagName, ArrayList<String> parameters, @SuppressWarnings("SameParameterValue") String order) {
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

    public By build() {
        if (mode == Locators.CSS) {
            return By.cssSelector(buildSelectorExpression());
        }
        return By.xpath(buildXpathExpression());
    }

    private String buildXpathExpression() {
        StringBuilder xpathExpression = new StringBuilder();
        xpathExpression.append("//")
                .append(tagName);
        parameters.forEach(xpathExpression::append);
        if (!order.equals("")) {
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
        if (!order.equals("")) {
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
        return new ShadowLocatorBuilder(shadowDomLocator, By.cssSelector(buildSelectorExpression()));
    }
}
