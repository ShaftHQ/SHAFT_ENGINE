package com.shaft.gui.internal.locator;

@SuppressWarnings("unused")
public class XpathAxis {
    final LocatorBuilder locatorBuilder;

    XpathAxis(LocatorBuilder locatorBuilder) {
        this.locatorBuilder = locatorBuilder;
    }

    public LocatorBuilder followingSibling(String tagName) {
        locatorBuilder.partialXpath += "//following-sibling::" + tagName;
        return locatorBuilder;
    }

    public LocatorBuilder precedingSibling(String tagName) {
        locatorBuilder.partialXpath += "//preceding-sibling::" + tagName;
        return locatorBuilder;
    }

    public LocatorBuilder ancestor(String tagName) {
        locatorBuilder.partialXpath += "//ancestor::" + tagName;
        return locatorBuilder;
    }

    public LocatorBuilder descendant(String tagName) {
        locatorBuilder.partialXpath += "//descendant::" + tagName;
        return locatorBuilder;
    }

    public LocatorBuilder following(String tagName) {
        locatorBuilder.partialXpath += "//following::" + tagName;
        return locatorBuilder;
    }

    public LocatorBuilder preceding(String tagName) {
        locatorBuilder.partialXpath += "//preceding::" + tagName;
        return locatorBuilder;
    }
}
