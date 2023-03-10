package io.github.shafthq.shaft.gui.locator;

import io.github.shafthq.shaft.driver.DriverFactoryHelper;
import io.github.shafthq.shaft.gui.element.ElementActionsHelper;
import org.openqa.selenium.By;

public class ShadowLocatorBuilder {
    public static By shadowDomLocator;
    public static By cssSelector;
    private LocatorBuilder builder;

    public ShadowLocatorBuilder(LocatorBuilder builder, By shadowDomLocator, By cssSelector) {
        ShadowLocatorBuilder.shadowDomLocator = shadowDomLocator;
        ShadowLocatorBuilder.cssSelector = cssSelector;
        this.builder = builder;
    }

    public By build() {
        //TODO: wrap with a fluent wait
        LocatorBuilder.setShadowElement(DriverFactoryHelper.getDriver().get().findElement(shadowDomLocator).getShadowRoot().findElement(By.cssSelector(builder.buildSelectorExpression())));
        var suggestedUniqueXpath = ElementActionsHelper.suggestNewXpathUsingJavascript(DriverFactoryHelper.getDriver().get(), LocatorBuilder.getShadowElement());
        return By.xpath(suggestedUniqueXpath);
    }
}