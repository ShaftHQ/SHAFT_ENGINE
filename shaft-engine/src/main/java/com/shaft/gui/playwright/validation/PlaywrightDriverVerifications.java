package com.shaft.gui.playwright.validation;

import com.shaft.gui.driver.BrowserAssertions;
import com.shaft.gui.driver.DriverVerifications;
import com.shaft.gui.driver.ElementAssertions;
import com.shaft.gui.driver.ShaftLocator;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.NativeValidationsBuilder;
import org.openqa.selenium.By;

public class PlaywrightDriverVerifications implements DriverVerifications {
    private final PlaywrightSession session;

    public PlaywrightDriverVerifications(PlaywrightSession session) {
        this.session = session;
    }

    @Override
    public BrowserAssertions browser() {
        return new PlaywrightBrowserValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT, session);
    }

    @Override
    public ElementAssertions element(By elementLocator) {
        return element(ShaftLocator.from(elementLocator));
    }

    @Override
    public ElementAssertions element(ShaftLocator elementLocator) {
        return new PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT,
                session, elementLocator.toPlaywrightLocator(session.page()));
    }

    public ElementAssertions element(com.microsoft.playwright.Locator elementLocator) {
        return new PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT, session,
                elementLocator);
    }

    @Override
    public NativeValidationsBuilder object(Object actual) {
        return Validations.verifyThat().object(actual);
    }
}
