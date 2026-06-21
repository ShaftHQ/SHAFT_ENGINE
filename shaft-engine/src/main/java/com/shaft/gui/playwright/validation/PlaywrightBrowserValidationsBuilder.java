package com.shaft.gui.playwright.validation;

import com.shaft.gui.driver.BrowserAssertions;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.NativeValidationsBuilder;

import java.util.Locale;

public class PlaywrightBrowserValidationsBuilder implements BrowserAssertions {
    private final ValidationEnums.ValidationCategory validationCategory;
    private final PlaywrightSession session;

    public PlaywrightBrowserValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, PlaywrightSession session) {
        this.validationCategory = validationCategory;
        this.session = session;
    }

    @Override
    public NativeValidationsBuilder attribute(String browserAttribute) {
        return object(readBrowserAttribute(browserAttribute));
    }

    @Override
    public NativeValidationsBuilder url() {
        return object(session.page().url());
    }

    @Override
    public NativeValidationsBuilder title() {
        return object(session.page().title());
    }

    @Override
    public NativeValidationsBuilder text() {
        return object(String.valueOf(session.page().evaluate("() => document.body ? document.body.innerText : ''")));
    }

    private Object readBrowserAttribute(String browserAttribute) {
        return switch (browserAttribute.toLowerCase(Locale.ROOT)) {
            case "currenturl", "pageurl", "windowurl", "url" -> session.page().url();
            case "pagesource", "windowsource", "source" -> session.page().content();
            case "title", "windowtitle", "pagetitle" -> session.page().title();
            case "text", "pagetext", "windowtext" -> session.page().evaluate("() => document.body ? document.body.innerText : ''");
            case "windowhandle", "pagehandle", "handle" -> session.pageHandle(session.page());
            case "windowsize", "pagesize", "size" -> session.page().evaluate("() => `${window.innerWidth}x${window.innerHeight}`");
            default -> session.page().evaluate("(attribute) => document.documentElement.getAttribute(attribute)", browserAttribute);
        };
    }

    private NativeValidationsBuilder object(Object actual) {
        return validationCategory == ValidationEnums.ValidationCategory.HARD_ASSERT
                ? Validations.assertThat().object(actual)
                : Validations.verifyThat().object(actual);
    }
}
