package com.shaft.validation.internal.builder;

import com.shaft.validation.ValidationEnums;
import lombok.Getter;
import org.openqa.selenium.WebDriver;

@Getter
public class WebBrowser implements ValidationsBuilder {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected final WebDriver driver;

    protected String validationMethod;
    protected String browserAttribute;

    protected final StringBuilder reportMessageBuilder;

    public WebBrowser(ValidationEnums.ValidationCategory validationCategory, WebDriver driver, StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.driver = driver;

        this.reportMessageBuilder = reportMessageBuilder;
    }

    /**
     * Use this to check against a certain browser attribute
     *
     * @param browserAttribute the target browser attribute that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public Native attribute(String browserAttribute) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = browserAttribute;
        reportMessageBuilder.append("attribute \"").append(browserAttribute).append("\" ");
        return new Native(this);
    }

    /**
     * Use this to check against the current page URL
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    @SuppressWarnings("SpellCheckingInspection")
    public Native url() {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = "currenturl";
        reportMessageBuilder.append("URL ");
        return new Native(this);
    }

    /**
     * Use this to check against the current page title
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public Native title() {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = "title";
        reportMessageBuilder.append("title ");
        return new Native(this);
    }

}