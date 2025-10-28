package com.shaft.gui.internal.locator;

import lombok.NonNull;
import org.openqa.selenium.By;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.openqa.selenium.support.pagefactory.ByAll;

import java.util.Arrays;

public class SmartLocators {

    public static By inputField(@NonNull String elementName) {
        return new ByAll(
                // input
                xpathBuilder(PathStrategy.INPUT_CONTAINS_PLACEHOLDER, elementName),
                xpathBuilder(PathStrategy.INPUT_CONTAINS_ARIA_LABEL, elementName),
                xpathBuilder(PathStrategy.INPUT_CONTAINS_ID, elementName),
                xpathBuilder(PathStrategy.INPUT_CHILD_OF_CONTAINS_TEXT, elementName),
                // text area
                xpathBuilder(PathStrategy.TEXTAREA_CONTAINS_PLACEHOLDER, elementName),
                xpathBuilder(PathStrategy.TEXTAREA_CONTAINS_ARIA_LABEL, elementName),
                xpathBuilder(PathStrategy.TEXTAREA_CONTAINS_ID, elementName),
                xpathBuilder(PathStrategy.TEXTAREA_CHILD_OF_CONTAINS_TEXT, elementName),
                // relative input, straight right of
                xpathBuilder(PathStrategy.INPUT_RELATIVE_STRAIGHT_RIGHT_OF_CONTAINS_TEXT, elementName),
                // axis input
                xpathBuilder(PathStrategy.INPUT_FOLLOWING_CONTAINS_TEXT, elementName),
                xpathBuilder(PathStrategy.INPUT_PRECEDING_CONTAINS_TEXT, elementName)
        );
    }

    public static By clickableField(@NonNull String elementName) {
        return new ByAll(
                xpathBuilder(PathStrategy.BUTTON_CONTAINS_TEXT, elementName),
                xpathBuilder(PathStrategy.BUTTON_CONTAINS_ID, elementName),
                xpathBuilder(PathStrategy.BUTTON_CONTAINS_TITLE, elementName),
                xpathBuilder(PathStrategy.BUTTON_CONTAINS_VALUE, elementName),
                xpathBuilder(PathStrategy.BUTTON_CONTAINS_NAME, elementName),
                xpathBuilder(PathStrategy.BUTTON_CONTAINS_ARIA_LABEL, elementName),

                xpathBuilder(PathStrategy.LINK_CONTAINS_TEXT, elementName),
                xpathBuilder(PathStrategy.LINK_CONTAINS_ID, elementName),
                xpathBuilder(PathStrategy.LINK_CONTAINS_TITLE, elementName),
                xpathBuilder(PathStrategy.LINK_CONTAINS_VALUE, elementName),
                xpathBuilder(PathStrategy.LINK_CONTAINS_NAME, elementName),
                xpathBuilder(PathStrategy.LINK_CONTAINS_ARIA_LABEL, elementName),

                xpathBuilder(PathStrategy.INPUT_TYPE_SUBMIT_CONTAINS_VALUE, elementName),
                xpathBuilder(PathStrategy.INPUT_TYPE_BUTTON_CONTAINS_VALUE, elementName),
                xpathBuilder(PathStrategy.INPUT_IMAGE_CONTAINS_ALT, elementName),

                xpathBuilder(PathStrategy.ROLE_BUTTON_CONTAINS_TEXT, elementName),
                xpathBuilder(PathStrategy.ROLE_LINK_CONTAINS_TEXT, elementName),
                xpathBuilder(PathStrategy.ROLE_TAB_CONTAINS_TEXT, elementName),
                xpathBuilder(PathStrategy.ROLE_MENUITEM_CONTAINS_TEXT, elementName),

                xpathBuilder(PathStrategy.ANY_EXACT_TEXT_CLICKABLE, elementName),
                xpathBuilder(PathStrategy.INPUT_CONTAINS_ID, elementName),

                xpathBuilder(PathStrategy.ANY_CONTAINS_TEXT_FOLLOWING_BUTTON, elementName),
                xpathBuilder(PathStrategy.ANY_CONTAINS_TEXT_FOLLOWING_LINK, elementName)
        );
    }

    private static By xpathBuilder(@NonNull PathStrategy strategy, @NonNull String elementName) {
        StringBuilder xpath = new StringBuilder();

        var potentialFieldNames = new String[]{
                elementName,
                elementName.toLowerCase(),
                elementName.toUpperCase(),
                elementName.toLowerCase().replace(elementName.charAt(0), Character.toUpperCase(elementName.charAt(0)))
        };

        return switch (strategy) {
            case INPUT_CONTAINS_PLACEHOLDER -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//input[contains(@placeholder,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case INPUT_CONTAINS_ID -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//input[contains(@id,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case INPUT_CONTAINS_ARIA_LABEL -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//input[contains(@aria-label,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case INPUT_CHILD_OF_CONTAINS_TEXT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("(//*[contains(text(),'").append(potentialFieldName).append("')]/input)[1]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case TEXTAREA_CONTAINS_PLACEHOLDER -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//textarea[contains(@placeholder,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case TEXTAREA_CONTAINS_ID -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//textarea[contains(@id,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case TEXTAREA_CONTAINS_ARIA_LABEL -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//textarea[contains(@aria-label,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case TEXTAREA_CHILD_OF_CONTAINS_TEXT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("(//*[contains(text(),'").append(potentialFieldName).append("')]/textarea)[1]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case INPUT_RELATIVE_STRAIGHT_RIGHT_OF_CONTAINS_TEXT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//*[contains(text(),'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield RelativeLocator.with(By.tagName("input")).straightRightOf(By.xpath(xpath.toString()));
            }
            case INPUT_FOLLOWING_CONTAINS_TEXT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("(//*[contains(text(),'").append(potentialFieldName).append("')]/following::input)[1]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case INPUT_PRECEDING_CONTAINS_TEXT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("(//*[contains(text(),'").append(potentialFieldName).append("')]/preceding::input)[1]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case INPUT_TYPE_SUBMIT_CONTAINS_VALUE -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//input[@type='submit' and contains(@value,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case BUTTON_CONTAINS_TEXT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//button[contains(text(),'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case BUTTON_CONTAINS_ID -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//button[contains(@id,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case BUTTON_CONTAINS_NAME -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//button[contains(@name,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case BUTTON_CONTAINS_ARIA_LABEL -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//button[contains(@aria-label,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case BUTTON_CONTAINS_TITLE -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//button[contains(@title,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case BUTTON_CONTAINS_VALUE -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//button[contains(@value,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case LINK_CONTAINS_ID -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//a[contains(@id,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case LINK_CONTAINS_TEXT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//a[contains(text(),'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case LINK_CONTAINS_TITLE -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//a[contains(@title,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case LINK_CONTAINS_VALUE -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//a[contains(@value,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case LINK_CONTAINS_NAME -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//a[contains(@name,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case LINK_CONTAINS_ARIA_LABEL -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//a[contains(@aria-label,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case ANY_EXACT_TEXT_CLICKABLE -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//*[text()='").append(potentialFieldName).append("' and (@role='button' or @type='submit')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case INPUT_IMAGE_CONTAINS_ALT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//input[@type='image' and contains(@alt,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case INPUT_TYPE_BUTTON_CONTAINS_VALUE -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//input[@type='button' and contains(@value,'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case ROLE_BUTTON_CONTAINS_TEXT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//*[contains(@role,'button') and contains(text(),'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case ROLE_LINK_CONTAINS_TEXT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//*[contains(@role,'link') and contains(text(),'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case ROLE_TAB_CONTAINS_TEXT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//*[contains(@role,'tab') and contains(text(),'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case ROLE_MENUITEM_CONTAINS_TEXT -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("//*[contains(@role,'menuitem') and contains(text(),'").append(potentialFieldName).append("')]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case ANY_CONTAINS_TEXT_FOLLOWING_BUTTON -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("(//*[contains(text(),'").append(potentialFieldName).append("')]/following::button)[1]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
            case ANY_CONTAINS_TEXT_FOLLOWING_LINK -> {
                Arrays.stream(potentialFieldNames).iterator()
                        .forEachRemaining(potentialFieldName -> xpath.append("(//*[contains(text(),'").append(potentialFieldName).append("')]/following::a)[1]|"));
                xpath.deleteCharAt(xpath.length() - 1);
                yield By.xpath(xpath.toString());
            }
        };
    }

    private enum PathStrategy {
        //inputField_exclusive
        INPUT_CONTAINS_PLACEHOLDER, INPUT_CHILD_OF_CONTAINS_TEXT,
        TEXTAREA_CONTAINS_PLACEHOLDER, TEXTAREA_CONTAINS_ID, TEXTAREA_CHILD_OF_CONTAINS_TEXT,
        INPUT_RELATIVE_STRAIGHT_RIGHT_OF_CONTAINS_TEXT,
        INPUT_FOLLOWING_CONTAINS_TEXT,
        //inputField_exclusive; generated by Claude 3.5 Sonnet
        INPUT_CONTAINS_ARIA_LABEL, INPUT_PRECEDING_CONTAINS_TEXT, TEXTAREA_CONTAINS_ARIA_LABEL,
        //shared
        INPUT_CONTAINS_ID,
        //clickableField_exclusive
        BUTTON_CONTAINS_TEXT, BUTTON_CONTAINS_ID,
        //clickableFeild_exclusive; generated by Claude 3.5 Sonnet
        LINK_CONTAINS_TEXT, INPUT_TYPE_SUBMIT_CONTAINS_VALUE, ANY_EXACT_TEXT_CLICKABLE,
        BUTTON_CONTAINS_TITLE, BUTTON_CONTAINS_VALUE, BUTTON_CONTAINS_NAME, BUTTON_CONTAINS_ARIA_LABEL,
        LINK_CONTAINS_TITLE, LINK_CONTAINS_VALUE, LINK_CONTAINS_NAME, LINK_CONTAINS_ARIA_LABEL, LINK_CONTAINS_ID,
        INPUT_IMAGE_CONTAINS_ALT, INPUT_TYPE_BUTTON_CONTAINS_VALUE,
        ROLE_BUTTON_CONTAINS_TEXT, ROLE_LINK_CONTAINS_TEXT, ROLE_TAB_CONTAINS_TEXT, ROLE_MENUITEM_CONTAINS_TEXT,
        // CLICKABLE FIELD, ADDED AFTER REALISTIC TESTING
        ANY_CONTAINS_TEXT_FOLLOWING_BUTTON, ANY_CONTAINS_TEXT_FOLLOWING_LINK
    }
}
