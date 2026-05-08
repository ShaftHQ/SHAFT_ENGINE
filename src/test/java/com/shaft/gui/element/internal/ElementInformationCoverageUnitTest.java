package com.shaft.gui.element.internal;

import org.jsoup.helper.ValidationException;
import org.openqa.selenium.By;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

import static org.mockito.Mockito.mock;

public class ElementInformationCoverageUnitTest {

    @Test
    public void fromListShouldMapLegacyPayloadIntoFields() {
        WebElement firstElement = mock(WebElement.class);
        Rectangle rectangle = new Rectangle(1, 2, 3, 4);
        List<Object> payload = List.of(
                2,
                firstElement,
                By.id("sample"),
                "<button>Click</button>",
                "Click",
                "sampleElement",
                "success",
                rectangle
        );

        ElementInformation information = ElementInformation.fromList(payload);

        Assert.assertEquals(information.getNumberOfFoundElements(), 2);
        Assert.assertSame(information.getFirstElement(), firstElement);
        Assert.assertEquals(information.getLocator(), By.id("sample"));
        Assert.assertEquals(information.getOuterHTML(), "<button>Click</button>");
        Assert.assertEquals(information.getInnerHTML(), "Click");
        Assert.assertEquals(information.getElementName(), "sampleElement");
        Assert.assertEquals(information.getActionResult(), "success");
        Assert.assertEquals(information.getElementRect(), rectangle);
    }

    @Test
    public void toListShouldExportFieldsInLegacyOrder() {
        WebElement firstElement = mock(WebElement.class);
        Rectangle rectangle = new Rectangle(11, 22, 33, 44);

        ElementInformation information = new ElementInformation();
        information.setNumberOfFoundElements(3);
        information.setFirstElement(firstElement);
        information.setLocator(By.name("q"));
        information.setOuterHTML("<input value='term'>");
        information.setInnerHTML("");
        information.setElementName("search");
        information.setActionResult("done");
        information.setElementRect(rectangle);

        List<Object> payload = information.toList();

        Assert.assertEquals(payload.size(), 8);
        Assert.assertEquals(payload.get(0), 3);
        Assert.assertSame(payload.get(1), firstElement);
        Assert.assertEquals(payload.get(2), By.name("q"));
        Assert.assertEquals(payload.get(3), "<input value='term'>");
        Assert.assertEquals(payload.get(4), "");
        Assert.assertEquals(payload.get(5), "search");
        Assert.assertEquals(payload.get(6), "done");
        Assert.assertEquals(payload.get(7), rectangle);
    }

    @Test
    public void getElementTextShouldReturnVisibleTextWhenPresent() {
        ElementInformation information = new ElementInformation();
        information.setOuterHTML("<button>Click me</button>");
        information.setInnerHTML("Click me");

        Assert.assertEquals(information.getElementText(), "Click me");
    }

    @Test
    public void getElementTextShouldReturnValueAttributeWhenTextMissing() {
        ElementInformation information = new ElementInformation();
        information.setOuterHTML("<input value='typed value'>");
        information.setInnerHTML("");

        Assert.assertEquals(information.getElementText(), "typed value");
    }

    @Test
    public void getElementTextShouldReturnInnerHtmlWhenNoTextOrValue() {
        ElementInformation information = new ElementInformation();
        information.setOuterHTML("<img src='sample.png'>");
        information.setInnerHTML("fallback content");

        Assert.assertEquals(information.getElementText(), "fallback content");
    }

    @Test
    public void getElementTextShouldReturnEmptyWhenInnerHtmlContainsNestedMarkup() {
        ElementInformation information = new ElementInformation();
        information.setOuterHTML("<img src='sample.png'>");
        information.setInnerHTML("<span>nested</span>");

        Assert.assertEquals(information.getElementText(), "");
    }

    @Test
    public void getElementTextShouldThrowWhenOuterHtmlIsMissing() {
        ElementInformation information = new ElementInformation();
        information.setOuterHTML("");
        information.setInnerHTML("ignored");

        Assert.expectThrows(ValidationException.class, information::getElementText);
    }

    @Test
    public void getElementTagShouldReturnParsedTagNameAndThrowWhenOuterHtmlMissing() {
        ElementInformation withHtml = new ElementInformation();
        withHtml.setOuterHTML("<section>value</section>");

        ElementInformation withoutHtml = new ElementInformation();
        withoutHtml.setOuterHTML("");

        Assert.assertEquals(withHtml.getElementTag(), "section");
        Assert.expectThrows(ValidationException.class, withoutHtml::getElementTag);
    }
}
