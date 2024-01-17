package com.shaft.gui.element.internal;

import lombok.Getter;
import lombok.Setter;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Element;
import org.openqa.selenium.By;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.List;

@Setter
@SuppressWarnings("unused")
public class ElementInformation {
    @Getter
    private int numberOfFoundElements = 0;
    @Getter
    private WebElement firstElement = null;
    @Getter
    private By locator = null;
    @Getter
    private String outerHTML = "";
    @Getter
    private String innerHTML = "";
    @Getter
    private String elementName = "";
    @Getter
    private String actionResult = "";
    private String elementText = "";
    private String elementTag = "";
    @Getter
    private Rectangle elementRect = null;
    @Getter
    private Element element = null;

    public static ElementInformation fromList(List<Object> elementInformation) {
        var temp = new ElementInformation();
        temp.setNumberOfFoundElements((int) elementInformation.get(0));
        temp.setFirstElement((WebElement) elementInformation.get(1));
        temp.setLocator((By) elementInformation.get(2));
        temp.setOuterHTML((String) elementInformation.get(3));
        temp.setInnerHTML((String) elementInformation.get(4));
        temp.setElementName((String) elementInformation.get(5));
        temp.setActionResult((String) elementInformation.get(6));
        temp.setElementRect((Rectangle) elementInformation.get(7));
        return temp;
    }

    //TODO: generalize this approach to parse all element information and not have to fetch it again
    private static String parseElementText(ElementInformation elementInformation) {
        if (!elementInformation.outerHTML.isEmpty()) {
            // LOGIC:
            // we can use https://jsoup.org/ to parse the HTML
            // when parsing a body fragment, the outerHTML is always wrapped inside <html> and <body> tags
            // we can extract the original tag name of the first element and use it to find the jsoup element
            // then we can do our checks and return TEXT > VALUE > CONTENT > UNDEFINED in that order
            if (elementInformation.element.hasText() && !elementInformation.element.text().isEmpty())
                return elementInformation.element.text();
            if (elementInformation.element.hasAttr("value") && !elementInformation.element.attr("value").isEmpty())
                return elementInformation.element.attr("value");
            if (!elementInformation.innerHTML.isEmpty() && !elementInformation.innerHTML.contains("<"))
                return elementInformation.innerHTML;
        }
        return "";
    }

    private static Element parseElement(ElementInformation elementInformation) {
        if (!elementInformation.outerHTML.isEmpty()) {
            return Jsoup.parse(elementInformation.outerHTML).getElementsByTag("body").getFirst().child(0);
        }
        return new Element("");
    }

    public String getElementText() {
        if (this.element == null)
            this.setElement(parseElement(this));
        this.setElementText(parseElementText(this));
        return this.elementText;
    }

    public String getElementTag() {
        if (this.element == null)
            this.setElement(parseElement(this));
        this.setElementTag(parseElement(this).tagName());
        return this.elementTag;
    }

    public List<Object> toList() {
        var temp = new ArrayList<>();
        temp.add(numberOfFoundElements);
        temp.add(firstElement);
        temp.add(locator);
        temp.add(outerHTML);
        temp.add(innerHTML);
        temp.add(elementName);
        temp.add(actionResult);
        temp.add(elementRect);
        return temp;
    }
}
