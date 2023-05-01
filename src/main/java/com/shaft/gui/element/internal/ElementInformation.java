package com.shaft.gui.element.internal;

import lombok.Getter;
import lombok.Setter;
import org.jsoup.Jsoup;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.List;

public class ElementInformation {
        @Setter
        @Getter
        private int numberOfFoundElements = 0;
        @Setter
        @Getter
        private WebElement firstElement = null;
        @Setter
        @Getter
        private By locator = null;
        @Setter
        @Getter
        private String outerHTML = "";
        @Setter
        @Getter
        private String innerHTML = "";
    @Setter
    @Getter
    private String elementName = "";

    @Setter
    @Getter
    private String actionResult = "";

    @Setter
    @Getter
    private String elementText = "";

    public static ElementInformation fromList(List<Object> elementInformation) {
        var temp = new ElementInformation();
        temp.setNumberOfFoundElements((int) elementInformation.get(0));
        temp.setFirstElement((WebElement) elementInformation.get(1));
        temp.setLocator((By) elementInformation.get(2));
        temp.setOuterHTML((String) elementInformation.get(3));
        temp.setInnerHTML((String) elementInformation.get(4));
        temp.setElementName((String) elementInformation.get(5));
        temp.setActionResult((String) elementInformation.get(6));
        temp.setElementText(parseElementText(temp.outerHTML, temp.innerHTML));
        return temp;
    }

    //TODO: generalize this approach to parse all element information and not have to fetch it again
    private static String parseElementText(String outerHTML, String innerHTML) {
        if (!outerHTML.isEmpty()) {
            // LOGIC:
            // we can use https://jsoup.org/ to parse the HTML
            // when parsing a body fragment, the outerHTML is always wrapped inside <html> and <body> tags
            // we can extract the original tag name of the first element and use it to find the jsoup element
            // then we can do our checks and return TEXT > VALUE > CONTENT > UNDEFINED in that order
            var element = Jsoup.parse(outerHTML).getElementsByTag("body").get(0).child(0);
            if (element.hasText() && !element.text().isEmpty())
                return element.text();
            if (element.hasAttr("value") && !element.attr("value").isEmpty())
                return element.attr("value");
            if (!innerHTML.isEmpty() && !innerHTML.contains("<"))
                return innerHTML;
        }
        return "";
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
            temp.add(elementText);
            return temp;
        }
}
