package io.github.shafthq.shaft.gui.element;

import lombok.Getter;
import lombok.Setter;
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

        public static ElementInformation fromList(List<Object> elementInformation) {
                var temp = new ElementInformation();
                temp.setNumberOfFoundElements((int) elementInformation.get(0));
                temp.setFirstElement((WebElement) elementInformation.get(1));
                temp.setLocator((By) elementInformation.get(2));
                temp.setOuterHTML((String) elementInformation.get(3));
                temp.setInnerHTML((String) elementInformation.get(4));
                temp.setElementName((String) elementInformation.get(5));
                return temp;
        }

        public List<Object> toList() {
                var temp = new ArrayList<>();
                temp.add(numberOfFoundElements);
                temp.add(firstElement);
                temp.add(locator);
                temp.add(outerHTML);
                temp.add(innerHTML);
                temp.add(elementName);
                return temp;
        }
}
