package com.shaft.gui.element.internal;

import com.shaft.enums.internal.FlutterFindingStrategy;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.github.ashwith.flutter.FlutterFinder;
import java.util.Collections;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;

import java.util.List;

@SuppressWarnings("unused")
public abstract class FlutterBy extends By {
    private final FlutterFindingStrategy flutterFindingStrategy;
    private ElementInformation elementInformation;
    protected FlutterBy(FlutterFindingStrategy findingStrategy) {
        this.flutterFindingStrategy = findingStrategy;
    }

    public static FlutterBy text(String text) {
        return new ByText(text);
    }

    public static FlutterBy valueKey(String valueKey) {
        return new ByValueKeyString(valueKey);
    }

    public static FlutterBy valueKey(int valueKey) {
        return new ByValueKeyInt(valueKey);
    }

    public static FlutterBy type(String type) {
        return new ByType(type);
    }

    public List<Object> identifyFlutterElement(FlutterFinder finder) {
        try {
            switch (flutterFindingStrategy) {
                case TYPE -> {
                    return ((ByType)this).identifyElement(finder, ((ByType) this).type);
                }
                case VALUE_KEY_STRING -> {
                    return ((ByValueKeyString)this).identifyElement(finder, ((ByValueKeyString) this).valueKeyString);
                }
                case VALUE_KEY_INT -> {
                    return ((ByValueKeyInt)this).identifyElement(finder, ((ByValueKeyInt) this).valueKeyInt);
                }
                case TEXT -> {
                    return ((ByText)this).identifyElement(finder, ((ByText) this).text);
                }
                default -> throw new IllegalStateException("Unsupported FindingStrategy: " + flutterFindingStrategy);
            }
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
        return Collections.emptyList();
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof FlutterBy)) {
            return false;
        } else {
            By that = (By) o;
            return this.toString().equals(that.toString());
        }
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    public static class ByText extends FlutterBy {
        private final String text;

        public ByText(String text) {
            super(FlutterFindingStrategy.TEXT);
            this.text = text;

        }

        private List<Object> identifyElement(FlutterFinder finder, String text) {
            ElementInformation elementInformation;
            elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byText(text));
            return elementInformation.toList();
        }

        @Override
        public List<WebElement> findElements(SearchContext context) {
            return context.findElements(this);
        }

        @Override
        public String toString() {
            return "By.text: " + this.text;
        }

    }

    public static class ByValueKeyString extends FlutterBy {
        private final String valueKeyString;

        public ByValueKeyString(String valueKeyString) {
            super(FlutterFindingStrategy.VALUE_KEY_STRING);
            this.valueKeyString = valueKeyString;
        }

        private List<Object> identifyElement(FlutterFinder finder,  String valueKeyString) {
            ElementInformation elementInformation;
            elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byValueKey(valueKeyString));
            return elementInformation.toList();
        }

        @Override
        public List<WebElement> findElements(SearchContext context) {
            return context.findElements(this);
        }

        @Override
        public String toString() {
            return "By.valueKey: " + this.valueKeyString;
        }

    }

    public static class ByValueKeyInt extends FlutterBy {
        private final int valueKeyInt;

        public ByValueKeyInt(int valueKeyInt) {
            super(FlutterFindingStrategy.VALUE_KEY_INT);
            this.valueKeyInt = valueKeyInt;
        }

        private List<Object> identifyElement(FlutterFinder finder, int valueKeyInt) {
            ElementInformation elementInformation;
            elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byValueKey(valueKeyInt));
            return elementInformation.toList();
        }

        @Override
        public List<WebElement> findElements(SearchContext context) {
            return context.findElements(this);
        }

        @Override
        public String toString() {
            return "By.valueKey: " + this.valueKeyInt;
        }

    }

    public static class ByType extends FlutterBy {
        private final String type;

        public ByType(String type) {
            super(FlutterFindingStrategy.TYPE);
            this.type = type;
        }

        private List<Object> identifyElement(FlutterFinder finder, String type) {
            ElementInformation elementInformation;
            elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byType(type));
            return elementInformation.toList();
        }

        @Override
        public List<WebElement> findElements(SearchContext context) {
            return context.findElements(this);
        }

        @Override
        public String toString() {
            return "By.type: " + this.type;
        }

    }

}

