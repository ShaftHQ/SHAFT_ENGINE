package com.shaft.gui.element.internal;

import com.shaft.enums.internal.FlutterFindingStrategy;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.github.ashwith.flutter.FlutterElement;
import io.github.ashwith.flutter.FlutterFinder;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;

import java.util.Collections;
import java.util.List;

//ToDo -> make sure elementInformation object is fully utilized for flutter elements fast identification
//ToDo -> findElements placeholder methods needs to be looked at
@SuppressWarnings("unused")
public abstract class FlutterBy extends By {
    private final FlutterFindingStrategy flutterFindingStrategy;

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

    public static FlutterBy descendant(FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
        return new ByDescendant(of, matching, matchRoot, firstMatchOnly);
    }

    public static FlutterBy ancestor(FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
        return new ByAncestor(of, matching, matchRoot, firstMatchOnly);
    }

    public List<Object> identifyFlutterElement(FlutterFinder finder) {
        try {
            switch (flutterFindingStrategy) {
                case TYPE -> {
                    return ((ByType) this).identifyElement(finder, ((ByType) this).type);
                }
                case VALUE_KEY_STRING -> {
                    return ((ByValueKeyString) this).identifyElement(finder, ((ByValueKeyString) this).valueKey);
                }
                case VALUE_KEY_INT -> {
                    return ((ByValueKeyInt) this).identifyElement(finder, ((ByValueKeyInt) this).valueKey);
                }
                case TEXT -> {
                    return ((ByText) this).identifyElement(finder, ((ByText) this).text);
                }
                case DESCENDANT -> {
                    return ((ByDescendant) this).identifyElement(finder, ((ByDescendant) this).of, ((ByDescendant) this).matching,
                            ((ByDescendant) this).matchRoot, ((ByDescendant) this).firstMatchOnly);
                }
                case ANCESTOR -> {
                    return ((ByAncestor) this).identifyElement(finder, ((ByAncestor) this).of, ((ByAncestor) this).matching,
                            ((ByAncestor) this).matchRoot, ((ByAncestor) this).firstMatchOnly);
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
            elementInformation.setLocator(this);
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
        private final String valueKey;

        public ByValueKeyString(String valueKey) {
            super(FlutterFindingStrategy.VALUE_KEY_STRING);
            this.valueKey = valueKey;
        }

        private List<Object> identifyElement(FlutterFinder finder, String valueKey) {
            ElementInformation elementInformation;
            elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byValueKey(valueKey));
            elementInformation.setLocator(this);
            return elementInformation.toList();
        }

        @Override
        public List<WebElement> findElements(SearchContext context) {
            return context.findElements(this);
        }

        @Override
        public String toString() {
            return "By.valueKey: " + this.valueKey;
        }

    }

    public static class ByValueKeyInt extends FlutterBy {
        private final int valueKey;

        public ByValueKeyInt(int valueKey) {
            super(FlutterFindingStrategy.VALUE_KEY_INT);
            this.valueKey = valueKey;
        }

        private List<Object> identifyElement(FlutterFinder finder, int valueKey) {
            ElementInformation elementInformation;
            elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byValueKey(valueKey));
            elementInformation.setLocator(this);
            return elementInformation.toList();
        }

        @Override
        public List<WebElement> findElements(SearchContext context) {
            return context.findElements(this);
        }

        @Override
        public String toString() {
            return "By.valueKey: " + this.valueKey;
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
            elementInformation.setLocator(this);
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

    public static class ByDescendant extends FlutterBy {
        private final FlutterBy of;
        private final FlutterBy matching;
        private final boolean matchRoot;
        private final boolean firstMatchOnly;

        public ByDescendant(FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
            super(FlutterFindingStrategy.DESCENDANT);
            this.of = of;
            this.matching = matching;
            this.matchRoot = matchRoot;
            this.firstMatchOnly = firstMatchOnly;
        }

        private List<Object> identifyElement(FlutterFinder finder, FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
            ElementInformation elementInformation;
            elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byDescendant((FlutterElement) identifyElementType(finder, of).get(1),
                    (FlutterElement) identifyElementType(finder, matching).get(1), matchRoot, firstMatchOnly));
            elementInformation.setLocator(this);
            return elementInformation.toList();
        }

        //Todo -> enhance this logic to not use if else and use switch
        private List<Object> identifyElementType(FlutterFinder finder, FlutterBy flutterBy) {
            if (flutterBy instanceof ByText byText) {
                return ((ByText) flutterBy).identifyElement(finder, ((ByText) flutterBy).text);
            } else if (flutterBy instanceof ByType byType) {
                return ((ByType) flutterBy).identifyElement(finder, ((ByType) flutterBy).type);
            } else if (flutterBy instanceof ByValueKeyString byValueKeyString) {
                return ((ByValueKeyString) flutterBy).identifyElement(finder, ((ByValueKeyString) flutterBy).valueKey);
            } else if (flutterBy instanceof ByValueKeyInt byValueKeyInt) {
                return ((ByValueKeyInt) flutterBy).identifyElement(finder, ((ByValueKeyInt) flutterBy).valueKey);
            }
            return Collections.emptyList();
        }

        @Override
        public List<WebElement> findElements(SearchContext context) {
            return context.findElements(this);
        }

        @Override
        public String toString() {
            return "By.Descendant: \n" + "descendant of: " + this.of + "\nmatching: " +
                    this.matching + "\nmatchRoot: " + this.matchRoot + "\nfirstMatchOnly: " + this.firstMatchOnly;
        }

    }

    public static class ByAncestor extends FlutterBy {
        private final FlutterBy of;
        private final FlutterBy matching;
        private final boolean matchRoot;
        private final boolean firstMatchOnly;

        public ByAncestor(FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
            super(FlutterFindingStrategy.ANCESTOR);
            this.of = of;
            this.matching = matching;
            this.matchRoot = matchRoot;
            this.firstMatchOnly = firstMatchOnly;
        }

        private List<Object> identifyElement(FlutterFinder finder, FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
            ElementInformation elementInformation;
            elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byAncestor((FlutterElement) identifyElementType(finder, of).get(1),
                    (FlutterElement) identifyElementType(finder, matching).get(1), matchRoot, firstMatchOnly));
            elementInformation.setLocator(this);
            return elementInformation.toList();
        }

        //Todo -> enhance this logic to not use if else and use switch
        private List<Object> identifyElementType(FlutterFinder finder, FlutterBy flutterBy) {
            if (flutterBy instanceof ByText byText) {
                return ((ByText) flutterBy).identifyElement(finder, ((ByText) flutterBy).text);
            } else if (flutterBy instanceof ByType byType) {
                return ((ByType) flutterBy).identifyElement(finder, ((ByType) flutterBy).type);
            } else if (flutterBy instanceof ByValueKeyString byValueKeyString) {
                return ((ByValueKeyString) flutterBy).identifyElement(finder, ((ByValueKeyString) flutterBy).valueKey);
            } else if (flutterBy instanceof ByValueKeyInt byValueKeyInt) {
                return ((ByValueKeyInt) flutterBy).identifyElement(finder, ((ByValueKeyInt) flutterBy).valueKey);
            }
            return Collections.emptyList();
        }

        @Override
        public List<WebElement> findElements(SearchContext context) {
            return context.findElements(this);
        }

        @Override
        public String toString() {
            return "By.Ancestor: \n" + "ancestor of: " + this.of + "\nmatching: " +
                    this.matching + "\nmatchRoot: " + this.matchRoot + "\nfirstMatchOnly: " + this.firstMatchOnly;
        }

    }

}

