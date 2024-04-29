package com.shaft.gui.element.internal;

import com.google.common.annotations.Beta;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.github.ashwith.flutter.FlutterElement;
import io.github.ashwith.flutter.FlutterFinder;
import lombok.Getter;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

@SuppressWarnings("unused")
@Beta
public abstract class FlutterBy extends By implements By.Remotable {
    private final FlutterFindingStrategy flutterFindingStrategy;
    @Getter
    private final By.Remotable.Parameters remoteParameters;

    /**
     * Constructs a new {@code FlutterBy} instance with the specified finding strategy and locator string.
     *
     * @param findingStrategy The finding strategy for locating Flutter elements.
     * @param locatorString   The locator string used for identifying elements.
     * @throws IllegalArgumentException If the locator string is null or empty.
     */
    protected FlutterBy(FlutterFindingStrategy findingStrategy, String locatorString) {
        Preconditions.checkArgument(!Strings.isNullOrEmpty(locatorString), "Must supply a not empty locator value.");
        this.remoteParameters = new By.Remotable.Parameters(findingStrategy.name(), locatorString);
        this.flutterFindingStrategy = findingStrategy;
    }

    /**
     * Locates elements by their visible text.
     *
     * @param text The visible text of the element to locate.
     * @return A {@code FlutterBy} instance representing the strategy for locating elements by text.
     */
    public static FlutterBy text(String text) {
        return new ByText(text);
    }

    /**
     * Locates elements by their value key (String).
     *
     * @param valueKey The value key (String) of the element to locate.
     * @return A {@code FlutterBy} instance representing the strategy for locating elements by value key (String).
     */
    public static FlutterBy valueKey(String valueKey) {
        return new ByValueKeyString(valueKey);
    }

    /**
     * Locates elements by their value key (int).
     *
     * @param valueKey The value key (int) of the element to locate.
     * @return A {FlutterBy} instance representing the strategy for locating elements by value key (int).
     */
    public static FlutterBy valueKey(int valueKey) {
        return new ByValueKeyInt(valueKey);
    }

    /**
     * Locates elements by their type.
     *
     * @param type The type of the element to locate.
     * @return A {@code FlutterBy} instance representing the strategy for locating elements by type.
     */
    public static FlutterBy type(String type) {
        return new ByType(type);
    }

    /**
     * Locates descendant elements.
     *
     * @param of             The parent element.
     * @param matching       The child element to match.
     * @param matchRoot      Whether to match the root element.
     * @param firstMatchOnly Whether to match only the first occurrence.
     * @return A {@code FlutterBy} instance representing the strategy for locating descendant elements.
     */
    public static FlutterBy descendant(FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
        return new ByDescendant(of, matching, matchRoot, firstMatchOnly);
    }

    /**
     * Locates ancestor elements.
     *
     * @param of             The child element.
     * @param matching       The ancestor element to match.
     * @param matchRoot      Whether to match the root element.
     * @param firstMatchOnly Whether to match only the first occurrence.
     * @return A {@code FlutterBy} instance representing the strategy for locating ancestor elements.
     */
    public static FlutterBy ancestor(FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
        return new ByAncestor(of, matching, matchRoot, firstMatchOnly);
    }

    /**
     * Identifies the Flutter element based on the specified finder and the finding strategy.
     *
     * @param finder The Flutter finder to use for identifying elements.
     * @return A list of objects representing the identified Flutter element.
     */
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

    private static ElementInformation createElementInformation(FlutterElement element, FlutterBy locator) {
        ElementInformation elementInformation = new ElementInformation();
        elementInformation.setFirstElement(element);
        elementInformation.setLocator(locator);
        elementInformation.setElementName(JavaHelper.formatLocatorToString(locator));
        return elementInformation;
    }

    @Override
    public List<WebElement> findElements(SearchContext context) {
        return context.findElements(this);
    }

    @Override
    public WebElement findElement(SearchContext context) {
        return context.findElement(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        } else if (o != null && this.getClass() == o.getClass()) {
            if (!super.equals(o)) {
                return false;
            } else {
                FlutterBy flutterBy = (FlutterBy) o;
                return Objects.equals(this.remoteParameters, flutterBy.remoteParameters) && Objects.equals(this.flutterFindingStrategy.name(), flutterBy.flutterFindingStrategy.name());
            }
        } else {
            return false;
        }
    }

    @Override
    public String toString() {
        return String.format("%s.%s: %s", FlutterBy.class.getSimpleName(), this.flutterFindingStrategy.name(), this.remoteParameters.value());
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), this.remoteParameters.value(), this.flutterFindingStrategy.name());
    }

    /**
     * Enumeration of Flutter finding strategies.
     */
    public enum FlutterFindingStrategy {
        VALUE_KEY_STRING, VALUE_KEY_INT, TYPE, TEXT, DESCENDANT, ANCESTOR
    }

    /**
     * The Class for locating elements by their visible text.
     */
    public static class ByText extends FlutterBy {
        private final String text;

        public ByText(String text) {
            super(FlutterFindingStrategy.TEXT, text);
            this.text = text;

        }

        private List<Object> identifyElement(FlutterFinder finder, String text) {
            ElementInformation elementInformation = createElementInformation(finder.byText(text), this);
            return elementInformation.toList();
        }
    }

    /**
     * The Class for locating elements by their value key (String).
     */
    public static class ByValueKeyString extends FlutterBy implements Serializable {
        private final String valueKey;

        public ByValueKeyString(String valueKey) {
            super(FlutterFindingStrategy.VALUE_KEY_STRING, valueKey);
            this.valueKey = valueKey;
        }

        private List<Object> identifyElement(FlutterFinder finder, String valueKey) {
            ElementInformation elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byValueKey(valueKey));
            elementInformation.setLocator(this);
            elementInformation.setElementName(JavaHelper.formatLocatorToString(this));
            return elementInformation.toList();
        }
    }

    /**
     * The Class for locating elements by their value key (int).
     */
    public static class ByValueKeyInt extends FlutterBy implements Serializable {
        private final int valueKey;

        public ByValueKeyInt(int valueKey) {
            super(FlutterFindingStrategy.VALUE_KEY_INT, String.valueOf(valueKey));
            this.valueKey = valueKey;
        }

        private List<Object> identifyElement(FlutterFinder finder, int valueKey) {
            ElementInformation elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byValueKey(valueKey));
            elementInformation.setLocator(this);
            elementInformation.setElementName(JavaHelper.formatLocatorToString(this));
            return elementInformation.toList();
        }
    }

    /**
     * The Class for locating elements by their type.
     */
    public static class ByType extends FlutterBy implements Serializable {
        private final String type;

        public ByType(String type) {
            super(FlutterFindingStrategy.TYPE, type);
            this.type = type;
        }

        private List<Object> identifyElement(FlutterFinder finder, String type) {
            ElementInformation elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byType(type));
            elementInformation.setLocator(this);
            elementInformation.setElementName(JavaHelper.formatLocatorToString(this));
            return elementInformation.toList();
        }
    }

    /**
     * The Class for locating elements by their descendant.
     */
    public static class ByDescendant extends FlutterBy implements Serializable {
        private final FlutterBy of;
        private final FlutterBy matching;
        private final boolean matchRoot;
        private final boolean firstMatchOnly;

        public ByDescendant(FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
            super(FlutterFindingStrategy.DESCENDANT, "\nof: " + of.remoteParameters + "\nmatching: " + matching.remoteParameters + "\nmatchRoot: " + matchRoot + "\nfirstMatchOnly: " + firstMatchOnly);
            this.of = of;
            this.matching = matching;
            this.matchRoot = matchRoot;
            this.firstMatchOnly = firstMatchOnly;
        }

        private List<Object> identifyElement(FlutterFinder finder, FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
            ElementInformation elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byDescendant((FlutterElement) of.identifyFlutterElement(finder).get(1),
                    (FlutterElement) matching.identifyFlutterElement(finder).get(1), matchRoot, firstMatchOnly));
            elementInformation.setLocator(this);
            elementInformation.setElementName(JavaHelper.formatLocatorToString(this));
            return elementInformation.toList();
        }
    }

    /**
     * The Class for locating elements by their ancestor.
     */
    public static class ByAncestor extends FlutterBy implements Serializable {
        private final FlutterBy of;
        private final FlutterBy matching;
        private final boolean matchRoot;
        private final boolean firstMatchOnly;

        public ByAncestor(FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
            super(FlutterFindingStrategy.ANCESTOR, "\nof: " + of.remoteParameters + "\nmatching: " + matching.remoteParameters + "\nmatchRoot: " + matchRoot + "\nfirstMatchOnly: " + firstMatchOnly);
            this.of = of;
            this.matching = matching;
            this.matchRoot = matchRoot;
            this.firstMatchOnly = firstMatchOnly;
        }

        private List<Object> identifyElement(FlutterFinder finder, FlutterBy of, FlutterBy matching, boolean matchRoot, boolean firstMatchOnly) {
            ElementInformation elementInformation = new ElementInformation();
            elementInformation.setFirstElement(finder.byAncestor((FlutterElement) of.identifyFlutterElement(finder).get(1),
                    (FlutterElement) matching.identifyFlutterElement(finder).get(1), matchRoot, firstMatchOnly));
            elementInformation.setLocator(this);
            elementInformation.setElementName(JavaHelper.formatLocatorToString(this));
            return elementInformation.toList();
        }
    }

}

