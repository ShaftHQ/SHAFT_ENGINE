package com.shaft.gui.driver;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Immutable backend-neutral plan for resolving an element at execution time.
 */
public final class ElementTarget {
    private final List<Step> steps;

    private ElementTarget(List<Step> steps) {
        this.steps = List.copyOf(steps);
    }

    /**
     * Creates a portable target from a Selenium locator supported by {@link ShaftLocator#from(By)}.
     *
     * @param locator root locator
     * @return immutable target plan
     */
    public static ElementTarget located(By locator) {
        return located(ShaftLocator.from(Objects.requireNonNull(locator, "locator")));
    }

    /**
     * Creates a portable target from a SHAFT locator.
     *
     * @param locator root locator
     * @return immutable target plan
     */
    public static ElementTarget located(ShaftLocator locator) {
        return new ElementTarget(List.of(new LocatorStep(Objects.requireNonNull(locator, "locator"))));
    }

    /**
     * Returns a new target scoped to a descendant located by the supplied Selenium locator.
     *
     * @param locator descendant locator
     * @return composed immutable target
     */
    public ElementTarget descendant(By locator) {
        return descendant(ShaftLocator.from(Objects.requireNonNull(locator, "locator")));
    }

    /**
     * Returns a new target scoped to a descendant located by the supplied SHAFT locator.
     *
     * @param locator descendant locator
     * @return composed immutable target
     */
    public ElementTarget descendant(ShaftLocator locator) {
        return append(new LocatorStep(Objects.requireNonNull(locator, "locator")));
    }

    /**
     * Returns a new target selecting the zero-based matching element at the current plan step.
     *
     * @param index zero-based index
     * @return indexed immutable target
     */
    public ElementTarget nth(int index) {
        if (index < 0) {
            throw new IllegalArgumentException("Element target index must be zero or greater.");
        }
        return append(new IndexStep(index));
    }

    /**
     * Converts this plan into a Selenium locator that resolves the complete chain on every use.
     *
     * @return lazy Selenium locator
     */
    public By toBy() {
        return new PlannedBy(steps);
    }

    /**
     * Resolves this plan into a Playwright locator chain for the supplied current page.
     *
     * @param page current Playwright page
     * @return lazy Playwright locator chain
     */
    public Locator toPlaywrightLocator(Page page) {
        Objects.requireNonNull(page, "page");
        Locator current = null;
        for (Step step : steps) {
            if (step instanceof LocatorStep locatorStep) {
                current = current == null
                        ? locatorStep.locator().toPlaywrightLocator(page)
                        : current.locator(locatorStep.locator().toPlaywrightSelector());
            } else if (step instanceof IndexStep indexStep) {
                current = Objects.requireNonNull(current, "Element target root locator").nth(indexStep.index());
            }
        }
        return Objects.requireNonNull(current, "Element target root locator");
    }

    private ElementTarget append(Step step) {
        List<Step> composed = new ArrayList<>(steps);
        composed.add(step);
        return new ElementTarget(composed);
    }

    @Override
    public String toString() {
        return "ElementTarget[steps=" + steps.size() + "]";
    }

    private sealed interface Step permits LocatorStep, IndexStep {
    }

    private record LocatorStep(ShaftLocator locator) implements Step {
    }

    private record IndexStep(int index) implements Step {
    }

    private static final class PlannedBy extends By {
        private final List<Step> steps;

        private PlannedBy(List<Step> steps) {
            this.steps = List.copyOf(steps);
        }

        @Override
        public List<WebElement> findElements(SearchContext context) {
            Objects.requireNonNull(context, "context");
            List<? extends SearchContext> contexts = List.of(context);
            List<WebElement> matches = List.of();
            for (Step step : steps) {
                if (step instanceof LocatorStep locatorStep) {
                    List<WebElement> next = new ArrayList<>();
                    for (SearchContext candidate : contexts) {
                        next.addAll(candidate.findElements(locatorStep.locator().toBy()));
                    }
                    matches = List.copyOf(next);
                    contexts = matches;
                } else if (step instanceof IndexStep indexStep) {
                    matches = indexStep.index() < matches.size()
                            ? List.of(matches.get(indexStep.index()))
                            : List.of();
                    contexts = matches;
                }
            }
            return matches;
        }

        @Override
        public String toString() {
            return "ElementTarget";
        }
    }
}
