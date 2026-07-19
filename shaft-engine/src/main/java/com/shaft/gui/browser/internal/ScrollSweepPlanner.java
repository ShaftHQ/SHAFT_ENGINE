package com.shaft.gui.browser.internal;

/**
 * Pure decision logic for {@code BrowserActions.scrollToLoadAll()}'s bounded progressive scroll
 * sweep (issue #3749, Increment D).
 * <p>
 * Extracted out of the browser-driving loop so the step-count/height-stability stop condition
 * can be unit tested without a real {@code WebDriver}, following the same test-seam philosophy
 * as {@link BidiNetworkActivitySource}'s injected clock.
 */
public final class ScrollSweepPlanner {

    private ScrollSweepPlanner() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Decides whether a bounded progressive scroll sweep should stop after the step that just
     * completed.
     * <p>
     * Stops when either the configured step ceiling has been reached (bounding worst-case cost
     * regardless of page behavior), or the page has stopped growing <b>and</b> the viewport
     * bottom has been reached. A grown page keeps the sweep going even past the visual bottom,
     * since newly appended content may still need one more step to itself be scrolled into view.
     *
     * @param completedSteps     number of scroll steps performed so far (including the one that just completed)
     * @param maxSteps           the configured ceiling ({@code timeouts.lazyLoadingScrollSweepMaxSteps})
     * @param previousPageHeight the page's scroll height before this step
     * @param currentPageHeight  the page's scroll height after this step
     * @param bottomReached      whether the viewport bottom reached the page's scroll height after this step
     * @return {@code true} when the sweep should stop
     */
    public static boolean shouldStop(int completedSteps, int maxSteps, long previousPageHeight,
                                      long currentPageHeight, boolean bottomReached) {
        if (completedSteps >= maxSteps) {
            return true;
        }
        return bottomReached && currentPageHeight <= previousPageHeight;
    }
}
