package com.shaft.gui.internal.natural;

/**
 * Provider-neutral natural-language action planner.
 *
 * <p>Planners may inspect the current page and return structured SHAFT action
 * steps, but must never execute browser, element, or touch actions.</p>
 */
public interface NaturalActionPlanner {
    /**
     * @return stable planner identifier
     */
    String id();

    /**
     * @return planner priority; higher values are tried first for auto planning
     */
    default int priority() {
        return 0;
    }

    /**
     * Returns whether this planner can attempt the supplied request.
     *
     * @param request natural-action request
     * @return true if this planner can plan the request
     */
    default boolean supports(NaturalActionRequest request) {
        return true;
    }

    /**
     * Plans a natural-language action without executing it.
     *
     * @param request natural-action request
     * @return structured plan, or an unsupported zero-trust plan
     */
    NaturalActionPlan plan(NaturalActionRequest request);
}
