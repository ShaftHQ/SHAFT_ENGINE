package com.shaft.gui.internal.locator.semantic;

/**
 * Raised when more than one element matches a semantic candidate and no
 * explicit scope/disambiguation was supplied (issue #5457 FR-2 / SC-2).
 * Never silently picks an arbitrary match.
 */
public final class AmbiguousSemanticLocatorException extends IllegalStateException {
    private final SemanticLocatorStrategy strategy;
    private final String expression;
    private final int matchCount;

    public AmbiguousSemanticLocatorException(SemanticLocatorStrategy strategy, String expression, int matchCount) {
        super("Ambiguous semantic locator: strategy=" + strategy
                + " expression=\"" + expression + "\" matchCount=" + matchCount
                + ". Supply scoped disambiguation (nth/scopeRoot) or a stricter signal.");
        this.strategy = strategy;
        this.expression = expression;
        this.matchCount = matchCount;
    }

    public SemanticLocatorStrategy strategy() {
        return strategy;
    }

    public String expression() {
        return expression;
    }

    public int matchCount() {
        return matchCount;
    }
}
