package com.shaft.validation.internal.builder;

public interface ValidationsBuilder {
    /**
     * Syntactic sugar
     *
     * @return chainable self-reference
     */
    default ValidationsBuilder and() {
        return this;
    }
}
