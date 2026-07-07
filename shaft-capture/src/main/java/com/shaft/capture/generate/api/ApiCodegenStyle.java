package com.shaft.capture.generate.api;

/**
 * How recorded transactions are grouped into generated test methods.
 */
public enum ApiCodegenStyle {
    /**
     * One {@code @Test} method per origin, executing all of that origin's transactions in
     * recorded order, with {@link TransactionCorrelator} chaining correlated values between them
     * through local variables. This is the flagship style: it is what makes a create -&gt;
     * read/update sequence assert as a coherent scenario instead of independent literal replays.
     */
    SCENARIO,
    /**
     * One independent {@code @Test} method per transaction. Simpler and fully isolated (no
     * variable chaining between methods, since each runs standalone) -- every {@code VOLATILE}
     * response value is asserted/replayed as its recorded literal rather than correlated, since
     * there is no shared later-request scope for a variable to be reused in.
     */
    PER_REQUEST
}
