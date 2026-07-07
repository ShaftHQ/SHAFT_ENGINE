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
    PER_REQUEST,
    /**
     * One {@code @Test} method per origin, mirroring {@link #SCENARIO}'s correlation, but
     * rendered by {@code ApiTestRenderer.render(..., Map)} which interleaves caller-supplied UI
     * action source lines with API response assertions placed immediately after the UI anchor
     * each transaction is correlated with ({@code correlatedUiSequence}). Transactions with no
     * correlated UI sequence are appended at the end of the method, after every anchored
     * transaction, in recorded order.
     */
    HYBRID_UI_API
}
