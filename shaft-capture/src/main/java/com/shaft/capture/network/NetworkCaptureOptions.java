package com.shaft.capture.network;

import java.util.List;

/**
 * Options controlling {@link CaptureNetworkRecorder} filtering and body-capture behavior.
 *
 * @param includeAssetTypes whether stylesheet/script/image/font/media requests are recorded
 * @param urlIncludeGlobs   URL glob patterns; when non-empty, only matching URLs are recorded
 * @param urlExcludeGlobs   URL glob patterns excluded even when an include glob also matches
 * @param firstPartyOnly    whether only same-origin (relative to the initiator page) requests are recorded
 * @param maxTransactions   maximum number of transactions recorded per session
 * @param maxBodyBytes      maximum number of body bytes persisted per request/response; {@code <= 0} means unlimited
 */
public record NetworkCaptureOptions(
        boolean includeAssetTypes,
        List<String> urlIncludeGlobs,
        List<String> urlExcludeGlobs,
        boolean firstPartyOnly,
        int maxTransactions,
        long maxBodyBytes) {
    /**
     * Default maximum number of recorded transactions per session.
     */
    public static final int DEFAULT_MAX_TRANSACTIONS = 500;

    /**
     * Default maximum number of persisted body bytes per request/response.
     */
    public static final long DEFAULT_MAX_BODY_BYTES = 1_000_000L;

    /**
     * Creates normalized, validated network capture options.
     */
    public NetworkCaptureOptions {
        urlIncludeGlobs = urlIncludeGlobs == null ? List.of() : List.copyOf(urlIncludeGlobs);
        urlExcludeGlobs = urlExcludeGlobs == null ? List.of() : List.copyOf(urlExcludeGlobs);
        if (maxTransactions <= 0) {
            maxTransactions = DEFAULT_MAX_TRANSACTIONS;
        }
        if (maxBodyBytes <= 0) {
            maxBodyBytes = DEFAULT_MAX_BODY_BYTES;
        }
    }

    /**
     * Returns default network capture options: no asset types, no glob filters, first-party
     * only, capped at {@value #DEFAULT_MAX_TRANSACTIONS} transactions.
     *
     * @return default network capture options
     */
    public static NetworkCaptureOptions defaults() {
        return new NetworkCaptureOptions(false, List.of(), List.of(), true,
                DEFAULT_MAX_TRANSACTIONS, DEFAULT_MAX_BODY_BYTES);
    }
}
