package com.shaft.api;

import java.io.InterruptedIOException;
import java.net.ConnectException;
import java.net.NoRouteToHostException;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.time.Duration;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Opt-in retry policy for {@link RequestBuilder#perform()}.
 *
 * <p>Retries are limited to idempotent HTTP methods by default. Use
 * {@link #allowNonIdempotentRequests()} only when retrying POST or PATCH is safe
 * for the target API.</p>
 */
public final class RetryPolicy {
    private static final Set<Integer> DEFAULT_TRANSIENT_STATUS_CODES = Set.of(408, 429, 500, 502, 503, 504);
    private static final int DEFAULT_MAX_ATTEMPTS = 3;

    private final Set<Integer> retryStatusCodes;
    private final boolean retryNetworkFailures;
    private int maxAttempts = DEFAULT_MAX_ATTEMPTS;
    private boolean allowNonIdempotentRequests;
    private BackoffType backoffType = BackoffType.FIXED;
    private Duration initialDelay = Duration.ZERO;
    private Duration maxDelay = Duration.ZERO;

    private RetryPolicy(Set<Integer> retryStatusCodes, boolean retryNetworkFailures) {
        this.retryStatusCodes = new LinkedHashSet<>(retryStatusCodes);
        this.retryNetworkFailures = retryNetworkFailures;
    }

    /**
     * Retries transient network failures and common transient HTTP statuses:
     * 408, 429, 500, 502, 503, and 504.
     *
     * @return a retry policy with safe transient defaults
     */
    public static RetryPolicy transientFailures() {
        return new RetryPolicy(DEFAULT_TRANSIENT_STATUS_CODES, true);
    }

    /**
     * Retries the supplied HTTP status codes.
     *
     * @param statusCodes HTTP statuses to retry
     * @return a retry policy for the supplied statuses
     * @throws IllegalArgumentException if no status codes are supplied or a code is outside 100-599
     */
    public static RetryPolicy statusCodes(int... statusCodes) {
        if (statusCodes == null || statusCodes.length == 0) {
            throw new IllegalArgumentException("At least one retry status code is required.");
        }
        LinkedHashSet<Integer> codes = new LinkedHashSet<>();
        Arrays.stream(statusCodes).forEach(code -> {
            if (code < 100 || code > 599) {
                throw new IllegalArgumentException("Retry status code must be between 100 and 599: " + code);
            }
            codes.add(code);
        });
        return new RetryPolicy(codes, false);
    }

    /**
     * Sets the total number of attempts, including the first request.
     *
     * @param maxAttempts total attempts; must be greater than zero
     * @return this retry policy
     */
    public RetryPolicy maxAttempts(int maxAttempts) {
        if (maxAttempts < 1) {
            throw new IllegalArgumentException("maxAttempts must be greater than zero.");
        }
        this.maxAttempts = maxAttempts;
        return this;
    }

    /**
     * Allows retries for POST and PATCH requests.
     *
     * @return this retry policy
     */
    public RetryPolicy allowNonIdempotentRequests() {
        this.allowNonIdempotentRequests = true;
        return this;
    }

    /**
     * Uses the same delay before each retry.
     *
     * @param delay delay before the next attempt
     * @return this retry policy
     */
    public RetryPolicy fixedBackoff(Duration delay) {
        this.initialDelay = requireNonNegative(delay, "delay");
        this.maxDelay = this.initialDelay;
        this.backoffType = BackoffType.FIXED;
        return this;
    }

    /**
     * Doubles the delay after each failed attempt until {@code maxDelay}.
     *
     * @param initialDelay delay before the first retry
     * @param maxDelay     upper delay bound
     * @return this retry policy
     */
    public RetryPolicy exponentialBackoff(Duration initialDelay, Duration maxDelay) {
        this.initialDelay = requireNonNegative(initialDelay, "initialDelay");
        this.maxDelay = requireMaxDelay(maxDelay, this.initialDelay);
        this.backoffType = BackoffType.EXPONENTIAL;
        return this;
    }

    /**
     * Uses exponential backoff with a random delay between zero and the current
     * exponential delay.
     *
     * @param initialDelay maximum delay before the first retry
     * @param maxDelay     upper delay bound
     * @return this retry policy
     */
    public RetryPolicy jitteredBackoff(Duration initialDelay, Duration maxDelay) {
        this.initialDelay = requireNonNegative(initialDelay, "initialDelay");
        this.maxDelay = requireMaxDelay(maxDelay, this.initialDelay);
        this.backoffType = BackoffType.JITTERED;
        return this;
    }

    int maxAttempts() {
        return maxAttempts;
    }

    boolean canRetry(RestActions.RequestType requestType) {
        return maxAttempts > 1 && (allowNonIdempotentRequests || isIdempotent(requestType));
    }

    boolean shouldRetryStatus(int statusCode) {
        return retryStatusCodes.contains(statusCode);
    }

    boolean shouldRetry(Throwable throwable) {
        return retryNetworkFailures && isTransientNetworkFailure(throwable);
    }

    Duration delayBeforeAttempt(int nextAttempt) {
        Duration calculatedDelay = switch (backoffType) {
            case FIXED -> initialDelay;
            case EXPONENTIAL -> exponentialDelay(nextAttempt);
            case JITTERED -> jitteredDelay(exponentialDelay(nextAttempt));
        };
        return calculatedDelay.compareTo(maxDelay) > 0 ? maxDelay : calculatedDelay;
    }

    private static boolean isIdempotent(RestActions.RequestType requestType) {
        return requestType == RestActions.RequestType.GET
                || requestType == RestActions.RequestType.PUT
                || requestType == RestActions.RequestType.DELETE;
    }

    private static boolean isTransientNetworkFailure(Throwable throwable) {
        Throwable cause = throwable;
        while (cause != null) {
            if (cause instanceof SocketTimeoutException
                    || cause instanceof ConnectException
                    || cause instanceof NoRouteToHostException
                    || cause instanceof UnknownHostException
                    || cause instanceof SocketException
                    || cause instanceof InterruptedIOException) {
                return true;
            }
            String simpleName = cause.getClass().getSimpleName();
            if ("ConnectTimeoutException".equals(simpleName)
                    || "ConnectionClosedException".equals(simpleName)
                    || "HttpHostConnectException".equals(simpleName)
                    || "NoHttpResponseException".equals(simpleName)) {
                return true;
            }
            cause = cause.getCause();
        }
        return false;
    }

    private Duration exponentialDelay(int nextAttempt) {
        Duration delay = initialDelay;
        for (int attempt = 2; attempt < nextAttempt; attempt++) {
            delay = delay.multipliedBy(2);
            if (delay.compareTo(maxDelay) >= 0) {
                return maxDelay;
            }
        }
        return delay;
    }

    private static Duration jitteredDelay(Duration upperBound) {
        long upperBoundMillis = upperBound.toMillis();
        if (upperBoundMillis <= 0) {
            return Duration.ZERO;
        }
        return Duration.ofMillis(ThreadLocalRandom.current().nextLong(upperBoundMillis + 1));
    }

    private static Duration requireNonNegative(Duration duration, String name) {
        Objects.requireNonNull(duration, name);
        if (duration.isNegative()) {
            throw new IllegalArgumentException(name + " must not be negative.");
        }
        return duration;
    }

    private static Duration requireMaxDelay(Duration maxDelay, Duration initialDelay) {
        Duration checkedMaxDelay = requireNonNegative(maxDelay, "maxDelay");
        if (checkedMaxDelay.compareTo(initialDelay) < 0) {
            throw new IllegalArgumentException("maxDelay must be greater than or equal to initialDelay.");
        }
        return checkedMaxDelay;
    }

    private enum BackoffType {
        FIXED, EXPONENTIAL, JITTERED
    }
}
