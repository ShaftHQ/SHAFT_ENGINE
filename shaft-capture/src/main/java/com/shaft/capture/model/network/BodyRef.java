package com.shaft.capture.model.network;

/**
 * Safe reference to a request or response body. The original body content is
 * never retained in this model; only a stable reference and safe metadata are kept.
 *
 * @param ref stable reference identifier or relative path to externalized body content
 * @param sha256 content digest of the referenced body
 * @param sizeBytes size of the referenced body in bytes
 * @param encoding transfer or content encoding of the referenced body
 * @param truncated whether the referenced body was truncated when captured
 */
public record BodyRef(
        String ref,
        String sha256,
        long sizeBytes,
        String encoding,
        boolean truncated) {
    /**
     * Creates an immutable body reference.
     */
    public BodyRef {
        ref = NetworkModelSupport.text(ref);
        sha256 = NetworkModelSupport.text(sha256);
        encoding = NetworkModelSupport.text(encoding);
        if (sizeBytes < 0) {
            throw new IllegalArgumentException("Body size cannot be negative.");
        }
    }
}
