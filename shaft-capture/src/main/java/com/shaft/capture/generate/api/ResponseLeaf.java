package com.shaft.capture.generate.api;

/**
 * One classified scalar leaf discovered while walking a recorded JSON response body.
 *
 * @param jsonPath JSONPath-style pointer to this leaf (e.g. {@code $.id}, {@code $.items[0].sku})
 * @param key the JSON property name owning this leaf (empty for array elements with no key)
 * @param value the leaf's textual value
 * @param classification this leaf's classification
 */
public record ResponseLeaf(String jsonPath, String key, String value, LeafClassification classification) {
    public ResponseLeaf {
        jsonPath = jsonPath == null ? "" : jsonPath;
        key = key == null ? "" : key;
        value = value == null ? "" : value;
        classification = classification == null ? LeafClassification.STABLE : classification;
    }

    /**
     * Returns a copy of this leaf upgraded to {@link LeafClassification#CORRELATED}.
     *
     * @return a correlated copy of this leaf
     */
    public ResponseLeaf asCorrelated() {
        return new ResponseLeaf(jsonPath, key, value, LeafClassification.CORRELATED);
    }
}
