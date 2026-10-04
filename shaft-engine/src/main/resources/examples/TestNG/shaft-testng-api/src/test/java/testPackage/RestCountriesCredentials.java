package testPackage;

final class RestCountriesCredentials {
    private RestCountriesCredentials() {
        // utility class
    }

    /**
     * Returns the REST Countries API key from the {@code restCountries.apiKey} property
     * (set in {@code src/main/resources/properties/secrets.properties} or via {@code -D}),
     * falling back to the {@code RESTCOUNTRIES_API_KEY} environment variable.
     *
     * @return the configured API key
     * @throws IllegalStateException if the key is not configured
     */
    static String apiKey() {
        String apiKey = firstNonBlank(
                System.getProperty("restCountries.apiKey"),
                System.getenv("RESTCOUNTRIES_API_KEY"));
        if (apiKey == null) {
            throw new IllegalStateException("REST Countries API key is not configured. Set restCountries.apiKey in "
                    + "src/main/resources/properties/secrets.properties, pass -DrestCountries.apiKey, or set the "
                    + "RESTCOUNTRIES_API_KEY environment variable. See "
                    + "src/main/resources/properties/secrets.properties.example for details.");
        }
        return apiKey;
    }

    private static String firstNonBlank(String... candidates) {
        for (String candidate : candidates) {
            if (candidate != null && !candidate.isBlank()) {
                return candidate;
            }
        }
        return null;
    }
}
