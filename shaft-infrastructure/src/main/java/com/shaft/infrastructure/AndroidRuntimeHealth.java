package com.shaft.infrastructure;

import java.io.IOException;
import java.net.URI;
import java.time.Duration;
import java.util.Map;

interface AndroidRuntimeHealth {
    void awaitEmulator(String serial, AndroidRuntimeLayout layout, Map<String, String> environment,
                       Duration timeout) throws IOException;
    void awaitAppium(URI endpoint, Duration timeout) throws IOException;
}
