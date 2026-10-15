package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.time.Duration;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/** #6999: the compact constructor null-checks remoteEndpoint without the self-assignment Error Prone flagged. */
class SetupOptionsNullEndpointTest {
    private static SetupOptions options(Optional<URI> endpoint) {
        return new SetupOptions(SetupProfile.WEB_LOCAL, SetupMode.EXTERNAL, ShaftCachePaths.current(), false, false,
                true, true, Duration.ofMinutes(2), Duration.ofSeconds(30), endpoint);
    }

    @Test
    void nullRemoteEndpointIsRejectedByName() {
        NullPointerException error = assertThrows(NullPointerException.class, () -> options(null));
        assertEquals("remoteEndpoint", error.getMessage());
    }

    @Test
    void emptyRemoteEndpointKeepsRequestedMode() {
        assertEquals(Optional.empty(), options(Optional.empty()).remoteEndpoint());
    }
}
