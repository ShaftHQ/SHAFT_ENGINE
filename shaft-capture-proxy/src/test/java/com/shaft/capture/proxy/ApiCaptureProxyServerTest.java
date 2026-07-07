package com.shaft.capture.proxy;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ApiCaptureProxyServerTest {

    @TempDir
    Path tempDir;

    @Test
    void pinnedHostsIsAnImmutableSnapshotMutatedOnlyThroughAddAndRemove() throws Exception {
        CaptureCertificateAuthority certificateAuthority = new CaptureCertificateAuthority(tempDir.resolve("ca"));
        try (ApiCaptureProxyServer proxy = new ApiCaptureProxyServer(
                certificateAuthority, 0, transaction -> { }, warning -> { })) {
            assertTrue(proxy.pinnedHosts().isEmpty());

            proxy.addPinnedHost("pinned.example.test");

            assertEquals(Set.of("pinned.example.test"), proxy.pinnedHosts());
            assertThrows(UnsupportedOperationException.class, () -> proxy.pinnedHosts().add("other.example.test"),
                    "pinnedHosts() must return a read-only snapshot, not the live internal set");

            proxy.removePinnedHost("pinned.example.test");

            assertFalse(proxy.pinnedHosts().contains("pinned.example.test"));
        }
    }
}
