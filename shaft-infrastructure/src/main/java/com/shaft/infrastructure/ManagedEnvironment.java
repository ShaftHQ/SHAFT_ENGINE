package com.shaft.infrastructure;

import java.net.URI;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

/** An owned, verified runtime plus its connection metadata and idempotent release action. */
public final class ManagedEnvironment implements AutoCloseable {
    private final SetupProfile profile;
    private final SetupReceipt receipt;
    private final Optional<URI> endpoint;
    private final Map<String, String> connectionProperties;
    private final Runnable release;
    private final AtomicBoolean closed = new AtomicBoolean();

    public ManagedEnvironment(SetupProfile profile, SetupReceipt receipt, Optional<URI> endpoint,
                              Map<String, String> connectionProperties, Runnable release) {
        this.profile = Objects.requireNonNull(profile, "profile");
        this.receipt = Objects.requireNonNull(receipt, "receipt");
        this.endpoint = Objects.requireNonNull(endpoint, "endpoint");
        this.connectionProperties = Map.copyOf(Objects.requireNonNull(connectionProperties,
                "connectionProperties"));
        this.release = Objects.requireNonNull(release, "release");
    }

    public SetupProfile profile() { return profile; }
    public SetupReceipt receipt() { return receipt; }
    public Optional<URI> endpoint() { return endpoint; }
    public Map<String, String> connectionProperties() { return connectionProperties; }
    public boolean isClosed() { return closed.get(); }

    @Override
    public synchronized void close() {
        if (closed.get()) return;
        release.run();
        closed.set(true);
    }
}
