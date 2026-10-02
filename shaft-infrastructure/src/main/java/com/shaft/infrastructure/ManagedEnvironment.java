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

    /**
     * Creates a managed environment that runs the given release action once when closed.
     */
    public ManagedEnvironment(SetupProfile profile, SetupReceipt receipt, Optional<URI> endpoint,
                              Map<String, String> connectionProperties, Runnable release) {
        this.profile = Objects.requireNonNull(profile, "profile");
        this.receipt = Objects.requireNonNull(receipt, "receipt");
        this.endpoint = Objects.requireNonNull(endpoint, "endpoint");
        this.connectionProperties = Map.copyOf(Objects.requireNonNull(connectionProperties,
                "connectionProperties"));
        this.release = Objects.requireNonNull(release, "release");
    }

    /**
     * Returns the setup profile this environment was started for.
     */
    public SetupProfile profile() { return profile; }
    /**
     * Returns the receipt of the installation behind this environment.
     */
    public SetupReceipt receipt() { return receipt; }
    /**
     * Returns the service endpoint, when the environment exposes one.
     */
    public Optional<URI> endpoint() { return endpoint; }
    /**
     * Returns the connection properties clients need to use this environment.
     */
    public Map<String, String> connectionProperties() { return connectionProperties; }
    /**
     * Returns whether this environment has been closed.
     */
    public boolean isClosed() { return closed.get(); }

    @Override
    public synchronized void close() {
        if (closed.get()) return;
        release.run();
        closed.set(true);
    }
}
