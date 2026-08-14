package com.shaft.infrastructure;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;

interface AndroidOwnedProcess {
    long pid();
    Instant startInstant();
    String commandIdentity();
    boolean isAlive();
    void stop(Duration timeout) throws IOException;
}
