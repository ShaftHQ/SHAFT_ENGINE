package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

interface AndroidRuntimeController {
    AndroidOwnedProcess start(String role, List<String> command, Path workingDirectory,
                              Map<String, String> environment, Set<String> removedEnvironment,
                              Path log) throws IOException;

    Optional<AndroidOwnedProcess> find(long pid, Instant startInstant, String commandIdentity) throws IOException;
}
