# SHAFT Capture

`shaft-capture` defines the provider-neutral intermediate representation used
by SHAFT Capture and the managed-browser recorder that produces it. It records
browser intent for review, replay, migration, and future Java/JUnit/Cucumber
generators without depending on `shaft-ai`.

Capture creation, validation, serialization, and privacy enforcement work with
`pilot.ai.enabled=false`. Optional AI consumers may receive only the already
redacted representation after the separate Pilot approval checks succeed.

## Managed browser recording

The recorder launches a fresh SHAFT-managed Chrome or Edge session. Firefox is
rejected with an explicit unsupported-browser message until equivalent event
coverage is available. WebDriver BiDi supplies navigation, browsing-context,
prompt, and preload-script signals when available. A JavaScript listener
drained through ordinary WebDriver provides deterministic interaction capture
and remains the compatibility fallback.

Build the executable MCP JAR, then use its `capture` subcommand:

```bash
mvn -pl shaft-mcp -am package -DskipTests -Dgpg.skip
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar capture start \
  --url https://example.test --browser chrome \
  --output recordings/example.json --headless
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar capture status
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar capture checkpoint \
  --description "Checkout ready"
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar capture stop
```

Use `--runtime-dir <path>` on every command to isolate control files. `stop`
also accepts `--discard`. Only one recorder may own a runtime directory at a
time. The daemon control endpoint is bound to loopback, requires a generated
bearer token, and removes its token and descriptor at shutdown. Browser
profiles are temporary and removed after normal stop or interruption.

The same lifecycle is exposed by the `capture_start`, `capture_status`, and
`capture_stop` MCP tools. Status contains safe metadata and counts, never typed
values.

All process arguments and filesystem paths are built with Java APIs
(`ProcessBuilder`, `Path`, and `Files`). No Windows, POSIX shell, or path
separator is assumed; restrictive POSIX permissions are applied when supported
and otherwise the host filesystem's inherited permissions are used.

## Format

Every session has a `schemaVersion`, safe session and browser metadata, ordered
events and checkpoints, external test-data references, a redaction summary, and
explicit extension maps. The current version is `1.0`; readers migrate the
synthetic `0.9` format and reject unsupported versions with an actionable
message.

The event hierarchy covers:

- navigation, click, type, clear, select, check/uncheck, and upload;
- keyboard, window/tab, frame, alert, and explicit wait operations;
- explicit verification events and replay status.

`ElementSnapshot` retains sanitized role, accessible name, label, normalized
attributes, visibility state, and `LocatorCandidate` evidence. Candidate scores
are deterministic inputs based on strategy, uniqueness, visibility, stability,
and recorded signals. No model inference is used to rank locators.

The bundled schema is:

`shaft-capture/src/main/resources/schema/shaft-capture-session-1.0.schema.json`

`CaptureJsonCodec` validates before read and write, emits stable human-readable
JSON, preserves explicit extension fields, and never publishes a partially
validated recording.

## Privacy boundary

`CapturePrivacyClassifier` runs before values enter a `CaptureSession`.
Passwords, tokens, configured sensitive fields/selectors/attributes/URL
parameters, and configured value patterns produce named environment or secret
references with no original value. Ordinary typed data is externalized to
`capture-data.json` by default through `ExternalTestDataWriter`.

Upload events store a logical fixture reference, sanitized basename, media type,
and size. They never retain an arbitrary absolute user path or file contents.
Evidence references accept only safe relative paths. Cookies, storage, headers,
page source, screenshots, and other evidence are absent unless a later
collector explicitly enables a documented category.

The persisted redaction summary contains only counts and rule names. It does not
contain removed values.

## Lifecycle

`CaptureSessionStore` provides thread-safe start, append, checkpoint,
interruption, stop, and read operations. Each update serializes and validates a
complete snapshot before an atomic replacement. In-progress or crashed sessions
remain readable with status `INCOMPLETE`; a normal stop records `COMPLETED` and
an end timestamp.

Example:

```java
var session = CaptureSession.start(
        "checkout-recording",
        Instant.now(),
        browserMetadata);
var store = new CaptureSessionStore(Path.of("recordings/checkout.json"));
store.start(session);
store.append(captureEvent);
store.stop(Instant.now());
```

Run the focused suite with:

```bash
mvn -pl shaft-capture -am test
```

The real-browser Chrome and Edge suite is opt-in:

```bash
mvn -pl shaft-capture -am test \
  -DincludeCaptureBrowserE2E=true \
  -Dtest=ManagedCaptureRecorderBrowserTest
```
