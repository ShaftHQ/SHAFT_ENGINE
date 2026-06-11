# SHAFT Capture recording format

`shaft-capture` defines the provider-neutral intermediate representation used
by SHAFT Capture. It records browser intent for review, replay, migration, and
future Java/JUnit/Cucumber generators without collecting browser events itself
and without depending on `shaft-ai`.

Capture creation, validation, serialization, and privacy enforcement work with
`pilot.ai.enabled=false`. Optional AI consumers may receive only the already
redacted representation after the separate Pilot approval checks succeed.

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
