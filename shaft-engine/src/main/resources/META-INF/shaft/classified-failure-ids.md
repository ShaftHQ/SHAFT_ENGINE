# Classified failure ids

A failure or skip is classified when its message contains a whole-token id and the session started.
`ClassifiedFailureId.accepts` in `com.shaft.tools.io` is the helper. Matching is case-sensitive for
the letters in each token.

## Default tokens

| Token | Meaning | Example |
| --- | --- | --- |
| `DEF-<digits>` | Product defect | `DEF-42` |
| `G<digits>` | Locator gap. A following letter, digit, or hyphen does not match, so `Zephyrus-G14-GA401` is not `G14`. | `G14` |
| `SHAFT#<digits>` | SHAFT engine issue | `SHAFT#5812` |
| `BS-<tokens>` | BrowserStack id made of uppercase letters and digits | `BS-4412` |
| `ADB-<tokens>` | Device id made of uppercase letters and digits | `ADB-OFFLINE` |

The default pattern is:

```text
(?<![A-Za-z0-9])(?:DEF-\d+|G\d+(?![A-Za-z0-9-])|SHAFT#\d+|BS-[A-Z0-9]{1,}(?:-[A-Z0-9]{1,}){0,}|ADB-[A-Z0-9]{1,}(?:-[A-Z0-9]{1,}){0,}|FUNC-[A-Z0-9]{1,}(?:-[A-Z0-9]{1,}){0,}|J\d{2}-BUG-\d+)(?![A-Za-z0-9])
```

## Built-in extension

These two families are part of the published contract and are already in the default pattern:

- `FUNC-<token>` — one or more uppercase or digit segments, for example `FUNC-J01-BUG-03`
- `Jnn-BUG-<digits>` — `J` plus exactly two digits, `-BUG-`, and digits, for example `J01-BUG-03`

## Adding another token

Pass a whole-token `Pattern` to `ClassifiedFailureId.accepts(message, extension)`. The extension is
not used when the message is a never-started session. Example:

```java
Pattern ticket = Pattern.compile("(?<![A-Za-z0-9])TICKET-\\d+(?![A-Za-z0-9])");
ClassifiedFailureId.accepts(message, ticket);
```

## Rejected even when an id is present

- The whole token `BS-QUOTA`
- Any message containing `SessionNotCreated`

An unclassified skip, or a hostname that only looks like `G14`, is not accepted.
