package com.shaft.gui.driver;

import java.nio.file.Path;
import java.time.Instant;
import java.util.Optional;
import java.util.OptionalLong;

/**
 * Backend-neutral handle to one browser download.
 *
 * <p>Metadata that a backend cannot prove is returned as an empty optional. Operations that the live backend cannot
 * perform fail explicitly with {@link UnsupportedOperationException}.</p>
 */
public interface BrowserDownload {
    /** @return browser-suggested filename; never an automatically trusted local path */
    String suggestedFilename();

    /** @return source URL when the backend exposes it */
    Optional<String> url();

    /** @return completed file size in bytes when it can be read without making local-path assumptions */
    OptionalLong size();

    /** @return file creation instant when the backend exposes or permits reading it */
    Optional<Instant> creationTime();

    /** @return file modification instant when the backend exposes or permits reading it */
    Optional<Instant> lastModifiedTime();

    /** @return native failure text after completion, or empty for success/unsupported metadata */
    Optional<String> failure();

    /** Saves or replaces the exact target file, creating its parent directories. */
    BrowserDownload saveAs(Path target);

    /** Cancels this download when the native backend supports per-download cancellation. */
    BrowserDownload cancel();

    /** Deletes this individual download when supported, then returns its owning namespace. */
    DownloadActionsContract delete();

    /** @return this download's owning namespace */
    DownloadActionsContract and();
}
