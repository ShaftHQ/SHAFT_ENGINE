package com.shaft.capture.storage;

import com.shaft.capture.format.CaptureFormatException;
import com.shaft.capture.model.network.BodyRef;

import java.io.IOException;
import java.nio.file.AccessDeniedException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

/**
 * Persists request/response bodies as external files with SHA-256 integrity and optional truncation.
 * Mirrors ExternalTestDataWriter's atomic temp-file+move pattern.
 */
public final class NetworkBodyStore {
    private static final int DEFAULT_MAX_BODY_BYTES = 1048576; // 1 MiB
    private static final HexFormat HEX = HexFormat.of().withUpperCase();

    /**
     * Stores a body as an external file and returns a BodyRef with integrity metadata.
     * The stored file name is derived from the SHA-256 of the original full body.
     *
     * @param body raw body bytes (may be null or empty)
     * @param contentType optional media/content-type of the body, recorded as the BodyRef encoding
     * @param sessionDir directory where the body file will be stored (session-relative)
     * @return BodyRef with a ref to the stored file, sha256/size of original body, its encoding,
     *         and truncation flag, or null if body is null/empty
     * @throws CaptureFormatException if write fails
     */
    public BodyRef store(byte[] body, String contentType, Path sessionDir) {
        return store(body, contentType, sessionDir, DEFAULT_MAX_BODY_BYTES);
    }

    /**
     * Stores a body as an external file with a configurable size cap and returns a BodyRef.
     *
     * @param body raw body bytes (may be null or empty)
     * @param contentType optional media/content-type of the body, recorded as the BodyRef encoding
     * @param sessionDir directory where the body file will be stored (session-relative)
     * @param maxBodyBytes maximum bytes to store (original body size is always recorded)
     * @return BodyRef with a ref to the stored file, sha256/size of original full body, its
     *         encoding, and truncation flag, or null if body is null/empty
     * @throws CaptureFormatException if write fails
     */
    public BodyRef store(byte[] body, String contentType, Path sessionDir, int maxBodyBytes) {
        // Empty/null bodies produce no file and return null
        if (body == null || body.length == 0) {
            return null;
        }

        try {
            // Compute SHA-256 of the original full body
            String sha256 = computeSha256(body);
            long originalSize = body.length;
            boolean truncated = originalSize > maxBodyBytes;

            // Determine how much to actually write
            int bytesToWrite = (int) Math.min(originalSize, maxBodyBytes);
            byte[] dataToWrite = truncated ? new byte[bytesToWrite] : body;
            if (truncated) {
                System.arraycopy(body, 0, dataToWrite, 0, bytesToWrite);
            }

            // Write atomically using temp file + move pattern (like ExternalTestDataWriter)
            String filename = sha256 + ".bin";
            Path storagePath = sessionDir.resolve(filename);
            atomicWrite(storagePath, dataToWrite);

            return new BodyRef(filename, sha256, originalSize, contentType, truncated);
        } catch (IOException | NoSuchAlgorithmException exception) {
            throw new CaptureFormatException("Network body could not be stored atomically.", exception);
        }
    }

    /**
     * Computes the SHA-256 hash of a byte array.
     *
     * @param data bytes to hash
     * @return hexadecimal SHA-256 hash (uppercase)
     * @throws NoSuchAlgorithmException if SHA-256 is not available
     */
    private String computeSha256(byte[] data) throws NoSuchAlgorithmException {
        MessageDigest digest = MessageDigest.getInstance("SHA-256");
        digest.update(data);
        return HEX.formatHex(digest.digest());
    }

    /**
     * Atomically writes bytes to a destination file using a temporary file pattern.
     * Mirrors ExternalTestDataWriter.atomicWrite() but for binary data.
     *
     * @param destination target file path
     * @param data bytes to write
     * @throws IOException if write fails
     */
    private static void atomicWrite(Path destination, byte[] data) throws IOException {
        Path absolute = destination.toAbsolutePath().normalize();
        Path temporary = null;
        try {
            Files.createDirectories(absolute.getParent());
            temporary = Files.createTempFile(absolute.getParent(), "." + absolute.getFileName(), ".tmp");
            Files.write(temporary, data);
            moveReplacing(temporary, absolute);
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort cleanup of an unpublished temporary artifact.
                }
            }
        }
    }

    /**
     * Moves a file atomically if supported, falling back with retries on access denied.
     * Mirrors ExternalTestDataWriter.moveReplacing().
     *
     * @param temporary source file
     * @param destination target file
     * @throws IOException if move fails
     */
    private static void moveReplacing(Path temporary, Path destination) throws IOException {
        boolean atomic = true;
        for (int attempt = 1; attempt <= 50; attempt++) {
            try {
                if (atomic) {
                    Files.move(temporary, destination, StandardCopyOption.ATOMIC_MOVE,
                            StandardCopyOption.REPLACE_EXISTING);
                } else {
                    Files.move(temporary, destination, StandardCopyOption.REPLACE_EXISTING);
                }
                return;
            } catch (AtomicMoveNotSupportedException ignored) {
                atomic = false;
            } catch (AccessDeniedException exception) {
                if (attempt == 50) {
                    throw exception;
                }
                try {
                    Thread.sleep(10);
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while publishing network body.", interrupted);
                }
            }
        }
    }
}
