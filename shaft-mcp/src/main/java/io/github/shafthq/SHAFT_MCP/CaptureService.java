package io.github.shafthq.SHAFT_MCP;

import com.shaft.capture.runtime.CaptureBrowser;
import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStartRequest;
import com.shaft.capture.runtime.CaptureStatus;
import jakarta.annotation.PreDestroy;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.nio.file.Path;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

/**
 * MCP adapter for deterministic managed-browser SHAFT Capture recording.
 */
@Service
public class CaptureService {
    private static final DateTimeFormatter FILE_TIME = DateTimeFormatter
            .ofPattern("yyyyMMdd-HHmmss")
            .withZone(ZoneOffset.UTC);
    private static final Path RUNTIME_DIRECTORY = Path.of("target", "shaft-capture-mcp");

    private final CaptureManager manager = new CaptureManager();

    /**
     * Launches a fresh SHAFT-managed browser and starts deterministic capture.
     *
     * @param targetUrl initial http, https, or file URL
     * @param browser Chrome or Edge; blank selects Chrome
     * @param outputPath capture JSON path; blank selects a timestamped recording
     * @param headless whether to launch without a visible window
     * @return safe recorder status
     */
    @Tool(name = "capture_start",
            description = "starts a privacy-safe SHAFT managed-browser recording with no AI provider")
    public CaptureStatus start(
            String targetUrl,
            String browser,
            String outputPath,
            boolean headless) {
        Path output = outputPath == null || outputPath.isBlank()
                ? Path.of("recordings", "capture-" + FILE_TIME.format(Instant.now()) + ".json")
                : Path.of(outputPath);
        return manager.start(new CaptureStartRequest(
                targetUrl,
                browser == null || browser.isBlank()
                        ? CaptureBrowser.CHROME
                        : CaptureBrowser.parse(browser),
                output,
                RUNTIME_DIRECTORY,
                headless));
    }

    /**
     * Returns safe recorder status without captured values.
     *
     * @return current or final recorder status
     */
    @Tool(name = "capture_status",
            description = "returns SHAFT Capture session, browser, URL, event count, warnings, and output status")
    public CaptureStatus status() {
        return manager.status();
    }

    /**
     * Stops the active recording.
     *
     * @param discard whether to delete capture artifacts after shutdown
     * @return final recorder status
     */
    @Tool(name = "capture_stop",
            description = "stops SHAFT Capture and optionally discards the local recording")
    public CaptureStatus stop(boolean discard) {
        return manager.stop(discard);
    }

    /**
     * Preserves an incomplete session when the MCP server shuts down unexpectedly.
     */
    @PreDestroy
    void close() {
        manager.close();
    }
}
