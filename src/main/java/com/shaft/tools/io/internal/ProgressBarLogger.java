package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import me.tongfei.progressbar.ProgressBar;
import me.tongfei.progressbar.ProgressBarBuilder;
import me.tongfei.progressbar.ProgressBarStyle;

import java.time.Duration;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


/**
 * Logs a lightweight terminal progress bar for long-running synchronous operations.
 *
 * <p>The progress bar runs on a virtual thread and is automatically closed when the operation
 * completes via try-with-resources. ANSI styling is disabled in non-interactive runs and when
 * {@code cucumber.ansi-colors.disabled=true} to keep CI logs readable.</p>
 *
 * @see <a href="https://shafthq.github.io/">SHAFT User Guide</a>
 */
public class ProgressBarLogger implements AutoCloseable {
    private static final String ANSI_BRIGHT_CYAN = "\u001b[36;1m";
    private static final String ANSI_RESET = "\u001b[0m";
    private final ExecutorService service;
    ProgressBar pb;
    Runnable task;

    /**
     * Creates a new progress bar logger using the default element identification timeout.
     *
     * @param taskName user-friendly task label shown beside the progress bar
     */
    public ProgressBarLogger(String taskName) {
        this(taskName, (int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout());
    }

    /**
     * Creates a new progress bar logger with a custom timeout.
     *
     * @param taskName   user-friendly task label shown beside the progress bar
     * @param timeoutVal timeout in seconds used as the progress maximum
     */
    public ProgressBarLogger(String taskName, int timeoutVal) {
        String leftBracket = "│";
        String rightBracket = "│";
        if (shouldUseAnsiColors()) {
            leftBracket = ANSI_BRIGHT_CYAN + leftBracket;
            rightBracket = rightBracket + ANSI_RESET;
        }
        ProgressBarBuilder pbb = ProgressBar.builder()
                .setTaskName(taskName)
                .setInitialMax(timeoutVal)
                .setMaxRenderedLength(120)
                .hideEta()
                .setStyle(ProgressBarStyle.builder()
                        .leftBracket(leftBracket)
                        .delimitingSequence("")
                        .rightBracket(rightBracket)
                        .block('█')
                        .space('░')
                        .fractionSymbols(" ▏▎▍▌▋▊▉")
                        .rightSideFractionSymbol('░')
                        .build()
                );
        task = () -> {
            pb = pbb.build();
            pb.setExtraMessage("seconds.");
            boolean interrupted = false;
            while (Duration.ofSeconds(timeoutVal - 1).minus(pb.getElapsedAfterStart()).isPositive()) {
                try {
                    Thread.sleep(Duration.ofSeconds(1));
                } catch (InterruptedException e) {
                    interrupted = handleInterruptedProgressUpdate(e);
                    break;
                }
                pb.step();
                pb.refresh();
            }
            if (!interrupted) {
                pb.stepTo(timeoutVal);
            }
            pb.close();
        };
        service = Executors.newVirtualThreadPerTaskExecutor();
        service.submit(task);
    }

    static boolean handleInterruptedProgressUpdate(InterruptedException interruptedException) {
        Thread.currentThread().interrupt();
        return true;
    }

    static boolean shouldUseAnsiColors() {
        String ansiDisabled = ThreadLocalPropertiesManager.getProperty("cucumber.ansi-colors.disabled");
        boolean hasConsole;
        try {
            hasConsole = System.console() != null;
        } catch (SecurityException ignored) {
            hasConsole = false;
        }
        return (ansiDisabled == null || !Boolean.parseBoolean(ansiDisabled.trim())) && hasConsole;
    }

    /**
     * Stops the background progress task and closes the progress bar stream.
     */
    @Override
    public void close() {
        service.shutdownNow();
    }
}
