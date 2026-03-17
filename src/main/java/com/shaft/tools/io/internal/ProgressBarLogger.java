package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import me.tongfei.progressbar.ProgressBar;
import me.tongfei.progressbar.ProgressBarBuilder;
import me.tongfei.progressbar.ProgressBarStyle;

import java.time.Duration;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


public class ProgressBarLogger implements AutoCloseable {
    private static final String ANSI_BRIGHT_CYAN = "\u001b[36;1m";
    private static final String ANSI_RESET = "\u001b[0m";
    private final ExecutorService service;
    ProgressBar pb;
    Runnable task;

    public ProgressBarLogger(String taskName) {
        this(taskName, (int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout());
    }

    public ProgressBarLogger(String taskName, int timeoutVal) {
        ProgressBarBuilder pbb = ProgressBar.builder()
                .setTaskName(taskName)
                .setInitialMax(timeoutVal)
                .setMaxRenderedLength(120)
                .hideEta()
                .setStyle(ProgressBarStyle.builder()
                        .leftBracket(ANSI_BRIGHT_CYAN + "│")
                        .delimitingSequence("")
                        .rightBracket("│" + ANSI_RESET)
                        .block('█')
                        .space('░')
                        .fractionSymbols(" ▏▎▍▌▋▊▉")
                        .rightSideFractionSymbol('░')
                        .build()
                );
        task = () -> {
            pb = pbb.build();
            pb.setExtraMessage("seconds.");
            while (Duration.ofSeconds(timeoutVal - 1).minus(pb.getElapsedAfterStart()).isPositive()) {
                try {
                    Thread.sleep(Duration.ofSeconds(1));
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                pb.step();
                pb.refresh();
            }
            pb.stepTo(timeoutVal);
            pb.close();
        };
        service = Executors.newVirtualThreadPerTaskExecutor();
        service.submit(task);
    }

    @Override
    public void close() {
        service.shutdownNow();
    }
}
