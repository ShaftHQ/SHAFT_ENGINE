package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import me.tongfei.progressbar.ProgressBar;
import me.tongfei.progressbar.ProgressBarBuilder;
import me.tongfei.progressbar.ProgressBarStyle;

import java.time.Duration;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


public class ProgressBarLogger implements AutoCloseable {
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
                .setMaxRenderedLength(100)
                .hideEta()
                .setStyle(ProgressBarStyle.builder()
                        .leftBracket("\u001b[34;1m│") // the ANSI color code https://gist.github.com/dominikwilkowski/60eed2ea722183769d586c76f22098dd
                        .delimitingSequence("")
                        .rightBracket("│\u001b[0m")
                        .block('█')
                        .space(' ')
                        .fractionSymbols(" ▏▎▍▌▋▊▉")
                        .rightSideFractionSymbol(' ')
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
