package com.shaft.benchmarks;

import com.shaft.gui.browser.internal.PermissionOrigin;
import com.shaft.gui.internal.aria.AriaSnapshotHelper;
import com.shaft.listeners.internal.ShardPartitioner;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

import java.util.concurrent.TimeUnit;

/**
 * #7009: hot paths hit per element/test (aria-snapshot parsing for structural validation,
 * permission-origin normalization per browser session, shard spec parsing per test).
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(1)
@State(Scope.Benchmark)
public class ParserHotPathBenchmark {
    private static final String ARIA = String.join("\n",
            "- navigation \"Main\":",
            "  - list:",
            "    - listitem:",
            "      - link \"Home\"",
            "    - listitem:",
            "      - link \"Docs\"",
            "- main:",
            "  - heading \"SHAFT\" [level=1]",
            "  - button \"Run\"",
            "  - textbox \"Search\"");

    @Benchmark
    public Object ariaSnapshotParse() {
        return AriaSnapshotHelper.parse(ARIA);
    }

    @Benchmark
    public String permissionOriginNormalize() {
        return PermissionOrigin.normalize("HTTPS://Example.COM:443");
    }

    @Benchmark
    public Object shardSpecParse() {
        return ShardPartitioner.parse(" 3 / 8 ");
    }
}
