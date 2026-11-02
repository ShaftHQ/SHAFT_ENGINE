package com.shaft.fuzz;

import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Replays the Jazzer targets (#7012) on fixed seeds plus deterministic random bytes, so the PR gate
 * proves each harness runs; the weekly fuzz workflow explores beyond these seeds.
 */
public class ParserFuzzSeedsUnitTest {
    private static final String[] SEEDS = {
        "", " ", "3/8", " 1 / 2 ", "0/0", "9/3", "99999999999/1", "a/b",
        "https://example.com", "HTTP://EXAMPLE.com:80", "https://[::1]:8443", "http://user@host", "file:///x",
        "- button \"OK\"", "- list:\n  - listitem \"a\"", "- : :", "[1, 2", "!!java.lang.Object {}", "- 42", "{a: b}",
    };

    @DataProvider(name = "inputs")
    public Object[][] inputs() {
        List<byte[]> inputs = new ArrayList<>();
        for (String seed : SEEDS) {
            inputs.add(seed.getBytes(StandardCharsets.UTF_8));
        }
        Random random = new Random(7012L);
        for (int i = 0; i < 200; i++) {
            byte[] bytes = new byte[random.nextInt(48)];
            random.nextBytes(bytes);
            inputs.add(bytes);
        }
        Object[][] rows = new Object[inputs.size()][];
        for (int i = 0; i < rows.length; i++) {
            rows[i] = new Object[] {inputs.get(i)};
        }
        return rows;
    }

    @Test(dataProvider = "inputs")
    public void shardSpec(byte[] data) {
        ShardSpecFuzzer.fuzzerTestOneInput(data);
    }

    @Test(dataProvider = "inputs")
    public void permissionOrigin(byte[] data) {
        PermissionOriginFuzzer.fuzzerTestOneInput(data);
    }

    @Test(dataProvider = "inputs")
    public void ariaSnapshot(byte[] data) {
        AriaSnapshotFuzzer.fuzzerTestOneInput(data);
    }
}
