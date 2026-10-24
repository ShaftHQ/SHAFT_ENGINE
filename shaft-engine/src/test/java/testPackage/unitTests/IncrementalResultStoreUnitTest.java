package testPackage.unitTests;

import com.shaft.tools.io.internal.IncrementalResultStore;
import com.shaft.tools.io.internal.IncrementalResultStore.ValidityKey;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

public class IncrementalResultStoreUnitTest {
    private static final ValidityKey KEY = new ValidityKey("sha1", "app1", "files1", "10.4");

    private static void result(Path dir, String uuid, String history, String status, long start) throws IOException {
        Files.createDirectories(dir);
        Files.writeString(dir.resolve(uuid + "-result.json"), """
                {"uuid":"%s","historyId":"%s","status":"%s","start":%d,
                 "labels":[{"name":"testClass","value":"pkg.LoginTest"}],
                 "steps":[{"attachments":[{"source":"%s-attachment.png"}]}]}""".replace("%s", "\u0000")
                .replaceFirst("\u0000", uuid).replaceFirst("\u0000", history).replaceFirst("\u0000", status)
                .replace("%d", String.valueOf(start)).replaceFirst("\u0000", uuid), StandardCharsets.UTF_8);
        Files.writeString(dir.resolve(uuid + "-attachment.png"), "png", StandardCharsets.UTF_8);
        Files.writeString(dir.resolve("c" + uuid + "-container.json"), "{\"children\":[\"" + uuid + "\"]}", StandardCharsets.UTF_8);
    }

    @Test
    public void passIsStoredWithAttachmentContainerAndManifestThenMerged() throws IOException {
        Path tmp = Files.createTempDirectory("irs");
        Path results = tmp.resolve("results");
        result(results, "u1", "h1", "passed", 100);
        Path store = tmp.resolve("store");
        Assert.assertEquals(IncrementalResultStore.record(results, store, 50, c -> KEY), 1);
        Path testDir = store.resolve("h1");
        Assert.assertTrue(Files.exists(testDir.resolve("u1-result.json")));
        Assert.assertTrue(Files.exists(testDir.resolve("u1-attachment.png")));
        Assert.assertTrue(Files.exists(testDir.resolve("cu1-container.json")));
        Assert.assertTrue(Files.readString(testDir.resolve("manifest.json")).contains("files1"));
        Path out = tmp.resolve("out");
        var merge = IncrementalResultStore.merge(store, out, c -> KEY);
        Assert.assertEquals(merge.merged(), java.util.List.of("h1"));
        Assert.assertEquals(merge.exitCode(), IncrementalResultStore.MERGE_OK);
        Assert.assertTrue(Files.exists(out.resolve("u1-attachment.png")));
        Assert.assertFalse(Files.exists(out.resolve("manifest.json")));
    }

    @Test
    public void leftoverResultsFromBeforeRunStartAreIgnored() throws IOException {
        Path tmp = Files.createTempDirectory("irs");
        result(tmp.resolve("results"), "u1", "h1", "passed", 10);
        Assert.assertEquals(IncrementalResultStore.record(tmp.resolve("results"), tmp.resolve("store"), 50, c -> KEY), 0);
        Assert.assertFalse(Files.exists(tmp.resolve("store").resolve("h1")));
    }

    @Test
    public void changedFilesHashMakesPassStale() throws IOException {
        Path tmp = Files.createTempDirectory("irs");
        result(tmp.resolve("results"), "u1", "h1", "passed", 100);
        IncrementalResultStore.record(tmp.resolve("results"), tmp.resolve("store"), 0, c -> KEY);
        var merge = IncrementalResultStore.merge(tmp.resolve("store"), tmp.resolve("out"),
                c -> new ValidityKey("sha2", "app1", "files2", "10.4"));
        Assert.assertEquals(merge.stale(), java.util.List.of("h1"));
        Assert.assertEquals(merge.exitCode(), IncrementalResultStore.MERGE_STALE);
    }

    @Test
    public void newerFailureOnSameHashFlagsPassAndDoesNotOverwriteIt() throws IOException {
        Path tmp = Files.createTempDirectory("irs");
        Path store = tmp.resolve("store");
        result(tmp.resolve("run1"), "u1", "h1", "passed", 100);
        IncrementalResultStore.record(tmp.resolve("run1"), store, 0, c -> KEY);
        result(tmp.resolve("run2"), "u2", "h1", "failed", 200);
        IncrementalResultStore.record(tmp.resolve("run2"), store, 0, c -> KEY);
        Assert.assertTrue(Files.exists(store.resolve("h1").resolve("u1-result.json")));
        Assert.assertTrue(Files.exists(store.resolve("h1").resolve("failures.jsonl")));
        var merge = IncrementalResultStore.merge(store, tmp.resolve("out"), c -> KEY);
        Assert.assertEquals(merge.flagged(), java.util.List.of("h1"));
        Assert.assertEquals(merge.exitCode(), IncrementalResultStore.MERGE_FLAGGED);
    }

    @Test
    public void filesHashChangesWithContentAndIgnoresMissingFiles() throws IOException {
        Path root = Files.createTempDirectory("irs");
        Files.writeString(root.resolve("pom.xml"), "a");
        String first = IncrementalResultStore.filesHash(root, Set.of(Path.of("pom.xml"), Path.of("missing.java")));
        Files.writeString(root.resolve("pom.xml"), "b");
        Assert.assertNotEquals(IncrementalResultStore.filesHash(root, Set.of(Path.of("pom.xml"))), first);
        Assert.assertEquals(IncrementalResultStore.filesMatching(root, "pom.xml"), Set.of(Path.of("pom.xml")));
    }

    @Test
    public void incrementalStorePropertiesAreOptInAndSettable() {
        var allure = com.shaft.driver.SHAFT.Properties.allure;
        Assert.assertFalse(allure.incrementalStore());
        Assert.assertFalse(allure.incrementalStoreMerge());
        Assert.assertEquals(allure.incrementalStoreJourney(), "default");
        Assert.assertTrue(allure.incrementalStoreHashGlobs().contains("pom.xml"));
        allure.set().incrementalStore(true).incrementalStoreMerge(true).incrementalStoreJourney("login");
        try {
            Assert.assertTrue(com.shaft.driver.SHAFT.Properties.allure.incrementalStore());
            Assert.assertTrue(com.shaft.driver.SHAFT.Properties.allure.incrementalStoreMerge());
            Assert.assertEquals(com.shaft.driver.SHAFT.Properties.allure.incrementalStoreJourney(), "login");
        } finally {
            com.shaft.driver.SHAFT.Properties.allure.set().incrementalStore(false).incrementalStoreMerge(false)
                    .incrementalStoreJourney("default");
        }
    }

    @Test
    public void hookRecordsAndMergesWithoutFailingTheRun() throws IOException {
        var allure = com.shaft.driver.SHAFT.Properties.allure;
        com.shaft.tools.io.internal.IncrementalResultStoreHook.afterExecution(0);
        Path results = Path.of(com.shaft.driver.SHAFT.Properties.paths.allureResults());
        String uuid = "irs-hook-" + System.nanoTime();
        long start = System.currentTimeMillis();
        result(results, uuid, uuid, "passed", start + 1);
        allure.set().incrementalStore(true).incrementalStoreMerge(true).incrementalStoreJourney("unit-hook");
        try {
            com.shaft.tools.io.internal.IncrementalResultStoreHook.afterExecution(start);
            Path stored = Path.of("target", "passed-results", "unit-hook", uuid);
            Assert.assertTrue(Files.exists(stored.resolve("manifest.json")));
            Assert.assertTrue(Files.readString(stored.resolve("manifest.json")).contains("filesHash"));
        } finally {
            allure.set().incrementalStore(false).incrementalStoreMerge(false).incrementalStoreJourney("default");
            for (String name : new String[]{uuid + "-result.json", uuid + "-attachment.png", "c" + uuid + "-container.json"}) {
                Files.deleteIfExists(results.resolve(name));
            }
        }
    }

    @Test
    public void edgeCasesAreHandled() throws IOException {
        Path tmp = Files.createTempDirectory("irs");
        Assert.assertEquals(IncrementalResultStore.merge(tmp.resolve("none"), tmp.resolve("out"), c -> KEY).exitCode(),
                IncrementalResultStore.MERGE_OK);
        Assert.assertEquals(IncrementalResultStore.record(tmp.resolve("none"), tmp.resolve("store"), 0, c -> KEY), 0);
        Assert.assertTrue(IncrementalResultStore.filesMatching(tmp, " , ").isEmpty());
        Assert.assertTrue(IncrementalResultStore.filesMatching(tmp.resolve("none"), "**").isEmpty());
        Files.createDirectories(tmp.resolve("store").resolve("no-manifest"));
        Assert.assertTrue(IncrementalResultStore.merge(tmp.resolve("store"), tmp.resolve("out"), c -> KEY).merged().isEmpty());
        result(tmp.resolve("results"), "u1", "h1", "broken", 100);
        IncrementalResultStore.record(tmp.resolve("results"), tmp.resolve("store"), 0, c -> KEY);
        result(tmp.resolve("results2"), "u2", "h1", "passed", 50);
        IncrementalResultStore.record(tmp.resolve("results2"), tmp.resolve("store"), 0, c -> KEY);
        Assert.assertTrue(Files.exists(tmp.resolve("store").resolve("h1").resolve("failures.jsonl")));
        Assert.assertEquals(IncrementalResultStore.merge(tmp.resolve("store"), tmp.resolve("out"), c -> KEY).flagged(),
                java.util.List.of("h1"));
    }
}
