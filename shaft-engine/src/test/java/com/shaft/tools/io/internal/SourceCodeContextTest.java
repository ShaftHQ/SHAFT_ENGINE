package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class SourceCodeContextTest {
    @Test
    public void neverDoesNotResolveLifecycleOrReadSource() {
        com.shaft.driver.SHAFT.Properties.reporting.set().whenToAttachSourceCode("Never");
        try (var source = org.mockito.Mockito.mockStatic(SourceCodeContext.class)) {
            SourceCodeAttachment collector = new SourceCodeAttachment();
            var step = new io.qameta.allure.model.StepResult().setStatus(io.qameta.allure.model.Status.FAILED);
            collector.started(step, () -> { throw new AssertionError("Never must not resolve the lifecycle"); });
            collector.stopping(step);
            source.verifyNoInteractions();
        } finally {
            com.shaft.properties.internal.Properties.clearForCurrentThread();
        }
    }

    @Test
    public void boundedWindowClampsToFirstAndLastLinesAndRejectsInvalidLine() throws Exception {
        Path source = Files.createTempFile("source-window", ".java");
        try {
            Files.writeString(source, "first\nsecond\nthird\nfourth\nfifth\nsixth\n");
            Assert.assertEquals(SourceCodeContext.read(source, 1).lines().size(), 3);
            Assert.assertEquals(SourceCodeContext.read(source, 3).lines().size(), 5);
            Assert.assertEquals(SourceCodeContext.read(source, 6).lines().size(), 3);
            Assert.assertEquals(SourceCodeContext.read(source, 3).lines().get(2).text(), "third");
            Assert.assertTrue(SourceCodeContext.read(source, 7).lines().isEmpty());
            Assert.assertTrue(SourceCodeContext.read(source, -1).lines().isEmpty());
            Assert.assertTrue(SourceCodeContext.read(null, 1).lines().isEmpty());
            Files.writeString(source, "");
            Assert.assertTrue(SourceCodeContext.read(source, 1).lines().isEmpty());
        } finally {
            Files.deleteIfExists(source);
        }
    }

    @Test
    public void oversizedSourceIsOmittedWithoutPublishingPartialLines() throws Exception {
        Path source = Files.createTempFile("source-size", ".java");
        try {
            Files.writeString(source, "x".repeat(17_000));
            var window = SourceCodeContext.read(source, 1);
            Assert.assertTrue(window.lines().isEmpty());
            Assert.assertTrue(window.unavailableReason().contains("size limit"));
        } finally {
            Files.deleteIfExists(source);
        }
    }

    @Test
    public void relativeFallsBackWhenPathHasNoFileName() {
        Path root = Path.of("/").toAbsolutePath().normalize();
        // Root paths have a null file name on Unix; relative must not NPE.
        String relative = SourceCodeContext.relative(root, null);
        Assert.assertFalse(relative.isEmpty());
        Assert.assertFalse(relative.contains("null"));
    }

    @Test
    public void renderEscapesLocationAndShowsRecordedLineWhenSnippetMissing() {
        var frame = new StackTraceElement("customer.Test", "execute", "Test.java", 7);
        String location = "src/<evil>&" + "\"" + "'" + "/Test.java";
        String html = SourceCodeAttachment.render(frame, location, SourceCodeContext.read(null, 7));
        Assert.assertTrue(html.contains("Recorded line: 7"), html);
        Assert.assertTrue(html.contains("&lt;evil&gt;"), html);
        Assert.assertTrue(html.contains("&amp;"), html);
        Assert.assertFalse(html.contains("<evil>"), html);
        Assert.assertTrue(html.contains("customer.Test.execute"), html);
    }

    @Test
    public void sourceMetadataResolvesNestedClassButCannotTraversePaths() {
        var nested = new StackTraceElement(getClass().getName() + "$Nested", "call", "SourceCodeContextTest.java", 1);
        Path source = java.util.Objects.requireNonNull(SourceCodeContext.findSourceFile(nested, getClass()),
                "Nested-class source must resolve to its containing Java file.");
        String relative = SourceCodeContext.relative(source, getClass());
        Assert.assertTrue(relative.contains("src/test/java/com/shaft/tools/io/internal/SourceCodeContextTest.java"), relative);
        Assert.assertFalse(Path.of(relative).isAbsolute());
        var traversal = new StackTraceElement(getClass().getName(), "call", "../SourceCodeContextTest.java", 1);
        Assert.assertNull(SourceCodeContext.findSourceFile(traversal, getClass()));
        var kotlin = new StackTraceElement("customer.Test", "call", "Test.kt", 1);
        Assert.assertNull(SourceCodeContext.findSourceFile(kotlin, getClass()));
    }

    @Test
    public void renderedSnippetRedactsSecretsBeforeEscapingMarkup() {
        String credential = "source-context-test-private-value";
        FailureTraceReporter.registerSensitiveSourceValue(credential);
        ReportSecrets.registerValue("source-context-property-secret");
        try {
            var frame = new StackTraceElement("customer.Test", "execute", "Test.java", 42);
            var window = new SourceCodeContext.Window(List.of(
                    new SourceCodeContext.Line(41, "String value = \"" + credential + "\";"),
                    new SourceCodeContext.Line(42, "call(\"<script>alert('x')</script>\");"),
                    new SourceCodeContext.Line(43, "String other = \"source-context-property-secret\";")), "");
            String html = SourceCodeAttachment.render(frame, "src/test/java/customer/Test.java", window);
            Assert.assertFalse(html.contains(credential));
            Assert.assertFalse(html.contains("source-context-property-secret"));
            Assert.assertFalse(html.contains("<script>"));
            Assert.assertTrue(html.contains("&lt;script&gt;"));
            Assert.assertTrue(html.contains("class=\"source-line active\""));
            Assert.assertEquals(html.split("&#8594;", -1).length - 1, 1);
            Assert.assertFalse(html.contains("file:"));
        } finally {
            FailureTraceReporter.clearSensitiveValues();
        }
    }

    @Test
    public void unavailableSourceShowsKnownLocationWithoutInventingSnippet() {
        var frame = new StackTraceElement("customer.Test", "execute", "Test.java", -1);
        String html = SourceCodeAttachment.render(frame, "Test.java", SourceCodeContext.read(null, -1));
        Assert.assertTrue(html.contains("customer.Test.execute"));
        Assert.assertTrue(html.contains("metadata is unavailable"));
        Assert.assertFalse(html.contains("Recorded line:"));
        Assert.assertFalse(html.contains("class=\"source-line active\""));
    }

    @Test
    public void unsafeShortCredentialOmitsTheWholeSourceWindow() {
        FailureTraceReporter.registerSensitiveSourceValue("xy");
        try {
            var frame = new StackTraceElement("customer.Test", "execute", "Test.java", 2);
            var window = new SourceCodeContext.Window(List.of(new SourceCodeContext.Line(1, "before();"),
                    new SourceCodeContext.Line(2, "enter(\"xy\");"), new SourceCodeContext.Line(3, "after();")), "");
            String html = SourceCodeAttachment.render(frame, "Test.java", window);
            Assert.assertTrue(html.contains("Source context omitted"));
            Assert.assertFalse(html.contains("before();"));
            Assert.assertFalse(html.contains("enter("));
            Assert.assertFalse(html.contains("source-line active"));
        } finally {
            FailureTraceReporter.clearSensitiveValues();
        }
    }

    @Test
    public void totalReadLimitAppliesBeforeTheSelectedWindow() throws Exception {
        Path source = Files.createTempFile("source-read-budget", ".java");
        try {
            Files.writeString(source, "x".repeat(1_000_001) + "\nsecond\nthird\nfourth");
            var window = SourceCodeContext.read(source, 4);
            Assert.assertTrue(window.lines().isEmpty(), "A distant line must not bypass the total read budget.");
            Assert.assertTrue(window.unavailableReason().contains("read limit"));
        } finally {
            Files.deleteIfExists(source);
        }
    }

    @Test
    public void crlfAndFinalLineWithoutNewlinePreserveSourceText() throws Exception {
        Path source = Files.createTempFile("source-line-endings", ".java");
        try {
            Files.writeString(source, "first\r\nsecond\r\nlast");
            var window = SourceCodeContext.read(source, 3);
            Assert.assertEquals(window.lines().stream().map(SourceCodeContext.Line::text).toList(),
                    List.of("first", "second", "last"));
            Assert.assertEquals(SourceCodeContext.snippet(source, 3),
                    "1: first" + System.lineSeparator() + "  2: second" + System.lineSeparator() + "> 3: last");
            Assert.assertTrue(SourceCodeContext.read(source, 4).lines().isEmpty());
        } finally {
            Files.deleteIfExists(source);
        }
    }

    @Test
    public void missingAndUnreadableSourcesExplainTheFailureWithoutLeakingPaths() throws Exception {
        Path directory = Files.createTempDirectory("source-unreadable");
        try {
            for (Path path : List.of(directory, directory.resolve("missing.java"))) {
                var window = SourceCodeContext.read(path, 42);
                Assert.assertTrue(window.lines().isEmpty());
                Assert.assertEquals(window.unavailableReason(), "Java source could not be read on the test runner.");
                String html = SourceCodeAttachment.render(new StackTraceElement("customer.Test", "run", "Test.java", 42),
                        "Test.java", window);
                Assert.assertTrue(html.contains("Recorded line: 42"));
                Assert.assertFalse(html.contains("source-line active"));
                Assert.assertFalse(html.contains(directory.toString()));
            }
        } finally {
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void unsupportedMetadataAndBootstrapClassesHaveSafeFallbacks() throws Exception {
        for (String name : new String[]{null, "../Secret.java", "..\\Secret.java", "Fixture.kt"}) {
            Assert.assertNull(SourceCodeContext.findSourceFile(
                    new StackTraceElement("customer.UnavailableSourceFixture", "run", name, 1), String.class));
        }
        var frame = new StackTraceElement("customer.UnavailableSourceFixture", "run", "UnavailableSourceFixture.java", 1);
        Assert.assertNull(SourceCodeContext.findSourceFile(frame, String.class));
        Path outside = Files.createTempFile("source-private-location", ".java");
        try {
            Assert.assertEquals(SourceCodeContext.relative(outside, String.class), outside.getFileName().toString());
            Assert.assertEquals(SourceCodeContext.relative(outside, null), outside.getFileName().toString());
        } finally {
            Files.deleteIfExists(outside);
        }
    }

    @org.testng.annotations.DataProvider
    public Object[][] mavenSourceRoots() {
        return new Object[][]{{"src/test/java", "test-classes", "customer"}, {"src/main/java", "classes", ""}};
    }

    @Test(dataProvider = "mavenSourceRoots")
    public void externalMavenModulesResolveTheirOwnSources(String sourceRoot, String outputName, String packageName) throws Exception {
        try (CompiledSource fixture = new CompiledSource(sourceRoot, outputName, packageName)) {
            Path resolved = java.util.Objects.requireNonNull(SourceCodeContext.findSourceFile(fixture.frame, fixture.type));
            Assert.assertEquals(resolved.toRealPath(), fixture.source.toRealPath());
            String relative = SourceCodeContext.relative(resolved, fixture.type);
            Assert.assertEquals(relative, fixture.directory.getFileName() + "/"
                    + fixture.directory.relativize(fixture.source).toString().replace('\\', '/'));
            Assert.assertFalse(Path.of(relative).isAbsolute());
            Assert.assertEquals(SourceCodeContext.read(resolved, fixture.frame.getLineNumber()).lines().size(), 5);
            Path outsideModule = fixture.directory.resolveSibling("unrelated-source.java");
            Assert.assertEquals(SourceCodeContext.relative(outsideModule, fixture.type), "unrelated-source.java");
        }
    }

    @Test
    public void compiledOnlyAndRemoteCodeOriginsDoNotInventSource() throws Exception {
        try (CompiledSource fixture = new CompiledSource("src/test/java", "test-classes", "customer")) {
            Files.delete(fixture.source);
            Assert.assertNull(SourceCodeContext.findSourceFile(fixture.frame, fixture.type));
            String html = SourceCodeAttachment.render(fixture.frame, fixture.frame.getFileName(),
                    SourceCodeContext.read(null, fixture.frame.getLineNumber()));
            Assert.assertTrue(html.contains("Java source is unavailable"));
            Assert.assertTrue(html.contains("Recorded line: " + fixture.frame.getLineNumber()));
            Assert.assertFalse(html.contains("source-line active"));
            byte[] bytes = Files.readAllBytes(fixture.classFile);
            for (java.net.URL location : List.of(java.net.URI.create("https://source.invalid/classes/").toURL(),
                    fixture.directory.resolve("compiled-only.jar").toUri().toURL())) {
                Class<?> isolated = new SourceOriginLoader().define(fixture.type.getName(), bytes, location);
                Assert.assertNull(SourceCodeContext.findSourceFile(fixture.frame, isolated));
                Assert.assertEquals(SourceCodeContext.relative(fixture.source, isolated), fixture.source.getFileName().toString());
            }
        }
    }

    @Test
    public void sourceSymlinkCannotEscapeItsMavenRoot() throws Exception {
        try (CompiledSource fixture = new CompiledSource("src/test/java", "test-classes", "customer")) {
            Path outside = fixture.directory.resolve("outside.java");
            Files.move(fixture.source, outside);
            try {
                Files.createSymbolicLink(fixture.source, outside);
            } catch (UnsupportedOperationException | java.nio.file.FileSystemException unavailable) {
                throw new org.testng.SkipException("Symbolic links are unavailable on this runner.", unavailable);
            }
            Assert.assertNull(SourceCodeContext.findSourceFile(fixture.frame, fixture.type),
                    "A source-file symlink must not publish content outside the allowed source root.");
        }
    }

    private static final class SourceOriginLoader extends java.security.SecureClassLoader {
        private Class<?> define(String name, byte[] bytes, java.net.URL location) {
            return defineClass(name, bytes, 0, bytes.length,
                    new java.security.CodeSource(location, new java.security.cert.Certificate[0]));
        }
    }

    private static final class CompiledSource implements AutoCloseable {
        private final Path directory = Files.createTempDirectory("source-maven-module");
        private final Path source;
        private final Path classFile;
        private final java.net.URLClassLoader loader;
        private final Class<?> type;
        private final StackTraceElement frame;

        private CompiledSource(String sourceRoot, String outputName, String packageName) throws Exception {
            String simpleName = "UnavailableSourceFixture";
            String className = packageName.isEmpty() ? simpleName : packageName + "." + simpleName;
            String relative = className.replace('.', '/');
            source = directory.resolve(sourceRoot).resolve(relative + ".java");
            Path output = directory.resolve("target").resolve(outputName);
            classFile = output.resolve(relative + ".class");
            Files.createDirectories(source.getParent());
            Files.createDirectories(output);
            Files.writeString(source, (packageName.isEmpty() ? "" : "package " + packageName + ";\n")
                    + "public final class " + simpleName + " {\n"
                    + "  public static StackTraceElement caller() {\n"
                    + "    return new Throwable().getStackTrace()[0];\n"
                    + "  }\n}\n");
            var compiler = java.util.Objects.requireNonNull(javax.tools.ToolProvider.getSystemJavaCompiler(),
                    "The source fixture requires the JDK compiler used by the Maven build.");
            Assert.assertEquals(compiler.run(java.io.InputStream.nullInputStream(), System.out, System.err,
                    "-g", "-d", output.toString(), source.toString()), 0);
            loader = new java.net.URLClassLoader(new java.net.URL[]{output.toUri().toURL()}, ClassLoader.getPlatformClassLoader());
            type = loader.loadClass(className);
            frame = (StackTraceElement) java.util.Objects.requireNonNull(type.getMethod("caller").invoke(null));
        }

        @Override
        public void close() throws Exception {
            loader.close();
            try (var paths = Files.walk(directory)) {
                for (Path path : paths.sorted(java.util.Comparator.reverseOrder()).toList()) {
                    Files.deleteIfExists(path);
                }
            }
        }
    }
}
