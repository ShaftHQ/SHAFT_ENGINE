package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.testng.TestNG;
import org.testng.xml.XmlClass;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlTest;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Stream;

/**
 * Initializes runtime project folders and service provider metadata files.
 */
public class ProjectStructureManager {
    private static final Logger logger = LogManager.getLogger(ProjectStructureManager.class);
    private static final String JUNIT_EXTENSION_AUTODETECTION = "junit.jupiter.extensions.autodetection.enabled=true";

    /**
     * Detects the active test runner from the current stack trace.
     *
     * @return the inferred execution mode
     */
    public static RunType identifyRunType() {
        Supplier<Stream<String>> stacktraceSupplier = () -> Arrays.stream(new Throwable().getStackTrace())
                .map(StackTraceElement::getClassName);
        var isUsingJunitDiscovery = stacktraceSupplier.get()
                .anyMatch(org.junit.platform.launcher.core.EngineDiscoveryOrchestrator.class.getCanonicalName()::equals);
        var isUsingTestNG = stacktraceSupplier.get().anyMatch(TestNG.class.getCanonicalName()::equals);
        var isUsingCucumber = stacktraceSupplier.get()
                .anyMatch(io.cucumber.core.runner.Runner.class.getCanonicalName()::equals);
        if (isUsingJunitDiscovery) {
            logger.debug("Detected JUnit 5 execution.");
            return RunType.JUNIT;
        } else if (isUsingTestNG) {
            logger.debug("Detected TestNG execution.");
            return RunType.TESTNG;
        } else if (isUsingCucumber) {
            logger.debug("Detected Cucumber execution.");
            return RunType.CUCUMBER;
        } else {
            logger.debug("Detected JUnit 5 execution.");
            return RunType.JUNIT;
        }
    }

    /**
     * Detects the active test runner from the TestNG suites being altered.
     *
     * <p>This overload exists specifically for {@link org.testng.IAlterSuiteListener#alter},
     * which TestNG invokes directly during its own suite-processing phase, before any test
     * method (and therefore before any Cucumber {@code io.cucumber.core.runner.Runner} stack
     * frame) has run. At that point {@link #identifyRunType()}'s stack probe can never observe
     * Cucumber, even for a Cucumber-over-TestNG run ({@code AbstractTestNGCucumberTests}), so a
     * caller at that call site is already known to be TestNG-driven &mdash; the only remaining
     * question is whether TestNG is hosting a Cucumber runner. That is answered here by
     * inspecting the suite's own class list instead of the call stack.
     *
     * @param suites the suites TestNG is about to run
     * @return {@link RunType#CUCUMBER} when any suite (including suites reached transitively via
     * {@code <suite-file>}) hosts a Cucumber TestNG runner class, whether declared directly in
     * {@code <classes>} (with or without a {@code <methods>} filter) or discovered by a
     * {@code <packages>} scan, {@link RunType#TESTNG} otherwise
     */
    public static RunType identifyRunType(List<XmlSuite> suites) {
        boolean isUsingCucumberTestNgRunner = flattenSuites(suites)
                .flatMap(suite -> suite.getTests().stream())
                .flatMap(ProjectStructureManager::classesDeclaredByTest)
                .anyMatch(ProjectStructureManager::extendsAbstractTestNGCucumberTests);
        if (isUsingCucumberTestNgRunner) {
            logger.debug("Detected Cucumber execution (TestNG runner).");
            return RunType.CUCUMBER;
        }
        logger.debug("Detected TestNG execution.");
        return RunType.TESTNG;
    }

    /**
     * Walks {@code suites} together with every suite transitively reachable through
     * {@link XmlSuite#getChildSuites()} (the in-memory representation of a {@code <suite-file>}
     * reference). Traversal is bounded by an identity-based visited set: a suite already emitted is
     * never re-queued, which keeps this terminating even if a caller hands in a child-suite graph
     * that cycles back on itself (TestNG's own parser never produces one, but this method takes a
     * plain {@code List<XmlSuite>} and must not hang on an adversarial or hand-built one either).
     *
     * @param suites the top-level suites to start from
     * @return every suite in {@code suites} and its transitive child suites, each exactly once
     */
    private static Stream<XmlSuite> flattenSuites(List<XmlSuite> suites) {
        Set<XmlSuite> visited = Collections.newSetFromMap(new IdentityHashMap<>());
        List<XmlSuite> flattened = new ArrayList<>();
        Deque<XmlSuite> pending = new ArrayDeque<>(suites);
        while (!pending.isEmpty()) {
            XmlSuite suite = pending.removeFirst();
            if (visited.add(suite)) {
                flattened.add(suite);
                pending.addAll(suite.getChildSuites());
            }
        }
        return flattened.stream();
    }

    /**
     * The classes a {@link XmlTest} makes available to its suite, whether declared explicitly via
     * {@code <classes>} or discovered via a {@code <packages>} scan. {@link org.testng.xml.XmlPackage#getXmlClasses()}
     * performs that scan itself (scoped to the declared package, not the full classpath) and is the
     * same mechanism TestNG uses internally to resolve {@code <packages>} at suite-build time.
     */
    private static Stream<XmlClass> classesDeclaredByTest(XmlTest xmlTest) {
        return Stream.concat(
                xmlTest.getXmlClasses().stream(),
                xmlTest.getXmlPackages().stream().flatMap(xmlPackage -> xmlPackage.getXmlClasses().stream()));
    }

    private static boolean extendsAbstractTestNGCucumberTests(XmlClass xmlClass) {
        try {
            return io.cucumber.testng.AbstractTestNGCucumberTests.class.isAssignableFrom(xmlClass.getSupportClass());
        } catch (Throwable throwable) {
            return false;
        }
    }

    /**
     * Prepares project structure and listener registration files based on run type.
     *
     * @param runType current execution framework mode
     */
    public static void initialize(RunType runType) {
        ReportManager.logDiscrete("Preparing SHAFT project structure.");
        if (Properties.platform.executionAddress().equals("local")
                && !Paths.get(System.getProperty("user.dir")).getFileName().toString().equals("shaft-engine")) {
            FileActions.getInstance(true).createFolder(Properties.paths.properties());
            FileActions.getInstance(true).createFolder(Properties.paths.dynamicObjectRepository());
            FileActions.getInstance(true).createFolder(Properties.paths.testData());
        }
        // manually override listeners configuration
        if (Properties.platform.executionAddress().equals("local")) {
            FileActions.getInstance(true).deleteFolder(Properties.paths.services());
            switch (runType) {
                case JUNIT -> {
                    FileActions.getInstance(true).createFolder(Properties.paths.services());
                    FileActions.getInstance(true).writeToFile(Properties.paths.services(), "org.junit.platform.launcher.LauncherSessionListener", "com.shaft.listeners.JunitListener");
                    FileActions.getInstance(true).writeToFile(Properties.paths.services(), "org.junit.jupiter.api.extension.Extension", "com.shaft.listeners.JunitExtension");
                    createOrUpdateJunitPlatformProperties();
                }
                case TESTNG, AI_AGENT, CUCUMBER -> {
                    FileActions.getInstance(true).createFolder(Properties.paths.services());
                    FileActions.getInstance(true).writeToFile(Properties.paths.services(), "org.testng.ITestNGListener", "com.shaft.listeners.TestNGListener");
                    FileActions.getInstance(true).writeToFile(Properties.paths.services(), "org.testng.IAnnotationTransformer", "com.shaft.listeners.TestNGListener");
                }
//                case CUCUMBER -> {
//                    FileActions.getInstance(true).createFolder(Properties.paths.services());
//                    FileActions.getInstance(true).writeToFile(Properties.paths.services(), "io.cucumber.plugin.ConcurrentEventListener", "com.shaft.listeners.CucumberFeatureListener");
//                }
            }
            createAllureListenersMetaFiles();
        }
    }

    private static void createAllureListenersMetaFiles() {
        FileActions.getInstance(true).createFolder(com.shaft.properties.internal.Properties.paths.services());
        Arrays.asList("io.qameta.allure.listener.ContainerLifecycleListener", "io.qameta.allure.listener.FixtureLifecycleListener",
                "io.qameta.allure.listener.StepLifecycleListener", "io.qameta.allure.listener.TestLifecycleListener").forEach(fileName -> FileActions.getInstance(true).writeToFile(Properties.paths.services(), fileName, "com.shaft.listeners.AllureListener"));
    }

    private static void createOrUpdateJunitPlatformProperties() {
        Path propertiesPath = getJunitPlatformPropertiesPath();
        try {
            Files.createDirectories(propertiesPath.getParent());
            if (Files.isRegularFile(propertiesPath)) {
                String content = Files.readString(propertiesPath, StandardCharsets.UTF_8);
                if (!content.contains(JUNIT_EXTENSION_AUTODETECTION)) {
                    Files.writeString(propertiesPath,
                            System.lineSeparator() + JUNIT_EXTENSION_AUTODETECTION + System.lineSeparator(),
                            StandardCharsets.UTF_8,
                            java.nio.file.StandardOpenOption.APPEND);
                }
            } else {
                Files.writeString(propertiesPath,
                        JUNIT_EXTENSION_AUTODETECTION + System.lineSeparator(),
                        StandardCharsets.UTF_8);
            }
        } catch (IOException e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    private static Path getJunitPlatformPropertiesPath() {
        Path resourceRoot = Paths.get(Properties.paths.services()).normalize();
        Path parent = resourceRoot.getParent();
        if (parent != null && "services".equals(resourceRoot.getFileName().toString())) {
            resourceRoot = parent;
        }
        parent = resourceRoot.getParent();
        if (parent != null && "META-INF".equals(resourceRoot.getFileName().toString())) {
            resourceRoot = parent;
        }
        return resourceRoot.resolve("junit-platform.properties");
    }

    /**
     * Supported runtime execution modes used for listener bootstrapping.
     */
    public enum RunType {TESTNG, JUNIT, CUCUMBER, AI_AGENT}
}
