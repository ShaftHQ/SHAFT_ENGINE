package com.shaft.driver.internal.DriverFactory;

import java.time.Duration;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Backs {@code executionAddress=testcontainers} (#6405): starts one {@code selenium/standalone-<browser>} container per
 * browser per JVM through Testcontainers (optional dependency) and returns its WebDriver URL. Ryuk removes it on exit.
 * Local servers exposed with {@code org.testcontainers.Testcontainers.exposeHostPorts(port)} are reachable from the
 * browser at {@code host.testcontainers.internal}.
 */
public final class TestcontainersGrid {
    /**
     * The {@code executionAddress} value that selects this mode.
     */
    public static final String EXECUTION_ADDRESS = "testcontainers";
    private static final int WEBDRIVER_PORT = 4444;
    private static final Map<String, String> HUBS = new ConcurrentHashMap<>();

    private TestcontainersGrid() {
    }

    /**
     * Returns the WebDriver URL of the container for this browser, starting it on first use.
     *
     * @param browserName target browser name, for example {@code chrome}
     * @return the remote WebDriver URL
     * @throws IllegalStateException when Testcontainers is missing or Docker is unavailable
     */
    public static String hubUrl(String browserName) {
        return HUBS.computeIfAbsent(image(browserName), TestcontainersGrid::start);
    }

    /**
     * Maps a browser name to its Selenium standalone image.
     *
     * @param browserName target browser name
     * @return the image name
     */
    public static String image(String browserName) {
        String browser = browserName == null || browserName.isBlank() ? "chrome" : browserName.toLowerCase(Locale.ROOT);
        return "selenium/standalone-" + (browser.contains("edge") ? "edge" : browser.contains("firefox") ? "firefox" : "chrome") + ":latest";
    }

    /**
     * Reports whether Testcontainers is on the classpath and can reach Docker.
     *
     * @return {@code true} when containers can start
     */
    public static boolean isAvailable() {
        try {
            Class.forName("org.testcontainers.DockerClientFactory");
            return org.testcontainers.DockerClientFactory.instance().isDockerAvailable();
        } catch (ClassNotFoundException | LinkageError e) {
            return false;
        }
    }

    private static String start(String image) {
        try {
            Class.forName("org.testcontainers.DockerClientFactory");
        } catch (ClassNotFoundException e) {
            throw new IllegalStateException("executionAddress=testcontainers needs the org.testcontainers:testcontainers dependency on the test classpath.", e);
        }
        if (!org.testcontainers.DockerClientFactory.instance().isDockerAvailable()) {
            throw new IllegalStateException("executionAddress=testcontainers needs a running Docker daemon, but none was found. Start Docker or use executionAddress=local.");
        }
        @SuppressWarnings("resource") // Ryuk stops it when the JVM exits
        var container = new org.testcontainers.containers.GenericContainer<>(org.testcontainers.utility.DockerImageName.parse(image))
                .withExposedPorts(WEBDRIVER_PORT)
                .withAccessToHost(true)
                .withSharedMemorySize(2L * 1024 * 1024 * 1024)
                .waitingFor(org.testcontainers.containers.wait.strategy.Wait.forHttp("/status").forPort(WEBDRIVER_PORT)
                        .withStartupTimeout(Duration.ofMinutes(3)));
        container.start();
        return "http://" + container.getHost() + ":" + container.getMappedPort(WEBDRIVER_PORT);
    }
}
