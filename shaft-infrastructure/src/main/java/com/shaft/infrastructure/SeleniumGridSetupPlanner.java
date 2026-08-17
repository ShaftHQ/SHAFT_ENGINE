package com.shaft.infrastructure;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Release-coupled plans for a SHAFT-owned Selenium Grid compose project. */
final class SeleniumGridSetupPlanner {
    static final String IMAGE_TAG = "4.47.0-20260808";
    static final String PROJECT = "shaft-selenium-grid";
    static final int DEFAULT_PORT = 4444;
    static final int DEFAULT_CHROME = 1;
    static final int DEFAULT_EDGE = 0;
    static final int DEFAULT_FIREFOX = 0;
    private static final Pattern SPEC = Pattern.compile(
            Pattern.quote(IMAGE_TAG) + ",port=([0-9]+),chrome=([0-9]+),edge=([0-9]+),firefox=([0-9]+)");

    private SeleniumGridSetupPlanner() {
        throw new IllegalStateException("Utility class");
    }

    static SetupPlan plan(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode,
                          SetupSelection selection) {
        GridScale scale = scale(selection);
        SetupActionKind gridKind = mode == SetupMode.EXTERNAL ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
        String compose = compose(scale.port());
        byte[] bytes = compose.getBytes(StandardCharsets.UTF_8);
        return SetupPlan.create(SetupProfile.SELENIUM_GRID, platform, architecture, mode, List.of(
                new SetupAction(SetupTarget.DOCKER, SetupActionKind.DIAGNOSE, "26.1.4+",
                        URI.create("urn:shaft:host:docker"),
                        sha256("docker\026.1.4+"), false, Set.of()),
                new SetupAction(SetupTarget.SELENIUM_GRID, gridKind, spec(scale),
                        URI.create("urn:shaft:selenium-grid:" + PROJECT + ":" + spec(scale)),
                        sha256(compose), bytes.length, false, Set.of())));
    }

    static SetupSelection selectionFromPlan(SetupPlan plan) {
        return selectionFromScale(scaleFromPlan(plan));
    }

    static GridScale scaleFromPlan(SetupPlan plan) {
        if (plan.profile() != SetupProfile.SELENIUM_GRID) {
            throw new IllegalArgumentException("Grid scale is only defined for Selenium Grid plans.");
        }
        List<SetupAction> actions = plan.actions().stream()
                .filter(action -> action.target() == SetupTarget.SELENIUM_GRID).toList();
        if (actions.size() != 1) {
            throw new IllegalArgumentException("Selenium Grid plan must contain exactly one SELENIUM_GRID action.");
        }
        Matcher match = SPEC.matcher(actions.getFirst().version());
        if (!match.matches()) throw new IllegalArgumentException("Selenium Grid plan metadata is invalid.");
        return new GridScale(parsePort(match.group(1)), parseScale("chrome", match.group(2)),
                parseScale("edge", match.group(3)), parseScale("firefox", match.group(4)));
    }

    static String compose(int port) {
        return """
                services:
                  chrome:
                    image: selenium/node-chrome:%1$s
                    shm_size: 2gb
                    depends_on:
                      selenium-hub:
                        condition: service_healthy
                    extra_hosts:
                      - "host.docker.internal:host-gateway"
                    environment:
                      - SE_EVENT_BUS_HOST=selenium-hub
                      - SE_NODE_GRID_URL=http://localhost:%2$d
                      - SE_NODE_CONNECTION_LIMIT_PER_SESSION=1000
                      - SE_NODE_SESSION_TIMEOUT=7200
                      - SE_RECORD_VIDEO=true
                  edge:
                    image: selenium/node-edge:%1$s
                    shm_size: 2gb
                    depends_on:
                      selenium-hub:
                        condition: service_healthy
                    extra_hosts:
                      - "host.docker.internal:host-gateway"
                    environment:
                      - SE_EVENT_BUS_HOST=selenium-hub
                      - SE_NODE_GRID_URL=http://localhost:%2$d
                      - SE_NODE_CONNECTION_LIMIT_PER_SESSION=1000
                      - SE_NODE_SESSION_TIMEOUT=7200
                      - SE_RECORD_VIDEO=true
                  firefox:
                    image: selenium/node-firefox:%1$s
                    shm_size: 2gb
                    depends_on:
                      selenium-hub:
                        condition: service_healthy
                    extra_hosts:
                      - "host.docker.internal:host-gateway"
                    environment:
                      - SE_EVENT_BUS_HOST=selenium-hub
                      - SE_NODE_GRID_URL=http://localhost:%2$d
                      - SE_NODE_CONNECTION_LIMIT_PER_SESSION=1000
                      - SE_NODE_SESSION_TIMEOUT=7200
                      - SE_RECORD_VIDEO=true
                  selenium-hub:
                    image: selenium/hub:%1$s
                    ports:
                      - "4442:4442"
                      - "4443:4443"
                      - "%2$d:4444"
                    healthcheck:
                      test: [ "CMD-SHELL", "curl -sf http://localhost:4444/wd/hub/status >/dev/null || exit 1" ]
                      interval: 5s
                      timeout: 10s
                      retries: 20
                """.formatted(IMAGE_TAG, port);
    }

    static GridScale scale(SetupSelection selection) {
        int port = selected(selection, "port_", DEFAULT_PORT, 1024, 65535, "Grid port");
        int chrome = selected(selection, "chrome_", DEFAULT_CHROME, 0, 20, "chrome scale");
        int edge = selected(selection, "edge_", DEFAULT_EDGE, 0, 20, "edge scale");
        int firefox = selected(selection, "firefox_", DEFAULT_FIREFOX, 0, 20, "firefox scale");
        for (String component : selection.components()) {
            if (!component.startsWith("port_") && !component.startsWith("chrome_")
                    && !component.startsWith("edge_") && !component.startsWith("firefox_")) {
                throw new IllegalArgumentException("Unsupported Selenium Grid component: " + component);
            }
        }
        if (chrome + edge + firefox == 0) {
            throw new IllegalArgumentException("Select at least one Grid browser replica.");
        }
        return new GridScale(port, chrome, edge, firefox);
    }

    static SetupSelection selectionFromScale(GridScale scale) {
        java.util.ArrayList<String> components = new java.util.ArrayList<>();
        if (scale.port() != DEFAULT_PORT) components.add("port_" + scale.port());
        if (scale.chrome() != DEFAULT_CHROME) components.add("chrome_" + scale.chrome());
        if (scale.edge() != DEFAULT_EDGE) components.add("edge_" + scale.edge());
        if (scale.firefox() != DEFAULT_FIREFOX) components.add("firefox_" + scale.firefox());
        return new SetupSelection(components);
    }

    private static int selected(SetupSelection selection, String prefix, int defaultValue, int min, int max,
                                String label) {
        List<String> values = selection.components().stream().filter(component -> component.startsWith(prefix)).toList();
        if (values.size() > 1) throw new IllegalArgumentException("Select at most one " + label + '.');
        int value = defaultValue;
        if (!values.isEmpty()) {
            try {
                value = Integer.parseInt(values.getFirst().substring(prefix.length()));
            } catch (NumberFormatException invalid) {
                throw new IllegalArgumentException(label + " must be a decimal integer.", invalid);
            }
        }
        if (value < min || value > max) {
            throw new IllegalArgumentException(label + " must be between " + min + " and " + max + '.');
        }
        return value;
    }

    private static int parsePort(String value) {
        return selected(new SetupSelection(List.of("port_" + value)), "port_", DEFAULT_PORT, 1024, 65535, "Grid port");
    }

    private static int parseScale(String browser, String value) {
        return selected(new SetupSelection(List.of(browser + "_" + value)), browser + "_", 0, 0, 20,
                browser + " scale");
    }

    private static String spec(GridScale scale) {
        return IMAGE_TAG + ",port=" + scale.port() + ",chrome=" + scale.chrome() + ",edge=" + scale.edge()
                + ",firefox=" + scale.firefox();
    }

    private static String sha256(String value) {
        try {
            byte[] digest = MessageDigest.getInstance("SHA-256")
                    .digest(value.getBytes(StandardCharsets.UTF_8));
            return "sha256:" + HexFormat.of().formatHex(digest);
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", impossible);
        }
    }

    record GridScale(int port, int chrome, int edge, int firefox) { }
}
