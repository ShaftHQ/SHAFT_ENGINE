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

/** Release-coupled plans for a SHAFT-owned Healenium compose project. */
final class HealeniumSetupPlanner {
    static final String BACKEND_IMAGE = "healenium/hlm-backend:3.4.6";
    static final String IMITATOR_IMAGE = "healenium/hlm-selector-imitator:1.4";
    static final String POSTGRES_IMAGE = "postgres:15.5-alpine";
    static final String PIN = "3.4.6+1.4+15.5-alpine";
    static final String PROJECT = "shaft-healenium";
    static final int DEFAULT_BACKEND_PORT = 7878;
    static final int DEFAULT_IMITATE_PORT = 8000;
    static final String INIT_SQL = """
            CREATE SCHEMA healenium AUTHORIZATION healenium_user;
            GRANT USAGE ON SCHEMA healenium TO healenium_user;
            """;
    /** Upstream local-dev default from the official Healenium compose example; not a SHAFT secret. */
    static final String LOCAL_DEV_DB_PASSWORD = "YDk2nmNs4s9aCP6K";
    private static final Pattern SPEC = Pattern.compile(
            Pattern.quote(PIN) + ",backend=([0-9]+),imitate=([0-9]+)");

    private HealeniumSetupPlanner() {
        throw new IllegalStateException("Utility class");
    }

    static SetupPlan plan(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode,
                          SetupSelection selection) {
        HealeniumScale scale = scale(selection);
        SetupActionKind kind = mode == SetupMode.EXTERNAL ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
        String artifact = artifact(scale);
        byte[] bytes = artifact.getBytes(StandardCharsets.UTF_8);
        return SetupPlan.create(SetupProfile.HEALENIUM, platform, architecture, mode, List.of(
                new SetupAction(SetupTarget.DOCKER, SetupActionKind.DIAGNOSE, "26.1.4+",
                        URI.create("urn:shaft:host:docker"),
                        sha256("docker\026.1.4+"), false, Set.of()),
                new SetupAction(SetupTarget.HEALENIUM, kind, spec(scale),
                        URI.create("urn:shaft:healenium:" + PROJECT + ":" + spec(scale)),
                        sha256(artifact), bytes.length, false, Set.of())));
    }

    static SetupSelection selectionFromPlan(SetupPlan plan) {
        return selectionFromScale(scaleFromPlan(plan));
    }

    static HealeniumScale scaleFromPlan(SetupPlan plan) {
        if (plan.profile() != SetupProfile.HEALENIUM) {
            throw new IllegalArgumentException("Healenium scale is only defined for Healenium plans.");
        }
        List<SetupAction> actions = plan.actions().stream()
                .filter(action -> action.target() == SetupTarget.HEALENIUM).toList();
        if (actions.size() != 1) {
            throw new IllegalArgumentException("Healenium plan must contain exactly one HEALENIUM action.");
        }
        Matcher match = SPEC.matcher(actions.getFirst().version());
        if (!match.matches()) throw new IllegalArgumentException("Healenium plan metadata is invalid.");
        return new HealeniumScale(parsePort("backend", match.group(1), DEFAULT_BACKEND_PORT),
                parsePort("imitate", match.group(2), DEFAULT_IMITATE_PORT));
    }

    static String compose(HealeniumScale scale) {
        return """
                services:
                  postgres-db:
                    image: %1$s
                    environment:
                      POSTGRES_DB: healenium
                      POSTGRES_USER: healenium_user
                      POSTGRES_PASSWORD: %6$s
                    volumes:
                      - ./init.sql:/docker-entrypoint-initdb.d/init.sql
                    healthcheck:
                      test: ["CMD-SHELL", "pg_isready -U healenium_user -d healenium"]
                      interval: 5s
                      timeout: 10s
                      retries: 20
                  healenium:
                    image: %2$s
                    depends_on:
                      postgres-db:
                        condition: service_healthy
                    ports:
                      - "%4$d:7878"
                    environment:
                      - SPRING_POSTGRES_DB=healenium
                      - SPRING_POSTGRES_SCHEMA=healenium
                      - SPRING_POSTGRES_USER=healenium_user
                      - SPRING_POSTGRES_PASSWORD=%6$s
                      - SPRING_POSTGRES_DB_HOST=postgres-db
                      - KEY_SELECTOR_URL=false
                      - COLLECT_METRICS=true
                      - FIND_ELEMENTS_AUTO_HEALING=false
                      - HLM_LOG_LEVEL=info
                  selector-imitator:
                    image: %3$s
                    ports:
                      - "%5$d:8000"
                """.formatted(POSTGRES_IMAGE, BACKEND_IMAGE, IMITATOR_IMAGE, scale.backendPort(),
                scale.imitatePort(), LOCAL_DEV_DB_PASSWORD);
    }

    static String artifact(HealeniumScale scale) {
        return compose(scale) + "\n" + INIT_SQL;
    }

    static HealeniumScale scale(SetupSelection selection) {
        int backend = selected(selection, "backend_", DEFAULT_BACKEND_PORT, 1024, 65535, "Healenium backend port");
        int imitate = selected(selection, "imitate_", DEFAULT_IMITATE_PORT, 1024, 65535, "Healenium imitate port");
        if (backend == imitate) {
            throw new IllegalArgumentException("Healenium backend and imitate ports must be different.");
        }
        for (String component : selection.components()) {
            if (!component.startsWith("backend_") && !component.startsWith("imitate_")) {
                throw new IllegalArgumentException("Unsupported Healenium component: " + component);
            }
        }
        return new HealeniumScale(backend, imitate);
    }

    static SetupSelection selectionFromScale(HealeniumScale scale) {
        java.util.ArrayList<String> components = new java.util.ArrayList<>();
        if (scale.backendPort() != DEFAULT_BACKEND_PORT) components.add("backend_" + scale.backendPort());
        if (scale.imitatePort() != DEFAULT_IMITATE_PORT) components.add("imitate_" + scale.imitatePort());
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

    private static int parsePort(String name, String value, int defaultValue) {
        return selected(new SetupSelection(List.of(name + "_" + value)), name + "_", defaultValue, 1024, 65535,
                "Healenium " + name + " port");
    }

    private static String spec(HealeniumScale scale) {
        return PIN + ",backend=" + scale.backendPort() + ",imitate=" + scale.imitatePort();
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

    record HealeniumScale(int backendPort, int imitatePort) { }
}
