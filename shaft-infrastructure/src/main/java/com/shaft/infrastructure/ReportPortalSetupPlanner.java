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

/** Release-coupled plans for a SHAFT-owned ReportPortal compose project. */
final class ReportPortalSetupPlanner {
    static final String GATEWAY_IMAGE = "traefik:v2.11.54";
    static final String POSTGRES_IMAGE = "postgres:18.4";
    static final String RABBITMQ_IMAGE = "rabbitmq:4.3.4-management";
    static final String MIGRATIONS_IMAGE = "reportportal/migrations:5.15.4";
    static final String INDEX_IMAGE = "reportportal/service-index:5.15.1";
    static final String UI_IMAGE = "reportportal/service-ui:5.15.5";
    static final String API_IMAGE = "reportportal/service-api:5.15.4";
    static final String UAT_IMAGE = "reportportal/service-authorization:5.15.1";
    static final String JOBS_IMAGE = "reportportal/service-jobs:5.15.2";
    static final String PIN = "5.15.5+5.15.4+18.4";
    static final String PROJECT = "shaft-reportportal";
    static final int DEFAULT_UI_PORT = 8080;
    /** Official local-dev defaults from reportportal/reportportal docker-compose.yml; not SHAFT secrets. */
    static final String LOCAL_DEV_DB_PASSWORD = "rppass";
    static final String LOCAL_DEV_AMQP_PASSWORD = "rabbitmq";
    static final String LOCAL_DEV_ADMIN_PASSWORD = "erebus";
    private static final Pattern SPEC = Pattern.compile(Pattern.quote(PIN) + ",ui=([0-9]+)");

    private ReportPortalSetupPlanner() {
        throw new IllegalStateException("Utility class");
    }

    static SetupPlan plan(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode,
                          SetupSelection selection) {
        ReportPortalScale scale = scale(selection);
        SetupActionKind kind = mode == SetupMode.EXTERNAL ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
        String artifact = compose(scale);
        byte[] bytes = artifact.getBytes(StandardCharsets.UTF_8);
        return SetupPlan.create(SetupProfile.REPORT_PORTAL, platform, architecture, mode, List.of(
                new SetupAction(SetupTarget.DOCKER, SetupActionKind.DIAGNOSE, "26.1.4+",
                        URI.create("urn:shaft:host:docker"),
                        sha256("docker\026.1.4+"), false, Set.of()),
                new SetupAction(SetupTarget.REPORT_PORTAL, kind, spec(scale),
                        URI.create("urn:shaft:reportportal:" + PROJECT + ":" + spec(scale)),
                        sha256(artifact), bytes.length, false, Set.of())));
    }

    static SetupSelection selectionFromPlan(SetupPlan plan) {
        return selectionFromScale(scaleFromPlan(plan));
    }

    static ReportPortalScale scaleFromPlan(SetupPlan plan) {
        if (plan.profile() != SetupProfile.REPORT_PORTAL) {
            throw new IllegalArgumentException("ReportPortal scale is only defined for ReportPortal plans.");
        }
        List<SetupAction> actions = plan.actions().stream()
                .filter(action -> action.target() == SetupTarget.REPORT_PORTAL).toList();
        if (actions.size() != 1) {
            throw new IllegalArgumentException("ReportPortal plan must contain exactly one REPORT_PORTAL action.");
        }
        Matcher match = SPEC.matcher(actions.getFirst().version());
        if (!match.matches()) throw new IllegalArgumentException("ReportPortal plan metadata is invalid.");
        return new ReportPortalScale(parsePort(match.group(1)));
    }

    static String compose(ReportPortalScale scale) {
        return """
                services:
                  gateway:
                    image: %s
                    ports:
                      - "%d:8080"
                    volumes:
                      - /var/run/docker.sock:/var/run/docker.sock
                    command:
                      - --providers.docker=true
                      - --providers.docker.constraints=Label(`traefik.expose`, `true`)
                      - --entrypoints.web.address=:8080
                      - --entrypoints.traefik.address=:8081
                      - --api.dashboard=true
                      - --api.insecure=true
                    networks:
                      - reportportal
                    restart: always
                  postgres:
                    image: %s
                    shm_size: '512m'
                    environment:
                      POSTGRES_USER: rpuser
                      POSTGRES_PASSWORD: %s
                      POSTGRES_DB: reportportal
                    volumes:
                      - postgres:/var/lib/postgresql
                    healthcheck:
                      test: ["CMD-SHELL", "pg_isready -d $$POSTGRES_DB -U $$POSTGRES_USER"]
                      interval: 10s
                      timeout: 120s
                      retries: 10
                    networks:
                      - reportportal
                    restart: always
                  rabbitmq:
                    image: %s
                    environment:
                      RABBITMQ_DEFAULT_USER: rabbitmq
                      RABBITMQ_DEFAULT_PASS: %s
                    configs:
                      - source: rabbitmq_extra_config
                        target: /etc/rabbitmq/conf.d/99-extra.conf
                      - source: rabbitmq_enabled_plugins
                        target: /etc/rabbitmq/enabled_plugins
                    healthcheck:
                      test: ["CMD", "rabbitmq-diagnostics", "-q", "ping"]
                      interval: 30s
                      timeout: 30s
                      retries: 5
                      start_period: 60s
                    networks:
                      - reportportal
                    restart: always
                  migrations:
                    image: %s
                    depends_on:
                      postgres:
                        condition: service_healthy
                    environment:
                      POSTGRES_SERVER: postgres
                      POSTGRES_PORT: 5432
                      POSTGRES_DB: reportportal
                      POSTGRES_USER: rpuser
                      POSTGRES_PASSWORD: %s
                    networks:
                      - reportportal
                    restart: on-failure
                  index:
                    image: %s
                    depends_on:
                      gateway:
                        condition: service_started
                    environment:
                      LB_URL: http://gateway:8081
                      TRAEFIK_V2_MODE: 'true'
                    labels:
                      - "traefik.http.routers.index.rule=PathPrefix(`/`)"
                      - "traefik.http.services.index.loadbalancer.server.port=8080"
                      - "traefik.expose=true"
                    networks:
                      - reportportal
                    restart: always
                  ui:
                    image: %s
                    environment:
                      RP_SERVER_PORT: "8080"
                    labels:
                      - "traefik.http.middlewares.ui-strip-prefix.stripprefix.prefixes=/ui"
                      - "traefik.http.routers.ui.middlewares=ui-strip-prefix@docker"
                      - "traefik.http.routers.ui.rule=PathPrefix(`/ui`)"
                      - "traefik.http.services.ui.loadbalancer.server.port=8080"
                      - "traefik.expose=true"
                    networks:
                      - reportportal
                    restart: always
                  api:
                    image: %s
                    depends_on:
                      rabbitmq:
                        condition: service_healthy
                      gateway:
                        condition: service_started
                      postgres:
                        condition: service_healthy
                      jobs:
                        condition: service_healthy
                    environment:
                      RP_DB_HOST: postgres
                      RP_DB_PORT: 5432
                      RP_DB_USER: rpuser
                      RP_DB_PASS: %s
                      RP_DB_NAME: reportportal
                      RP_AMQP_HOST: rabbitmq
                      RP_AMQP_PORT: 5672
                      RP_AMQP_APIPORT: 15672
                      RP_AMQP_USER: rabbitmq
                      RP_AMQP_PASS: %s
                      RP_AMQP_APIUSER: rabbitmq
                      RP_AMQP_APIPASS: %s
                      RP_JOBS_BASEURL: http://jobs:8686
                      DATASTORE_TYPE: filesystem
                      MANAGEMENT_HEALTH_ELASTICSEARCH_ENABLED: "false"
                    volumes:
                      - storage:/data/storage
                    labels:
                      - "traefik.http.middlewares.api-strip-prefix.stripprefix.prefixes=/api"
                      - "traefik.http.routers.api.middlewares=api-strip-prefix@docker"
                      - "traefik.http.routers.api.rule=PathPrefix(`/api`)"
                      - "traefik.http.services.api.loadbalancer.server.port=8585"
                      - "traefik.expose=true"
                    networks:
                      - reportportal
                    restart: always
                  uat:
                    image: %s
                    environment:
                      RP_DB_HOST: postgres
                      RP_DB_PORT: 5432
                      RP_DB_USER: rpuser
                      RP_DB_PASS: %s
                      RP_DB_NAME: reportportal
                      RP_AMQP_HOST: rabbitmq
                      RP_AMQP_PORT: 5672
                      RP_AMQP_USER: rabbitmq
                      RP_AMQP_PASS: %s
                      RP_INITIAL_ADMIN_PASSWORD: %s
                    volumes:
                      - storage:/data/storage
                    labels:
                      - "traefik.http.middlewares.uat-strip-prefix.stripprefix.prefixes=/uat"
                      - "traefik.http.routers.uat.middlewares=uat-strip-prefix@docker"
                      - "traefik.http.routers.uat.rule=PathPrefix(`/uat`)"
                      - "traefik.http.services.uat.loadbalancer.server.port=9999"
                      - "traefik.expose=true"
                    depends_on:
                      postgres:
                        condition: service_healthy
                    networks:
                      - reportportal
                    restart: always
                  jobs:
                    image: %s
                    depends_on:
                      rabbitmq:
                        condition: service_healthy
                      gateway:
                        condition: service_started
                      postgres:
                        condition: service_healthy
                    environment:
                      RP_DB_HOST: postgres
                      RP_DB_PORT: 5432
                      RP_DB_USER: rpuser
                      RP_DB_PASS: %s
                      RP_DB_NAME: reportportal
                      RP_AMQP_HOST: rabbitmq
                      RP_AMQP_PORT: 5672
                      RP_AMQP_USER: rabbitmq
                      RP_AMQP_PASS: %s
                      DATASTORE_TYPE: filesystem
                    volumes:
                      - storage:/data/storage
                    healthcheck:
                      test: ["CMD-SHELL", "curl -f http://localhost:8686/health || wget -q --spider http://localhost:8686/health || exit 1"]
                      interval: 30s
                      timeout: 10s
                      retries: 3
                      start_period: 60s
                    labels:
                      - "traefik.http.middlewares.jobs-strip-prefix.stripprefix.prefixes=/jobs"
                      - "traefik.http.routers.jobs.middlewares=jobs-strip-prefix@docker"
                      - "traefik.http.routers.jobs.rule=PathPrefix(`/jobs`)"
                      - "traefik.http.services.jobs.loadbalancer.server.port=8686"
                      - "traefik.expose=true"
                    networks:
                      - reportportal
                    restart: always
                configs:
                  rabbitmq_extra_config:
                    content: |
                      deprecated_features.permit.queue_master_locator = true
                      disk_free_limit.absolute = 50MB
                  rabbitmq_enabled_plugins:
                    content: |
                      [rabbitmq_management,rabbitmq_consistent_hash_exchange,rabbitmq_auth_backend_ldap,rabbitmq_shovel,rabbitmq_shovel_management].
                volumes:
                  storage:
                  postgres:
                networks:
                  reportportal:
                """.formatted(GATEWAY_IMAGE, scale.uiPort(), POSTGRES_IMAGE, LOCAL_DEV_DB_PASSWORD,
                RABBITMQ_IMAGE, LOCAL_DEV_AMQP_PASSWORD, MIGRATIONS_IMAGE, LOCAL_DEV_DB_PASSWORD,
                INDEX_IMAGE, UI_IMAGE, API_IMAGE, LOCAL_DEV_DB_PASSWORD, LOCAL_DEV_AMQP_PASSWORD,
                LOCAL_DEV_AMQP_PASSWORD, UAT_IMAGE, LOCAL_DEV_DB_PASSWORD, LOCAL_DEV_AMQP_PASSWORD,
                LOCAL_DEV_ADMIN_PASSWORD, JOBS_IMAGE, LOCAL_DEV_DB_PASSWORD, LOCAL_DEV_AMQP_PASSWORD);
    }

    static ReportPortalScale scale(SetupSelection selection) {
        int ui = selected(selection, "ui_", DEFAULT_UI_PORT, 1024, 65535, "ReportPortal UI port");
        for (String component : selection.components()) {
            if (!component.startsWith("ui_")) {
                throw new IllegalArgumentException("Unsupported ReportPortal component: " + component);
            }
        }
        return new ReportPortalScale(ui);
    }

    static SetupSelection selectionFromScale(ReportPortalScale scale) {
        if (scale.uiPort() == DEFAULT_UI_PORT) return SetupSelection.defaults();
        return new SetupSelection(List.of("ui_" + scale.uiPort()));
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
        return selected(new SetupSelection(List.of("ui_" + value)), "ui_", DEFAULT_UI_PORT, 1024, 65535,
                "ReportPortal UI port");
    }

    private static String spec(ReportPortalScale scale) {
        return PIN + ",ui=" + scale.uiPort();
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

    record ReportPortalScale(int uiPort) { }
}
