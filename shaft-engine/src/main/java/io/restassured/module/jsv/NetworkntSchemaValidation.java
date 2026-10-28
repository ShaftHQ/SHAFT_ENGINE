/*
 * Copyright 2026 ShaftHQ / ChaosEngine.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.restassured.module.jsv;

import com.networknt.schema.Error;
import com.networknt.schema.Schema;
import com.networknt.schema.SchemaLocation;
import com.networknt.schema.SchemaRegistry;
import com.networknt.schema.SchemaRegistryConfig;
import com.networknt.schema.SpecificationVersion;
import com.networknt.schema.path.PathType;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Classpath override of REST Assured 6.3.0's {@code NetworkntSchemaValidation}.
 *
 * <p>REST Assured still passes {@code com.fasterxml.jackson.databind.JsonNode} into
 * networknt {@code SchemaRegistry.getSchema}. networknt 3.x only accepts
 * {@code tools.jackson.databind.JsonNode}. This copy converts Jackson 2 nodes to
 * Jackson 3 via JSON text so shaft-engine can share networknt 3.x with shaft-mcp
 * (#7423).
 */
class NetworkntSchemaValidation {

    private static final JsonMapper TOOLS_JSON = JsonMapper.builder().build();

    private static final Map<JsonSchemaVersion, SchemaRegistry> SCHEMA_REGISTRIES = new ConcurrentHashMap<>();

    private NetworkntSchemaValidation() {
    }

    /**
     * Entry point kept binary-compatible with REST Assured 6.3.0 (Jackson 2 nodes).
     */
    static List<String> validate(JsonSchemaVersion version,
                                 com.fasterxml.jackson.databind.JsonNode schemaNode,
                                 String schemaLocation,
                                 com.fasterxml.jackson.databind.JsonNode content) {
        return validateTools(version, toTools(schemaNode), schemaLocation, toTools(content));
    }

    private static List<String> validateTools(JsonSchemaVersion version, JsonNode schemaNode,
                                              String schemaLocation, JsonNode content) {
        SchemaRegistry schemaRegistry = SCHEMA_REGISTRIES.computeIfAbsent(version, NetworkntSchemaValidation::createSchemaRegistry);
        final List<Error> errors;
        try {
            Schema schema = schemaLocation == null
                    ? schemaRegistry.getSchema(schemaNode)
                    : schemaRegistry.getSchema(SchemaLocation.of(schemaLocation), schemaNode);
            errors = schema.validate(content);
        } catch (RuntimeException e) {
            if (schemaLocation == null && hasCause(e, "URI is not absolute")) {
                throw new JsonSchemaValidationException("Couldn't resolve a relative $ref since the schema wasn't loaded from a location. " +
                        "Use matchesJsonSchemaInClasspath, or matchesJsonSchema with a File, URL or URI, so that relative $ref's can be resolved against the location of the schema.", e);
            }
            throw e;
        }
        List<String> messages = new ArrayList<>(errors.size());
        for (Error error : errors) {
            messages.add(error.toString());
        }
        return messages;
    }

    private static JsonNode toTools(com.fasterxml.jackson.databind.JsonNode node) {
        if (node == null) {
            return null;
        }
        return TOOLS_JSON.readTree(node.toString());
    }

    private static SchemaRegistry createSchemaRegistry(JsonSchemaVersion version) {
        SchemaRegistryConfig config = SchemaRegistryConfig.builder()
                .pathType(PathType.JSON_PATH)
                .locale(Locale.ENGLISH)
                .build();
        return SchemaRegistry.withDefaultDialect(SpecificationVersion.valueOf(version.name()), builder -> builder
                .schemaRegistryConfig(config)
                .schemaLoader(schemaLoader -> schemaLoader.fetchRemoteResources()));
    }

    private static boolean hasCause(Throwable throwable, String messagePart) {
        for (Throwable t = throwable; t != null; t = t.getCause() == t ? null : t.getCause()) {
            if (t.getMessage() != null && t.getMessage().contains(messagePart)) {
                return true;
            }
        }
        return false;
    }
}
