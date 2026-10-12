/*
 *  Copyright 2016-2026 Qameta Software Inc
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package io.qameta.allure.cucumber8jvm.testsourcemodel;

import io.cucumber.gherkin.GherkinParser;
import io.cucumber.messages.types.Background;
import io.cucumber.messages.types.Envelope;
import io.cucumber.messages.types.Examples;
import io.cucumber.messages.types.Feature;
import io.cucumber.messages.types.FeatureChild;
import io.cucumber.messages.types.GherkinDocument;
import io.cucumber.messages.types.RuleChild;
import io.cucumber.messages.types.Scenario;
import io.cucumber.messages.types.Source;
import io.cucumber.messages.types.SourceMediaType;
import io.cucumber.messages.types.Step;
import io.cucumber.messages.types.TableRow;
import io.cucumber.plugin.event.TestSourceRead;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

final class TestSourcesModel {
    private final Map<URI, TestSourceRead> pathToReadEventMap = new HashMap<>();
    private final Map<URI, GherkinDocument> pathToAstMap = new HashMap<>();
    private final Map<URI, Map<Integer, AstNode>> pathToNodeMap = new HashMap<>();

    static Scenario getScenarioDefinition(final AstNode astNode) {
        AstNode candidate = astNode;
        while (candidate != null && !(candidate.node instanceof Scenario)) {
            candidate = candidate.parent;
        }
        return candidate == null ? null : (Scenario) candidate.node;
    }

    void addTestSourceReadEvent(final URI path, final TestSourceRead event) {
        pathToReadEventMap.put(path, event);
    }

    Feature getFeature(final URI path) {
        if (!pathToAstMap.containsKey(path)) {
            parseGherkinSource(path);
        }
        if (pathToAstMap.containsKey(path)) {
            return pathToAstMap.get(path).getFeature().orElse(null);
        }
        return null;
    }

    private void parseGherkinSource(final URI path) {
        if (!pathToReadEventMap.containsKey(path)) {
            return;
        }
        final String source = pathToReadEventMap.get(path).getSource();

        final GherkinParser parser = GherkinParser.builder()
                .build();

        final Stream<Envelope> envelopes = parser.parse(
                Envelope.of(new Source(path.toString(), source, SourceMediaType.TEXT_X_CUCUMBER_GHERKIN_PLAIN))
        );

        // TODO: What about empty gherkin docs?
        final GherkinDocument gherkinDocument = envelopes
                .map(Envelope::getGherkinDocument)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .findFirst()
                .orElse(null);

        pathToAstMap.put(path, gherkinDocument);
        final Map<Integer, AstNode> nodeMap = new HashMap<>();

        // TODO: What about gherkin docs with no features?
        final Feature feature = gherkinDocument.getFeature().get();
        final AstNode currentParent = new AstNode(feature, null);
        for (FeatureChild child : feature.getChildren()) {
            processFeatureDefinition(nodeMap, child, currentParent);
        }
        pathToNodeMap.put(path, nodeMap);

    }

    private void processFeatureDefinition(
                                          final Map<Integer, AstNode> nodeMap, final FeatureChild child, final AstNode currentParent) {
        child.getBackground().ifPresent(background -> processBackgroundDefinition(nodeMap, background, currentParent));
        child.getScenario().ifPresent(scenario -> processScenarioDefinition(nodeMap, scenario, currentParent));
        child.getRule().ifPresent(rule -> {
            final AstNode childNode = new AstNode(rule, currentParent);
            nodeMap.put(messageLine(rule.getLocation()), childNode);
            rule.getChildren().forEach(ruleChild -> processRuleDefinition(nodeMap, ruleChild, childNode));
        });
    }

    private void processBackgroundDefinition(
                                             final Map<Integer, AstNode> nodeMap, final Background background, final AstNode currentParent) {
        final AstNode childNode = createAstNode(background, currentParent);
        nodeMap.put(messageLine(background.getLocation()), childNode);
        for (Step step : background.getSteps()) {
            nodeMap.put(messageLine(step.getLocation()), createAstNode(step, childNode));
        }
    }

    private void processScenarioDefinition(
                                           final Map<Integer, AstNode> nodeMap, final Scenario child, final AstNode currentParent) {
        final AstNode childNode = createAstNode(child, currentParent);
        nodeMap.put(messageLine(child.getLocation()), childNode);
        for (Step step : child.getSteps()) {
            nodeMap.put(messageLine(step.getLocation()), createAstNode(step, childNode));
        }
        if (!child.getExamples().isEmpty()) {
            processScenarioOutlineExamples(nodeMap, child, childNode);
        }
    }

    private void processRuleDefinition(
                                       final Map<Integer, AstNode> nodeMap, final RuleChild child, final AstNode currentParent) {
        child.getBackground().ifPresent(background -> processBackgroundDefinition(nodeMap, background, currentParent));
        child.getScenario().ifPresent(scenario -> processScenarioDefinition(nodeMap, scenario, currentParent));
    }

    private void processScenarioOutlineExamples(
                                                final Map<Integer, AstNode> nodeMap, final Scenario scenarioOutline, final AstNode parent) {
        for (Examples examples : scenarioOutline.getExamples()) {
            final AstNode examplesNode = createAstNode(examples, parent);
            // TODO: Can tables without headers even exist?
            final TableRow headerRow = examples.getTableHeader().get();
            final AstNode headerNode = createAstNode(headerRow, examplesNode);
            nodeMap.put(messageLine(headerRow.getLocation()), headerNode);
            for (int i = 0; i < examples.getTableBody().size(); ++i) {
                final TableRow examplesRow = examples.getTableBody().get(i);
                final AstNode expandedScenarioNode = createAstNode(examplesRow, examplesNode);
                nodeMap.put(messageLine(examplesRow.getLocation()), expandedScenarioNode);
            }
        }
    }

    /**
     * Cucumber messages v34 types {@code Location.line} as a Java {@code int}.
     * Calling the cucumber 7 {@code Long getLine()} descriptor throws
     * {@code NoSuchMethodError} on this runtime.
     */
    private static int messageLine(final io.cucumber.messages.types.Location location) {
        return location.getLine();
    }

    AstNode getAstNode(final URI path, final int line) {
        if (!pathToNodeMap.containsKey(path)) {
            parseGherkinSource(path);
        }
        if (pathToNodeMap.containsKey(path)) {
            return pathToNodeMap.get(path).get(line);
        }
        return null;
    }

    private AstNode createAstNode(final Object node, final AstNode astNode) {
        return new AstNode(node, astNode);
    }

    /**
     * A node of the parsed Gherkin document tree, linked to its parent for upward traversal.
     */
    private static class AstNode {
        private final Object node;
        private final AstNode parent;

        AstNode(final Object node, final AstNode parent) {
            this.node = node;
            this.parent = parent;
        }
    }
}
