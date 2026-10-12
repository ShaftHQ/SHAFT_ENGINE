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

import io.cucumber.gherkin.GherkinDialect;
import io.cucumber.gherkin.GherkinDialectProvider;
import io.cucumber.messages.types.Feature;
import io.cucumber.messages.types.Scenario;
import io.cucumber.plugin.event.TestSourceRead;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Compatibility proxy around Cucumber feature source storage.
 *
 * <p>The proxy hides version-specific Cucumber source model APIs from the reporting plugin. Integrations use it to add source-read events and resolve feature, scenario, and step keyword metadata during execution.</p>
 */
public class TestSourcesModelProxy {

    private final Map<URI, TestSourceRead> pathToReadEventMap = new HashMap<>();
    private final TestSourcesModel testSources;

    /**
     * Creates a test sources model proxy with default configuration.
     */
    public TestSourcesModelProxy() {
        this.testSources = new TestSourcesModel();
    }

    /**
     * Adds the test source read event.
     *
     * @param path the path to read from or write to
     * @param event the framework event to process
     */
    public void addTestSourceReadEvent(final URI path, final TestSourceRead event) {
        this.pathToReadEventMap.put(path, event);
        testSources.addTestSourceReadEvent(path, event);
    }

    /**
     * Returns the feature.
     *
     * @param path the path to read from or write to
     * @return the feature
     */
    public Feature getFeature(final URI path) {
        return testSources.getFeature(path);
    }

    /**
     * Returns the scenario definition.
     *
     * @param path the path to read from or write to
     * @param line the source line number to resolve
     * @return the scenario definition
     */
    public Scenario getScenarioDefinition(final URI path, final int line) {
        return TestSourcesModel.getScenarioDefinition(testSources.getAstNode(path, line));
    }

    /**
     * Returns the keyword from source.
     *
     * @param uri the feature file URI
     * @param stepLine the feature file line number of the step
     * @return the keyword from source
     */
    public String getKeywordFromSource(final URI uri, final int stepLine) {
        return this.getKeywordFromSourceInternal(uri, stepLine);
    }

    private String getKeywordFromSourceInternal(final URI uri, final int stepLine) {
        final Feature feature = getFeature(uri);
        if (feature != null) {
            final TestSourceRead event = this.getTestSourceReadEvent(uri);
            final String trimmedSourceLine = Objects.requireNonNull(event).getSource().split("\n")[stepLine - 1].trim();
            final GherkinDialect dialect = new GherkinDialectProvider(feature.getLanguage()).getDefaultDialect();
            for (String keyword : dialect.getStepKeywords()) {
                if (trimmedSourceLine.startsWith(keyword)) {
                    return keyword;
                }
            }
        }
        return "";
    }

    private TestSourceRead getTestSourceReadEvent(final URI uri) {
        if (this.pathToReadEventMap.containsKey(uri)) {
            return pathToReadEventMap.get(uri);
        }
        return null;
    }
}
