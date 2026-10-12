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
package io.qameta.allure.cucumber8jvm;

import io.cucumber.messages.types.Feature;
import io.cucumber.plugin.event.TestCase;
import io.qameta.allure.SeverityLevel;

import java.util.Arrays;

/**
 * Parser for tags.
 */
class TagParser {
    private static final String FLAKY = "@FLAKY";
    private static final String KNOWN = "@KNOWN";
    private static final String MUTED = "@MUTED";

    private final Feature feature;
    private final TestCase scenario;

    TagParser(final Feature feature, final TestCase scenario) {
        this.feature = feature;
        this.scenario = scenario;
    }

    boolean isFlaky() {
        return getStatusDetailByTag(FLAKY);
    }

    boolean isMuted() {
        return getStatusDetailByTag(MUTED);
    }

    boolean isKnown() {
        return getStatusDetailByTag(KNOWN);
    }

    private boolean getStatusDetailByTag(final String tagName) {
        return scenario.getTags().stream()
                .anyMatch(tag -> tag.equalsIgnoreCase(tagName))
                || feature.getTags().stream()
                        .anyMatch(tag -> tag.getName().equalsIgnoreCase(tagName));
    }

    boolean isResultTag(final String tag) {
        return Arrays.asList(FLAKY, KNOWN, MUTED)
                .contains(tag.toUpperCase());
    }

    boolean isPureSeverityTag(final String tag) {
        return Arrays.stream(SeverityLevel.values())
                .map(SeverityLevel::value)
                .map(value -> "@" + value)
                .anyMatch(value -> value.equalsIgnoreCase(tag));
    }

}
