package com.shaft.heal.internal;

import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.model.HealingCandidate;
import com.shaft.heal.model.HealingPlatform;
import com.shaft.heal.model.HealingScore;
import com.shaft.heal.model.LocatorFingerprint;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

final class CandidateExtractor {
    private static final int MAX_CANDIDATES = 100;
    private final HealingConfiguration configuration;
    private final FingerprintExtractor fingerprintExtractor;
    private final DeterministicScorer scorer;

    CandidateExtractor(HealingConfiguration configuration) {
        this.configuration = Objects.requireNonNull(configuration, "configuration");
        this.fingerprintExtractor = new FingerprintExtractor(configuration);
        this.scorer = new DeterministicScorer(configuration);
    }

    List<RankedCandidate> extract(
            WebDriver driver,
            LocatorFingerprint original,
            By frameLocator,
            By shadowHostLocator) {
        Optional<SearchContext> searchContext = searchContext(
                driver, original.platform(), frameLocator, shadowHostLocator);
        if (searchContext.isEmpty()) {
            return List.of();
        }
        SearchContext context = searchContext.get();
        LinkedHashSet<WebElement> elements = new LinkedHashSet<>();
        for (By locator : discoveryLocators(original, shadowHostLocator != null)) {
            try {
                for (WebElement element : context.findElements(locator)) {
                    elements.add(element);
                    if (elements.size() >= MAX_CANDIDATES) {
                        break;
                    }
                }
            } catch (WebDriverException ignored) {
                // Unsupported selectors are skipped; remaining deterministic evidence is retained.
            }
            if (elements.size() >= MAX_CANDIDATES) {
                break;
            }
        }

        List<RankedCandidate> candidates = new ArrayList<>();
        for (WebElement element : elements) {
            LocatorFingerprint fingerprint = fingerprintExtractor.extract(driver, element);
            HealingScore score = scorer.score(original, fingerprint);
            if (score.deterministicScore() <= 0) {
                continue;
            }
            LocatorProposal proposal = proposeLocator(context, element, fingerprint);
            boolean visible = displayed(element);
            boolean interactable = visible && enabled(element);
            String candidateId = HealingSupport.sha256(
                    proposal.locator() + "\n" + fingerprint).substring(0, 16);
            List<String> evidence = score.evidenceScores().entrySet().stream()
                    .filter(item -> item.getValue() > 0)
                    .map(item -> item.getKey() + "=" + String.format(java.util.Locale.ROOT, "%.3f", item.getValue()))
                    .toList();
            HealingCandidate report = new HealingCandidate(
                    candidateId,
                    proposal.locator().toString(),
                    fingerprint,
                    score,
                    evidence,
                    proposal.unique(),
                    visible,
                    interactable,
                    fingerprint.platform() == original.platform());
            candidates.add(new RankedCandidate(element, proposal.locator(), report));
        }
        return candidates.stream()
                .sorted((left, right) -> {
                    int scoreOrder = Double.compare(
                            right.report().score().deterministicScore(),
                            left.report().score().deterministicScore());
                    return scoreOrder != 0
                            ? scoreOrder
                            : left.report().proposedLocator().compareTo(right.report().proposedLocator());
                })
                .toList();
    }

    List<RankedCandidate> extract(
            WebDriver driver,
            LocatorFingerprint original,
            By shadowHostLocator) {
        return extract(driver, original, null, shadowHostLocator);
    }

    private List<By> discoveryLocators(LocatorFingerprint fingerprint, boolean shadowContext) {
        if (fingerprint.platform().nativePlatform()) {
            return nativeDiscoveryLocators(fingerprint);
        }
        Set<By> locators = new LinkedHashSet<>();
        addAttribute(locators, "id", fingerprint.id());
        addAttribute(locators, "name", fingerprint.name());
        fingerprint.testIds().forEach((attribute, value) -> addAttribute(locators, attribute, value));
        addAttribute(locators, "aria-label", fingerprint.accessibleName());
        addAttribute(locators, "aria-label", fingerprint.semanticAttributes().get("aria-label"));
        addAttribute(locators, "placeholder", fingerprint.placeholder());
        addAttribute(locators, "title", fingerprint.title());
        addAttribute(locators, "role", fingerprint.role());
        if (!shadowContext && !fingerprint.associatedLabel().isBlank()) {
            String label = xpathLiteral(fingerprint.associatedLabel());
            locators.add(By.xpath("//*[@id = //label[normalize-space(.)=" + label + "]/@for]"));
            locators.add(By.xpath("//label[normalize-space(.)=" + label + "]//*"));
        }
        if (!shadowContext && !fingerprint.accessibleName().isBlank()) {
            locators.add(By.xpath("//*[normalize-space(.)=" + xpathLiteral(fingerprint.accessibleName()) + "]"));
        }
        if (!fingerprint.tagName().isBlank()) {
            locators.add(By.tagName(fingerprint.tagName()));
        }
        return List.copyOf(locators);
    }

    private LocatorProposal proposeLocator(
            SearchContext context,
            WebElement element,
            LocatorFingerprint fingerprint) {
        if (fingerprint.platform().nativePlatform()) {
            return proposeNativeLocator(context, element, fingerprint);
        }
        List<By> proposals = new ArrayList<>();
        fingerprint.testIds().forEach((attribute, value) -> proposals.add(attributeLocator(attribute, value)));
        if (!fingerprint.id().isBlank()) {
            proposals.add(By.id(fingerprint.id()));
        }
        if (!fingerprint.name().isBlank()) {
            proposals.add(By.name(fingerprint.name()));
        }
        addProposal(proposals, "aria-label", fingerprint.accessibleName());
        addProposal(proposals, "aria-label", fingerprint.semanticAttributes().get("aria-label"));
        addProposal(proposals, "placeholder", fingerprint.placeholder());
        addProposal(proposals, "title", fingerprint.title());
        for (By proposal : proposals) {
            try {
                List<WebElement> matches = context.findElements(proposal);
                if (matches.size() == 1 && matches.getFirst().equals(element)) {
                    return new LocatorProposal(proposal, true);
                }
            } catch (WebDriverException ignored) {
                // Try the next explainable proposal.
            }
        }
        By fallback = fingerprint.tagName().isBlank()
                ? By.cssSelector("*")
                : By.tagName(fingerprint.tagName());
        return new LocatorProposal(fallback, false);
    }

    private static Optional<SearchContext> searchContext(
            WebDriver driver,
            HealingPlatform platform,
            By frameLocator,
            By shadowHostLocator) {
        if (platform.nativePlatform()) {
            return Optional.of(driver);
        }
        try {
            driver.switchTo().defaultContent();
            SearchContext context = driver;
            if (frameLocator != null) {
                List<WebElement> frames = driver.findElements(frameLocator);
                if (frames.size() != 1) {
                    return Optional.empty();
                }
                context = driver.switchTo().frame(frames.getFirst());
            }
            if (shadowHostLocator != null) {
                List<WebElement> hosts = context.findElements(shadowHostLocator);
                if (hosts.size() != 1) {
                    return Optional.empty();
                }
                context = hosts.getFirst().getShadowRoot();
            }
            return Optional.of(context);
        } catch (RuntimeException exception) {
            return Optional.empty();
        }
    }

    private static List<By> nativeDiscoveryLocators(LocatorFingerprint fingerprint) {
        Set<By> locators = new LinkedHashSet<>();
        addNativeAccessibilityId(locators, fingerprint.accessibleName());
        addNativeAccessibilityId(locators, fingerprint.nativeAttributes().get("content-desc"));
        addNativeAccessibilityId(locators, fingerprint.nativeAttributes().get("label"));
        addNativeAccessibilityId(locators, fingerprint.name());
        addNativeId(locators, fingerprint.id());
        addNativeId(locators, fingerprint.nativeAttributes().get("resource-id"));
        addNativeClass(locators, fingerprint.nativeAttributes().get("class"));
        addNativeClass(locators, fingerprint.tagName());
        addNativeXPath(locators, "content-desc", fingerprint.nativeAttributes().get("content-desc"));
        addNativeXPath(locators, "name", fingerprint.nativeAttributes().get("name"));
        addNativeXPath(locators, "label", fingerprint.nativeAttributes().get("label"));
        addNativeXPath(locators, "resource-id", fingerprint.nativeAttributes().get("resource-id"));
        addNativeXPath(locators, "text", fingerprint.visibleText());
        return List.copyOf(locators);
    }

    private static LocatorProposal proposeNativeLocator(
            SearchContext context,
            WebElement element,
            LocatorFingerprint fingerprint) {
        List<By> proposals = new ArrayList<>();
        addNativeAccessibilityId(proposals, fingerprint.accessibleName());
        addNativeAccessibilityId(proposals, fingerprint.nativeAttributes().get("content-desc"));
        addNativeAccessibilityId(proposals, fingerprint.nativeAttributes().get("label"));
        addNativeId(proposals, fingerprint.id());
        addNativeId(proposals, fingerprint.nativeAttributes().get("resource-id"));
        addNativeXPath(proposals, "name", fingerprint.nativeAttributes().get("name"));
        addNativeXPath(proposals, "label", fingerprint.nativeAttributes().get("label"));
        addNativeXPath(proposals, "text", fingerprint.visibleText());
        for (By proposal : proposals) {
            try {
                List<WebElement> matches = context.findElements(proposal);
                if (matches.size() == 1 && matches.getFirst().equals(element)) {
                    return new LocatorProposal(proposal, true);
                }
            } catch (WebDriverException ignored) {
                // Try the next bounded accessibility locator.
            }
        }
        By fallback = fingerprint.tagName().isBlank()
                ? By.xpath("//*")
                : By.className(fingerprint.tagName());
        return new LocatorProposal(fallback, false);
    }

    private static void addAttribute(Set<By> locators, String attribute, String value) {
        if (value != null && !value.isBlank()) {
            locators.add(attributeLocator(attribute, value));
        }
    }

    private static void addNativeAccessibilityId(java.util.Collection<By> locators, String value) {
        if (value != null && !value.isBlank()) {
            locators.add(AppiumBy.accessibilityId(value));
        }
    }

    private static void addNativeId(java.util.Collection<By> locators, String value) {
        if (value != null && !value.isBlank()) {
            locators.add(By.id(value));
        }
    }

    private static void addNativeClass(java.util.Collection<By> locators, String value) {
        if (value != null && !value.isBlank()) {
            locators.add(By.className(value));
        }
    }

    private static void addNativeXPath(java.util.Collection<By> locators, String attribute, String value) {
        if (value != null && !value.isBlank()) {
            locators.add(By.xpath("//*[@" + attribute + "=" + xpathLiteral(value) + "]"));
        }
    }

    private static void addProposal(List<By> proposals, String attribute, String value) {
        if (value != null && !value.isBlank()) {
            proposals.add(attributeLocator(attribute, value));
        }
    }

    private static By attributeLocator(String attribute, String value) {
        String escaped = value.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\r", "\\d ")
                .replace("\n", "\\a ");
        return By.cssSelector("[" + attribute + "=\"" + escaped + "\"]");
    }

    private static String xpathLiteral(String value) {
        if (!value.contains("'")) {
            return "'" + value + "'";
        }
        if (!value.contains("\"")) {
            return "\"" + value + "\"";
        }
        String[] parts = value.split("'", -1);
        StringBuilder result = new StringBuilder("concat(");
        for (int index = 0; index < parts.length; index++) {
            if (index > 0) {
                result.append(", \"'\", ");
            }
            result.append("'").append(parts[index]).append("'");
        }
        return result.append(")").toString();
    }

    private static boolean displayed(WebElement element) {
        try {
            return element.isDisplayed();
        } catch (WebDriverException exception) {
            return false;
        }
    }

    private static boolean enabled(WebElement element) {
        try {
            return element.isEnabled();
        } catch (WebDriverException exception) {
            return false;
        }
    }

    private record LocatorProposal(By locator, boolean unique) {
    }
}
