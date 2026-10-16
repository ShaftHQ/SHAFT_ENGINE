package com.shaft.architecture;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

import com.tngtech.archunit.base.DescribedPredicate;
import com.tngtech.archunit.core.domain.Dependency;
import com.tngtech.archunit.core.domain.JavaClass;
import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.lang.ArchRule;
import com.tngtech.archunit.library.freeze.FreezingArchRule;
import org.testng.Assert;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.methods;
import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noClasses;

/**
 * #7006: architecture rules for shaft-engine. Package cycles are judged against a
 * small edge baseline ({@code architecture/cyclic-package-edges.txt}): a new cyclic
 * edge fails, a fixed one must be removed. The API rule is frozen in
 * {@code src/test/resources/archunit_store}. Store creation is disabled in {@code archunit.properties},
 * so an unfrozen rule cannot silently create its own baseline in CI.
 */
public class ArchitectureRulesUnitTest {
    private static final String ROOT = "com.shaft";
    private static final String BASELINE = "/architecture/cyclic-package-edges.txt";
    private JavaClasses production;

    @BeforeClass
    public void importProductionClasses() {
        production = new ClassFileImporter()
                .withImportOption(ImportOption.Predefined.DO_NOT_INCLUDE_TESTS)
                .withImportOption(ImportOption.Predefined.DO_NOT_INCLUDE_JARS)
                .importPackages(ROOT);
    }

    /**
     * Dependency edges between top-level {@code com.shaft.*} packages that sit on a cycle
     * (both ends in one strongly connected component), as sorted "from -> to" lines.
     */
    static SortedSet<String> cyclicEdges(JavaClasses classes) {
        Map<String, Set<String>> graph = new TreeMap<>();
        for (JavaClass origin : classes) {
            String from = topLevel(origin.getPackageName());
            if (from.isEmpty()) {
                continue;
            }
            graph.computeIfAbsent(from, k -> new TreeSet<>());
            for (Dependency dependency : origin.getDirectDependenciesFromSelf()) {
                String to = topLevel(dependency.getTargetClass().getPackageName());
                if (!to.isEmpty() && !to.equals(from)) {
                    graph.get(from).add(to);
                }
            }
        }
        return cyclicEdges(graph);
    }

    static SortedSet<String> cyclicEdges(Map<String, Set<String>> graph) {
        SortedSet<String> result = new TreeSet<>();
        graph.forEach((from, targets) -> targets.stream()
                .filter(to -> reaches(graph, to, from))
                .forEach(to -> result.add(from + " -> " + to)));
        return result;
    }

    private static boolean reaches(Map<String, Set<String>> graph, String start, String goal) {
        Deque<String> pending = new ArrayDeque<>(List.of(start));
        Set<String> seen = new HashSet<>();
        while (!pending.isEmpty()) {
            String node = pending.pop();
            if (node.equals(goal)) {
                return true;
            }
            if (seen.add(node)) {
                pending.addAll(graph.getOrDefault(node, Set.of()));
            }
        }
        return false;
    }

    static String topLevel(String packageName) {
        if (!packageName.startsWith(ROOT + ".")) {
            return "";
        }
        String rest = packageName.substring(ROOT.length() + 1);
        int dot = rest.indexOf('.');
        return dot < 0 ? rest : rest.substring(0, dot);
    }

    static SortedSet<String> readBaseline() throws IOException {
        try (InputStream in = ArchitectureRulesUnitTest.class.getResourceAsStream(BASELINE)) {
            Assert.assertNotNull(in, "missing " + BASELINE);
            return new String(in.readAllBytes(), StandardCharsets.UTF_8).lines()
                    .map(String::strip).filter(l -> !l.isEmpty() && !l.startsWith("#"))
                    .collect(Collectors.toCollection(TreeSet::new));
        }
    }

    static ArchRule publicApiDoesNotReturnInternalTypes() {
        DescribedPredicate<JavaClass> internal = DescribedPredicate.describe("an ..internal.. type",
                type -> type.getPackageName().contains(".internal"));
        return methods().that().arePublic()
                .and().areDeclaredInClassesThat().arePublic()
                .and().areDeclaredInClassesThat().resideOutsideOfPackage("..internal..")
                .should().notHaveRawReturnType(internal)
                .allowEmptyShould(true)
                .as("public API does not return internal types");
    }

    static ArchRule productionDoesNotDependOnTestPackages() {
        return noClasses().that().resideInAPackage("com.shaft..")
                .should().dependOnClassesThat().resideInAnyPackage("testPackage..", "com.shaft.test..")
                .allowEmptyShould(true)
                .as("production code does not depend on test packages");
    }

    @Test
    public void topLevelPackagesHaveNoNewCycles() throws IOException {
        SortedSet<String> added = new TreeSet<>(cyclicEdges(production));
        added.removeAll(readBaseline());
        Assert.assertTrue(added.isEmpty(), "new cyclic package dependencies (break the cycle; do not extend "
                + BASELINE + "): " + added);
    }

    @Test
    public void cycleBaselineHasNoStaleEdges() throws IOException {
        SortedSet<String> stale = new TreeSet<>(readBaseline());
        stale.removeAll(cyclicEdges(production));
        Assert.assertTrue(stale.isEmpty(), "cycles fixed; remove from " + BASELINE + ": " + stale);
    }

    @Test
    public void cycleDetectionFindsOnlyEdgesOnACycle() {
        Map<String, Set<String>> graph = Map.of("a", Set.of("b"), "b", Set.of("a", "c"), "c", Set.of());
        Assert.assertEquals(cyclicEdges(graph), new TreeSet<>(List.of("a -> b", "b -> a")));
        Assert.assertEquals(topLevel("com.shaft.gui.internal.x"), "gui");
        Assert.assertEquals(topLevel("org.other"), "");
    }

    @Test
    public void publicApiExposesNoNewInternalTypes() {
        FreezingArchRule.freeze(publicApiDoesNotReturnInternalTypes()).check(production);
    }

    @Test
    public void productionCodeNeverDependsOnTests() {
        productionDoesNotDependOnTestPackages().check(production);
    }
}
