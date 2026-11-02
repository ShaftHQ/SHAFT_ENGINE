package com.shaft.gui.internal.aria;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class AriaSnapshotHelperUnitTest {

    @Test(description = "serialize/parse should round-trip a nested forest with names and leaf-only nodes")
    public void serializeAndParseShouldRoundTrip() {
        List<AriaNode> forest = List.of(
                new AriaNode("navigation", "", List.of(
                        new AriaNode("link", "Home", List.of()),
                        new AriaNode("link", "Docs", List.of())
                )),
                new AriaNode("heading", "Welcome", List.of()),
                new AriaNode("button", "Sign in", List.of())
        );

        String yaml = AriaSnapshotHelper.serialize(forest);
        List<AriaNode> roundTripped = AriaSnapshotHelper.parse(yaml);

        Assert.assertEquals(roundTripped, forest);
    }

    @Test(description = "serialize should quote-escape names containing quotes and backslashes")
    public void serializeShouldEscapeSpecialCharactersInNames() {
        List<AriaNode> forest = List.of(new AriaNode("button", "Say \"hi\" \\ bye", List.of()));

        String yaml = AriaSnapshotHelper.serialize(forest);
        List<AriaNode> roundTripped = AriaSnapshotHelper.parse(yaml);

        Assert.assertEquals(roundTripped, forest);
    }

    @Test(description = "parse should return an empty forest for blank input")
    public void parseShouldReturnEmptyForestForBlankInput() {
        Assert.assertTrue(AriaSnapshotHelper.parse("").isEmpty());
        Assert.assertTrue(AriaSnapshotHelper.parse(null).isEmpty());
    }

    @Test(description = "match should pass when the baseline is an exact match of the actual snapshot")
    public void matchShouldPassForExactMatch() {
        String yaml = AriaSnapshotHelper.serialize(List.of(
                new AriaNode("navigation", "", List.of(new AriaNode("link", "Home", List.of())))
        ));

        var result = AriaSnapshotHelper.match(yaml, yaml);

        Assert.assertTrue(result.matched());
    }

    @Test(description = "match should pass when the actual snapshot has extra sibling and child nodes at the same depth as the baseline")
    public void matchShouldPassWhenActualHasExtraNodes() {
        String baseline = AriaSnapshotHelper.serialize(List.of(
                new AriaNode("navigation", "", List.of(new AriaNode("link", "Home", List.of())))
        ));
        String actual = AriaSnapshotHelper.serialize(List.of(
                new AriaNode("navigation", "", List.of(
                        new AriaNode("link", "Home", List.of()),
                        new AriaNode("link", "Docs", List.of())
                )),
                new AriaNode("button", "Sign in", List.of())
        ));

        var result = AriaSnapshotHelper.match(baseline, actual);

        Assert.assertTrue(result.matched());
    }

    @Test(description = "match should pass when the baseline name is blank, matching any actual name")
    public void matchShouldTreatBlankBaselineNameAsWildcard() {
        String baseline = AriaSnapshotHelper.serialize(List.of(new AriaNode("heading", "", List.of())));
        String actual = AriaSnapshotHelper.serialize(List.of(new AriaNode("heading", "Anything", List.of())));

        var result = AriaSnapshotHelper.match(baseline, actual);

        Assert.assertTrue(result.matched());
    }

    @Test(description = "match should fail with a readable diff when a baseline node is missing from the actual snapshot")
    public void matchShouldFailWhenBaselineNodeIsMissing() {
        String baseline = AriaSnapshotHelper.serialize(List.of(new AriaNode("button", "Sign in", List.of())));
        String actual = AriaSnapshotHelper.serialize(List.of(new AriaNode("link", "Home", List.of())));

        var result = AriaSnapshotHelper.match(baseline, actual);

        Assert.assertFalse(result.matched());
        Assert.assertTrue(result.diffMessage().contains("button \"Sign in\""));
    }

    @Test(description = "match should fail on a role mismatch even when the name matches")
    public void matchShouldFailOnRoleMismatch() {
        String baseline = AriaSnapshotHelper.serialize(List.of(new AriaNode("button", "Home", List.of())));
        String actual = AriaSnapshotHelper.serialize(List.of(new AriaNode("link", "Home", List.of())));

        var result = AriaSnapshotHelper.match(baseline, actual);

        Assert.assertFalse(result.matched());
    }

    @Test(description = "match should fail when a required child node is absent from the matched actual node's children")
    public void matchShouldFailWhenChildNodeIsMissing() {
        String baseline = AriaSnapshotHelper.serialize(List.of(
                new AriaNode("navigation", "", List.of(new AriaNode("link", "Docs", List.of())))
        ));
        String actual = AriaSnapshotHelper.serialize(List.of(
                new AriaNode("navigation", "", List.of(new AriaNode("link", "Home", List.of())))
        ));

        var result = AriaSnapshotHelper.match(baseline, actual);

        Assert.assertFalse(result.matched());
    }
}
