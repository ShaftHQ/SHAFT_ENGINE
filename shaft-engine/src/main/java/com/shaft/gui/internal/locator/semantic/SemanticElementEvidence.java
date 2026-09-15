package com.shaft.gui.internal.locator.semantic;

import org.openqa.selenium.By;

import java.util.Objects;
import java.util.Optional;
import java.util.OptionalInt;

/**
 * Inspected accessibility / DOM evidence used to choose a locator (issue #5457 FR-3).
 * Match counts come from inspection (live page, aria snapshot, or fixture) and are
 * required for uniqueness enforcement (FR-2).
 */
public final class SemanticElementEvidence {
    private final String role;
    private final String accessibleName;
    private final String label;
    private final String visibleText;
    private final String testId;
    private final String id;
    private final String name;
    private final String css;
    private final String xpath;
    private final int roleMatchCount;
    private final int accessibleNameMatchCount;
    private final int labelMatchCount;
    private final int textMatchCount;
    private final int testIdMatchCount;
    private final int idMatchCount;
    private final int nameMatchCount;
    private final int cssMatchCount;
    private final int xpathMatchCount;
    private final Integer scopedIndex;
    private final By scopeRoot;
    private final String inspectionNotes;

    private SemanticElementEvidence(Builder builder) {
        this.role = safe(builder.role);
        this.accessibleName = safe(builder.accessibleName);
        this.label = safe(builder.label);
        this.visibleText = safe(builder.visibleText);
        this.testId = safe(builder.testId);
        this.id = safe(builder.id);
        this.name = safe(builder.name);
        this.css = safe(builder.css);
        this.xpath = safe(builder.xpath);
        this.roleMatchCount = nonNeg(builder.roleMatchCount);
        this.accessibleNameMatchCount = nonNeg(builder.accessibleNameMatchCount);
        this.labelMatchCount = nonNeg(builder.labelMatchCount);
        this.textMatchCount = nonNeg(builder.textMatchCount);
        this.testIdMatchCount = nonNeg(builder.testIdMatchCount);
        this.idMatchCount = nonNeg(builder.idMatchCount);
        this.nameMatchCount = nonNeg(builder.nameMatchCount);
        this.cssMatchCount = nonNeg(builder.cssMatchCount);
        this.xpathMatchCount = nonNeg(builder.xpathMatchCount);
        this.scopedIndex = builder.scopedIndex;
        this.scopeRoot = builder.scopeRoot;
        this.inspectionNotes = safe(builder.inspectionNotes);
    }

    public static Builder builder() {
        return new Builder();
    }

    public String role() {
        return role;
    }

    public String accessibleName() {
        return accessibleName;
    }

    public String label() {
        return label;
    }

    public String visibleText() {
        return visibleText;
    }

    public String testId() {
        return testId;
    }

    public String id() {
        return id;
    }

    public String name() {
        return name;
    }

    public String css() {
        return css;
    }

    public String xpath() {
        return xpath;
    }

    public int matchCount(SemanticLocatorStrategy strategy) {
        return switch (strategy) {
            case ROLE -> roleMatchCount;
            case ACCESSIBLE_NAME -> accessibleNameMatchCount;
            case LABEL -> labelMatchCount;
            case TEXT -> textMatchCount;
            case TEST_ID -> testIdMatchCount;
            case ID -> idMatchCount;
            case NAME -> nameMatchCount;
            case CSS -> cssMatchCount;
            case XPATH -> xpathMatchCount;
        };
    }

    public OptionalInt scopedIndex() {
        return scopedIndex == null ? OptionalInt.empty() : OptionalInt.of(scopedIndex);
    }

    public Optional<By> scopeRoot() {
        return Optional.ofNullable(scopeRoot);
    }

    public String inspectionNotes() {
        return inspectionNotes;
    }

    public boolean hasSemanticSignal() {
        return (!role.isBlank() && !accessibleName.isBlank())
                || !accessibleName.isBlank()
                || !label.isBlank()
                || !visibleText.isBlank()
                || !testId.isBlank();
    }

    private static String safe(String value) {
        return Objects.requireNonNullElse(value, "");
    }

    private static int nonNeg(int value) {
        return Math.max(0, value);
    }

    public static final class Builder {
        private String role;
        private String accessibleName;
        private String label;
        private String visibleText;
        private String testId;
        private String id;
        private String name;
        private String css;
        private String xpath;
        private int roleMatchCount;
        private int accessibleNameMatchCount;
        private int labelMatchCount;
        private int textMatchCount;
        private int testIdMatchCount;
        private int idMatchCount;
        private int nameMatchCount;
        private int cssMatchCount;
        private int xpathMatchCount;
        private Integer scopedIndex;
        private By scopeRoot;
        private String inspectionNotes;

        public Builder role(String role, String accessibleName, int matchCount) {
            this.role = role;
            this.accessibleName = accessibleName;
            this.roleMatchCount = matchCount;
            return this;
        }

        public Builder accessibleName(String accessibleName, int matchCount) {
            this.accessibleName = accessibleName;
            this.accessibleNameMatchCount = matchCount;
            return this;
        }

        public Builder label(String label, int matchCount) {
            this.label = label;
            this.labelMatchCount = matchCount;
            return this;
        }

        public Builder visibleText(String visibleText, int matchCount) {
            this.visibleText = visibleText;
            this.textMatchCount = matchCount;
            return this;
        }

        public Builder testId(String testId, int matchCount) {
            this.testId = testId;
            this.testIdMatchCount = matchCount;
            return this;
        }

        public Builder id(String id, int matchCount) {
            this.id = id;
            this.idMatchCount = matchCount;
            return this;
        }

        public Builder name(String name, int matchCount) {
            this.name = name;
            this.nameMatchCount = matchCount;
            return this;
        }

        public Builder css(String css, int matchCount) {
            this.css = css;
            this.cssMatchCount = matchCount;
            return this;
        }

        public Builder xpath(String xpath, int matchCount) {
            this.xpath = xpath;
            this.xpathMatchCount = matchCount;
            return this;
        }

        /** Zero-based index within the (optionally scoped) match set. */
        public Builder scopedIndex(int index) {
            if (index < 0) {
                throw new IllegalArgumentException("scopedIndex must be >= 0");
            }
            this.scopedIndex = index;
            return this;
        }

        public Builder scopeRoot(By scopeRoot) {
            this.scopeRoot = scopeRoot;
            return this;
        }

        public Builder inspectionNotes(String notes) {
            this.inspectionNotes = notes;
            return this;
        }

        public SemanticElementEvidence build() {
            return new SemanticElementEvidence(this);
        }
    }
}
