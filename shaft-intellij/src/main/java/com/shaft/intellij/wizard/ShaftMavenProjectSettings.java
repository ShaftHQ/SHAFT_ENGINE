package com.shaft.intellij.wizard;

import org.jetbrains.annotations.NotNull;

/** Mutable settings for the SHAFT Maven new-project / module wizard (W2.1). */
public final class ShaftMavenProjectSettings {
    private String groupId = "com.example";
    private String artifactId = "shaft-tests";
    private String version = "1.0.0-SNAPSHOT";
    private boolean openAssistantAfterCreate = true;

    public @NotNull String getGroupId() {
        return groupId;
    }

    public void setGroupId(@NotNull String groupId) {
        this.groupId = groupId.trim().isEmpty() ? "com.example" : groupId.trim();
    }

    public @NotNull String getArtifactId() {
        return artifactId;
    }

    public void setArtifactId(@NotNull String artifactId) {
        this.artifactId = artifactId.trim().isEmpty() ? "shaft-tests" : artifactId.trim();
    }

    public @NotNull String getVersion() {
        return version;
    }

    public void setVersion(@NotNull String version) {
        this.version = version.trim().isEmpty() ? "1.0.0-SNAPSHOT" : version.trim();
    }

    public boolean isOpenAssistantAfterCreate() {
        return openAssistantAfterCreate;
    }

    public void setOpenAssistantAfterCreate(boolean openAssistantAfterCreate) {
        this.openAssistantAfterCreate = openAssistantAfterCreate;
    }
}
