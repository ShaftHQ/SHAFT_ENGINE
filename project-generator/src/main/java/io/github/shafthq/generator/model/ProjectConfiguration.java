package io.github.shafthq.generator.model;

public class ProjectConfiguration {
    private String groupId = "com.example";
    private String artifactId = "my-automation-project";
    private String version = "1.0.0";
    private String testRunner; // TestNG, JUnit, Cucumber
    private String targetPlatform; // API, Web GUI, Mobile GUI
    private String executionEnvironment; // Local, LambdaTest, BrowserStack
    private boolean includeGitHubWorkflow;
    private String workflowTrigger = "workflow_dispatch"; // push, pull_request, workflow_dispatch, schedule

    public ProjectConfiguration() {
    }

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public String getArtifactId() {
        return artifactId;
    }

    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getTestRunner() {
        return testRunner;
    }

    public void setTestRunner(String testRunner) {
        this.testRunner = testRunner;
    }

    public String getTargetPlatform() {
        return targetPlatform;
    }

    public void setTargetPlatform(String targetPlatform) {
        this.targetPlatform = targetPlatform;
    }

    public String getExecutionEnvironment() {
        return executionEnvironment;
    }

    public void setExecutionEnvironment(String executionEnvironment) {
        this.executionEnvironment = executionEnvironment;
    }

    public boolean isIncludeGitHubWorkflow() {
        return includeGitHubWorkflow;
    }

    public void setIncludeGitHubWorkflow(boolean includeGitHubWorkflow) {
        this.includeGitHubWorkflow = includeGitHubWorkflow;
    }

    public String getWorkflowTrigger() {
        return workflowTrigger;
    }

    public void setWorkflowTrigger(String workflowTrigger) {
        this.workflowTrigger = workflowTrigger;
    }

    public String getPackageName() {
        return groupId.replace("-", "").replace("_", "");
    }
}
