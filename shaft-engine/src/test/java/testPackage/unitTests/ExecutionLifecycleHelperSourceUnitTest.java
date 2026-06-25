package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;

public class ExecutionLifecycleHelperSourceUnitTest {

    @Test
    public void engineTearDownShouldGenerateExecutionSummarySynchronously() throws Exception {
        String source = Files.readString(resolveSourcePath());

        SHAFT.Validations.assertThat().object(source)
                .contains("ExecutionSummaryReport.generateExecutionSummaryReport(")
                .perform();
        SHAFT.Validations.assertThat().object(source)
                .doesNotContain("Thread.ofVirtual().start(() -> ExecutionSummaryReport.generateExecutionSummaryReport(")
                .perform();
    }

    private Path resolveSourcePath() {
        Path modulePath = Path.of("src/main/java/com/shaft/listeners/internal/ExecutionLifecycleHelper.java");
        if (Files.exists(modulePath)) {
            return modulePath;
        }
        return Path.of("shaft-engine/src/main/java/com/shaft/listeners/internal/ExecutionLifecycleHelper.java");
    }
}
