package testPackage.unitTests;

import io.qameta.allure.*;
import io.qameta.allure.model.Status;
import org.testng.SkipException;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;

/**
 * Validation suite demonstrating all four Allure 3 test result states.
 *
 * <ul>
 *   <li><b>PASSED</b>  — {@link #testPasses()}: a straightforward passing assertion with an image attachment.</li>
 *   <li><b>FAILED</b>  — {@link #testFails()}: intentionally fails an assertion to produce a FAILED Allure result.</li>
 *   <li><b>SKIPPED</b> — {@link #testIsSkipped()}: always throws {@link SkipException}.</li>
 *   <li><b>BROKEN</b>  — {@link #testIsBroken()}: throws an unexpected {@link RuntimeException}.</li>
 * </ul>
 *
 * <p>The FAILED and BROKEN tests are tagged with {@code groups = "allure3-visual-demo"} so they can
 * be excluded from CI using {@code -Dgroups=!allure3-visual-demo}.  To run <em>all four</em> tests
 * (for local visual validation of the Allure 3 report), execute:
 * <pre>
 *   mvn test -Dtest=Allure3ReportValidationTests -DgenerateAllureReportArchive=true
 * </pre>
 */
@Epic("Allure 3 Migration")
@Feature("Report Validation")
public class Allure3ReportValidationTests {

    /**
     * PASSED — verifies that basic Java arithmetic works and attaches a
     * small PNG screenshot to prove image attachments render correctly.
     */
    @Test(description = "This test should PASS and include a screenshot attachment")
    @Story("PASSED result with attachment")
    @Severity(SeverityLevel.CRITICAL)
    public void testPasses() {
        Allure.step("Perform a passing assertion", () -> {
            int result = 1 + 1;
            if (result != 2) throw new AssertionError("Expected 2 but got " + result);
        });

        Allure.step("Attach a sample screenshot", () ->
                Allure.addAttachment("screenshot.png", "image/png", new java.io.ByteArrayInputStream(screenshotBytes()), "png"));
    }

    /**
     * FAILED — asserts that 1 equals 2, which is intentionally false.
     * This produces a FAILED status in the Allure 3 report.
     *
     * <p><b>Note:</b> This test is tagged {@code allure3-visual-demo} so it can be excluded
     * from CI with {@code -Dgroups=!allure3-visual-demo}.
     */
    @Test(description = "This test should FAIL — intentional assertion failure",
            groups = "allure3-visual-demo")
    @Story("FAILED result")
    @Severity(SeverityLevel.NORMAL)
    public void testFails() {
        Allure.step("Assert that 1 equals 2 (this step will fail)", () -> {
            if (1 != 2) throw new AssertionError("Intentional failure: expected 2, got 1");
        });
    }

    /**
     * SKIPPED — always throws {@link SkipException} to produce a SKIPPED status.
     */
    @Test(description = "This test should be SKIPPED")
    @Story("SKIPPED result")
    @Severity(SeverityLevel.MINOR)
    public void testIsSkipped() {
        throw new SkipException("Intentional skip for Allure 3 visual validation");
    }

    /**
     * BROKEN — throws an unexpected {@link RuntimeException} (not an {@link AssertionError}),
     * which produces a BROKEN status in the Allure 3 report.
     *
     * <p><b>Note:</b> This test is tagged {@code allure3-visual-demo} so it can be excluded
     * from CI with {@code -Dgroups=!allure3-visual-demo}.
     */
    @Test(description = "This test should be BROKEN — unexpected exception",
            groups = "allure3-visual-demo")
    @Story("BROKEN result")
    @Severity(SeverityLevel.MINOR)
    public void testIsBroken() {
        throw new RuntimeException("Intentional broken state for Allure 3 visual validation");
    }

    // ─── helpers ─────────────────────────────────────────────────────────────

    /**
     * Generates a simple 400×100 PNG image as a byte array for use as an Allure attachment.
     */
    private static byte[] screenshotBytes() {
        try {
            BufferedImage img = new BufferedImage(400, 100, BufferedImage.TYPE_INT_RGB);
            Graphics2D g = img.createGraphics();
            g.setColor(new Color(30, 120, 200));
            g.fillRect(0, 0, 400, 100);
            g.setColor(Color.WHITE);
            g.setFont(new Font("SansSerif", Font.BOLD, 20));
            g.drawString("SHAFT + Allure 3  ✓  PASSED", 30, 58);
            g.dispose();
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ImageIO.write(img, "png", baos);
            return baos.toByteArray();
        } catch (Exception e) {
            return new byte[0];
        }
    }
}
