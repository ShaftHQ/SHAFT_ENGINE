package testPackage.unitTests;

import com.shaft.cli.internal.SshOutputRedactor;
import com.shaft.cli.internal.SshUserInfoBridge;
import org.testng.Assert;
import org.testng.annotations.Test;

public class SshExtensionsUnitTest {

    @Test
    public void redactorLeavesTextWhenRegexBlank() {
        Assert.assertEquals(SshOutputRedactor.apply("secret=abc", ""), "secret=abc");
        Assert.assertEquals(SshOutputRedactor.apply("secret=abc", "   "), "secret=abc");
    }

    @Test
    public void redactorReplacesMatches() {
        String out = SshOutputRedactor.apply("token=xyz", "token=\\w+");
        Assert.assertTrue(out.contains("[REDACTED]"));
        Assert.assertFalse(out.contains("xyz"));
    }

    @Test
    public void redactorInvalidRegexReturnsOriginal() {
        String raw = "a[b";
        Assert.assertSame(SshOutputRedactor.apply(raw, "["), raw);
    }

    @Test
    public void keyboardInteractiveReturnsNullWithoutPassword() {
        var bridge = new SshUserInfoBridge("", null);
        Assert.assertNull(bridge.promptKeyboardInteractive(null, null, null, new String[] {"Password:"}, new boolean[] {false}));
    }

    @Test
    public void keyboardInteractiveSuppliesPasswordForEachPrompt() {
        var bridge = new SshUserInfoBridge("pw", null);
        String[] r = bridge.promptKeyboardInteractive(null, null, null, new String[] {"One:", "Two:"},
                new boolean[] {false, false});
        Assert.assertNotNull(r);
        Assert.assertEquals(r.length, 2);
        Assert.assertEquals(r[0], "pw");
        Assert.assertEquals(r[1], "pw");
    }
}
