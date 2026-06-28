package testPackage.unitTests;

import com.shaft.cli.SshConnectionOptions;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.file.Path;

public class SshConnectionOptionsUnitTest {
    @Test(description = "Builder should default strict host key checking to true when known_hosts is provided")
    public void builderShouldEnableStrictHostKeyCheckingWhenKnownHostsProvided() {
        SshConnectionOptions options = SshConnectionOptions.builder()
                .host("host.example.com")
                .username("user")
                .password("secret")
                .knownHosts(Path.of("known_hosts"))
                .build();

        Assert.assertTrue(options.isStrictHostKeyChecking());
    }

    @Test(description = "Builder should reject blank host values")
    public void builderShouldRejectBlankHost() {
        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                () -> SshConnectionOptions.builder()
                        .host(" ")
                        .username("user")
                        .password("secret")
                        .build());
        Assert.assertTrue(failure.getMessage().contains("host"));
    }

    @Test(description = "Builder should reject strict host key checking without known_hosts")
    public void builderShouldRejectStrictHostKeyCheckingWithoutKnownHosts() {
        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                () -> SshConnectionOptions.builder()
                        .host("host.example.com")
                        .username("user")
                        .password("secret")
                        .strictHostKeyChecking(true)
                        .build());
        Assert.assertTrue(failure.getMessage().contains("known_hosts"));
    }

    @Test(description = "Builder should require at least one authentication method")
    public void builderShouldRequireAuthenticationMethod() {
        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                () -> SshConnectionOptions.builder()
                        .host("host.example.com")
                        .username("user")
                        .build());
        Assert.assertTrue(failure.getMessage().contains("private key"));
    }

    @Test(description = "Redacted description should not expose password or passphrase values")
    public void redactedDescriptionShouldNotExposeSecrets() {
        SshConnectionOptions options = SshConnectionOptions.builder()
                .host("host.example.com")
                .username("user")
                .password("super-secret")
                .privateKey(Path.of("id_rsa"))
                .privateKeyPassphrase("phrase-secret")
                .build();

        String description = options.toRedactedDescription();
        Assert.assertFalse(description.contains("super-secret"));
        Assert.assertFalse(description.contains("phrase-secret"));
        Assert.assertTrue(description.contains("password=***"));
        Assert.assertTrue(description.contains("privateKeyPassphrase=***"));
    }

    @Test(description = "Keyboard-interactive handler should satisfy authentication requirement")
    public void keyboardInteractiveHandlerShouldSatisfyAuthenticationRequirement() {
        SshConnectionOptions.KeyboardInteractive handler = (destination, name, instruction, prompt, echo) -> new String[]{"response"};
        SshConnectionOptions options = SshConnectionOptions.builder()
                .host("host.example.com")
                .username("user")
                .keyboardInteractive(handler)
                .build();

        Assert.assertSame(handler, options.getKeyboardInteractive());
    }
}
