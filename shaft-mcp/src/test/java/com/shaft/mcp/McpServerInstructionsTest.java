package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.springframework.ai.mcp.server.common.autoconfigure.properties.McpServerProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ClassPathResource;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #4239 P2.1 (finding F9): {@code shaft-skills/shaft-locator-design/SKILL.md} and
 * three sibling skill files state the correct locator policy, the record -> confirm-replay ->
 * generate -> verify ordering, per-surface tool routing, and how to read {@code successful}/
 * {@code status}, but nothing at runtime ever delivered any of it to an LLM client: no {@code
 * spring.ai.mcp.server.instructions} property existed in any {@code application*.properties}, and
 * no {@code McpServerCustomizer} bean populated it programmatically anywhere in {@code
 * shaft-mcp/src/main}. Spring AI's {@code McpServerAutoConfiguration} wires {@link
 * McpServerProperties#getInstructions()} straight into the served MCP {@code initialize} response,
 * so every MCP client (not just the IntelliJ plugin) receives this content for free.
 *
 * <p>Deliberately does NOT use {@code @SpringBootTest}: every other MCP bootstrap test in this
 * module (see {@link ShaftMcpApplicationTests}, {@code EngineServiceTest},
 * {@code ToolIndexMechanicalDumpTest}) passes {@code spring.ai.mcp.server.enabled=false} because a
 * fully-enabled context builds a real stdio transport that reads {@code System.in} -- exactly the
 * hang this test must avoid while still proving the property binds. {@link ApplicationContextRunner}
 * binds {@link McpServerProperties} from the real {@code application.properties} file's raw text
 * without ever constructing a transport or a {@code ShaftMcpApplication} context.
 */
class McpServerInstructionsTest {

    @Configuration
    @EnableConfigurationProperties(McpServerProperties.class)
    static class McpServerPropertiesTestConfig {
    }

    @Test
    void applicationPropertiesInstructionsDistillLocatorPolicyOrderingRoutingAndStatusSemantics() throws IOException {
        assertInstructionsFromPropertiesFile("application.properties");
    }

    @Test
    void applicationHttpPropertiesInstructionsDistillLocatorPolicyOrderingRoutingAndStatusSemantics()
            throws IOException {
        assertInstructionsFromPropertiesFile("application-http.properties");
    }

    private void assertInstructionsFromPropertiesFile(String classpathResource) throws IOException {
        new ApplicationContextRunner()
                .withUserConfiguration(McpServerPropertiesTestConfig.class)
                .withPropertyValues(loadPropertyValues(classpathResource))
                .run(context -> {
                    McpServerProperties properties = context.getBean(McpServerProperties.class);
                    String instructions = properties.getInstructions();

                    assertTrue(instructions != null && !instructions.isBlank(),
                            classpathResource + ": spring.ai.mcp.server.instructions must be populated: "
                                    + instructions);
                    assertTrue(instructions.contains("hasRole"),
                            classpathResource + ": instructions must state the locator policy "
                                    + "(SHAFT locator builder ARIA role hasRole(...)): " + instructions);
                    assertTrue(instructions.contains("capture_start") && instructions.contains("verify"),
                            classpathResource + ": instructions must state the record -> confirm-replay -> "
                                    + "generate -> verify ordering: " + instructions);
                    assertTrue(instructions.contains("browser_") && instructions.contains("mobile_"),
                            classpathResource + ": instructions must state per-surface tool routing "
                                    + "(web vs mobile tool families): " + instructions);
                    assertTrue(instructions.contains("SUCCESS") && instructions.contains("UNCONFIRMED"),
                            classpathResource + ": instructions must explain how to read successful/status "
                                    + "(SUCCESS vs UNCONFIRMED): " + instructions);
                });
    }

    private static String[] loadPropertyValues(String classpathResource) throws IOException {
        Properties fileProperties = new Properties();
        try (InputStream in = new ClassPathResource(classpathResource).getInputStream()) {
            fileProperties.load(in);
        }
        List<String> values = new ArrayList<>();
        for (String name : fileProperties.stringPropertyNames()) {
            values.add(name + "=" + fileProperties.getProperty(name));
        }
        return values.toArray(new String[0]);
    }
}
