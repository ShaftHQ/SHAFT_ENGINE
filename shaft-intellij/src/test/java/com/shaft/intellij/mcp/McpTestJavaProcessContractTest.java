package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpTestJavaProcessContractTest {
    @Test
    void fakeMcpProcessesDoNotPutTheFullTestClasspathOnTheWindowsCommandLine() throws IOException {
        Path packageRoot = Path.of("src/test/java/com/shaft/intellij/mcp");
        try (var sources = Files.list(packageRoot)) {
            for (Path sourceFile : sources.filter(path -> path.toString().endsWith(".java"))
                    .filter(path -> !path.getFileName().toString().startsWith("McpTestJavaProcess"))
                    .toList()) {
                String source = Files.readString(sourceFile);
                assertFalse(source.contains("System.getProperty(\"java.class.path\")"),
                        sourceFile.getFileName() + " must use the shared bounded Java subprocess fixture");
            }
        }
    }

    @Test
    void sharedArgumentFileBoundsTheCommandAndLaunchesTheRealFakeServer() throws Exception {
        List<String> command = McpTestJavaProcess.command(FakeMcpServer.class, "toolsList");

        assertEquals(4, command.size());
        assertTrue(command.get(1).startsWith("@"));
        assertFalse(command.contains(System.getProperty("java.class.path")));
        assertTrue(Files.readString(McpTestJavaProcess.classpathArgumentFile()).contains("-cp"));

        try (ShaftMcpStdioClient client = new ShaftMcpStdioClient(command, Path.of("."), Map.of())) {
            assertEquals("SHAFT MCP connection is ready.",
                    client.initializeOnly(java.time.Duration.ofSeconds(5)));
        }
    }
}
