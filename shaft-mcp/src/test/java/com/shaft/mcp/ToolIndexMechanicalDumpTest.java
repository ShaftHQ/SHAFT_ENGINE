package com.shaft.mcp;

import io.modelcontextprotocol.server.McpServerFeatures;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContext;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Design doc amendment A6, "runtime index skew" drift gate: this test rebuilds the mechanical
 * half of the tool-index (names/services/descriptions/param schemas) straight from the live
 * Spring-registered {@code ToolCallback}/{@code @McpTool} beans -- via {@link ToolIndexDumper},
 * never a regex scan -- and asserts it is byte-for-byte the checked-in
 * {@code META-INF/shaft-mcp/tool-index-mechanical.json} resource. Any tool added, removed, or
 * schema-changed in shaft-mcp without regenerating that resource fails this test.
 *
 * <p>To regenerate the checked-in baseline after an intentional tool change, run:
 * {@code mvn -pl shaft-mcp test -Dtest=ToolIndexMechanicalDumpTest -Dshaft.toolIndex.write=true}
 * -- mirroring the existing {@code generate_shaft_skills_tool_catalog.py --check} vs. plain-run
 * convention used for the markdown catalog.
 */
@SpringBootTest(properties = "spring.ai.mcp.server.enabled=false")
class ToolIndexMechanicalDumpTest {

    private static final Path RESOURCE_PATH = Path.of(
            "src/main/resources/META-INF/shaft-mcp/tool-index-mechanical.json");

    @Autowired
    private ApplicationContext context;
    @Autowired
    private EngineService engineService;
    @Autowired
    private BrowserService browserService;
    @Autowired
    private ElementService elementService;
    @Autowired
    private MobileService mobileService;
    @Autowired
    private CaptureService captureService;
    @Autowired
    private DoctorService doctorService;
    @Autowired
    private TraceService traceService;
    @Autowired
    private HealerService healerService;
    @Autowired
    private GuideService guideService;
    @Autowired
    private ShaftProjectService shaftProjectService;
    @Autowired
    private TestAutomationService testAutomationService;
    @Autowired
    private CodingPartnerService codingPartnerService;
    @Autowired
    private AutobotService autobotService;
    @Autowired
    private PlannerService plannerService;

    @Test
    void liveToolSchemasMatchCheckedInMechanicalIndex() throws IOException {
        List<ToolIndexEntry> liveEntries = ToolIndexDumper.dump(
                List.of(engineService, browserService, elementService, mobileService, captureService,
                        doctorService, traceService, healerService, guideService, shaftProjectService,
                        testAutomationService, codingPartnerService, autobotService, plannerService),
                annotationScannedToolSpecs());

        ObjectMapper mapper = new ObjectMapper();
        ObjectNode liveDocument = mapper.createObjectNode();
        liveDocument.put("schemaVersion", "1.0");
        liveDocument.putPOJO("tools", liveEntries);
        String liveJson = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(liveDocument) + "\n";

        if (Boolean.getBoolean("shaft.toolIndex.write")) {
            Files.createDirectories(RESOURCE_PATH.getParent());
            Files.writeString(RESOURCE_PATH, liveJson, StandardCharsets.UTF_8);
            return;
        }

        String checkedInJson = readCheckedInMechanicalIndex();
        assertEquals(normalize(checkedInJson), normalize(liveJson),
                "shaft-mcp/src/main/resources/META-INF/shaft-mcp/tool-index-mechanical.json is out of "
                        + "date with the live Spring-registered tool schemas. Regenerate with: "
                        + "mvn -pl shaft-mcp test -Dtest=ToolIndexMechanicalDumpTest "
                        + "-Dshaft.toolIndex.write=true -- then re-run "
                        + "'python3 scripts/mcp/generate_tool_index.py' to refresh the merged index.");
    }

    private String readCheckedInMechanicalIndex() throws IOException {
        try (InputStream stream = getClass().getResourceAsStream("/META-INF/shaft-mcp/tool-index-mechanical.json")) {
            assertNotNull(stream, "tool-index-mechanical.json is missing from the classpath; generate it with "
                    + "-Dshaft.toolIndex.write=true (see this test's Javadoc)");
            return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    /** CRLF-insensitive: Windows checkouts materialize the committed file with CRLF line endings. */
    private static String normalize(String json) {
        return json.replace("\r\n", "\n");
    }

    @SuppressWarnings("unchecked")
    private List<McpServerFeatures.SyncToolSpecification> annotationScannedToolSpecs() {
        return (List<McpServerFeatures.SyncToolSpecification>) context.getBean("toolSpecs", List.class);
    }
}
