# SHAFT MCP - GitHub Copilot Instructions

## Project Overview

SHAFT MCP is a **Model Context Protocol (MCP) server** that enables Claude Desktop to perform web automation tasks using the SHAFT Engine (a Selenium-based test automation framework). The project is built with:

- **Language**: Java 25 (Temurin 25 LTS or newer)
- **Framework**: Spring Boot 4.0.5 with Spring AI 1.1.4
- **Build Tool**: Maven 3.x
- **Testing Framework**: JUnit Jupiter (JUnit 5) via TestNG-compatible surefire profile
- **Web Automation**: SHAFT Engine 10.2.20260422 (Selenium-based)
- **Distribution**: JAR file, Docker image, Maven Central

## Technology Stack

- Spring Boot for application framework
- Spring AI for MCP server implementation (STDIO and HTTP/SSE transports)
- SHAFT Engine for browser automation (Selenium WebDriver)
- Allure for test reporting
- SLF4J for logging

## Build & Test Commands

### Build the Project
```bash
mvn clean package -DskipTests -Dgpg.skip
```
This creates the JAR file in `target/SHAFT_MCP-10.2.20260422.jar`

### Run Tests (requires Selenium Grid)
```bash
# Pre-create the properties directory to prevent SHAFT Engine IOException
mkdir -p src/main/resources/properties
# Point to a running Selenium Grid
REMOTE_DRIVER_ADDRESS=http://localhost:4444/wd/hub mvn clean test -Pjunit
```
Tests **must** be run with the `junit` Maven profile (`-Pjunit`). The CI integration test
workflow (`integration-test.yml`) spins up a `selenium/standalone-chrome:latest` container
on port 4444 before running the tests.

**IMPORTANT — run tests before any code merge:**
Tests must be executed and confirmed passing before merging code changes.
If tests fail, iterate to fix the problem and re-run. Keep iterating until all tests pass.

### Full Build with Tests
```bash
mvn clean install
```

### Run the Application Locally (for testing)
```bash
java -jar target/SHAFT_MCP-10.2.20260422.jar
```

## Project Structure

```
SHAFT_MCP/
├── src/
│   ├── main/
│   │   ├── java/io/github/shafthq/SHAFT_MCP/
│   │   │   ├── ShaftMcpApplication.java    # Main Spring Boot application
│   │   │   ├── EngineService.java          # Driver lifecycle management
│   │   │   ├── BrowserService.java         # Browser control tools
│   │   │   ├── ElementService.java         # Element interaction tools
│   │   │   ├── BrowserType.java            # Browser type enum
│   │   │   └── locatorStrategy.java        # Element locator strategies
│   │   └── resources/
│   │       ├── application.properties      # STDIO transport (Claude Desktop)
│   │       └── application-http.properties # HTTP/SSE transport (Smithery cloud)
│   └── test/
│       └── java/io/github/shafthq/SHAFT_MCP/
│           ├── ShaftMcpApplicationTests.java
│           └── EngineServiceTest.java
├── pom.xml                                 # Maven configuration
├── Dockerfile                              # Docker image (downloads JAR from Maven Central)
├── Dockerfile.smithery                     # Smithery deployment (downloads JAR from Maven Central)
├── Dockerfile.smithery.build               # Smithery deployment (multi-stage build from source)
├── smithery.yaml                           # Smithery.ai configuration
├── server.json                             # MCP Registry manifest (OCI package listing)
├── SMITHERY_DEPLOYMENT.md                  # Smithery deployment guide
└── readme.md                               # User documentation
```

## Key Components

### 1. MCP Tools (Spring AI @Tool annotations)
- All tools are annotated with `@Tool` from `org.springframework.ai.tool.annotation.Tool`
- Tools are organized across three service classes:
  - **EngineService**: Driver initialization and lifecycle (`driver_initialize`, `driver_quit`, `generate_test_report`, `browser_get_page_source`)
  - **BrowserService**: Browser operations (`browser_navigate`, `browser_refresh`, `browser_get_current_url`, etc.)
  - **ElementService**: Element interactions (`element_click`, `element_type`, `element_get_text`, etc.)

### 2. SHAFT Engine Integration
- Uses `SHAFT.GUI.WebDriver` for browser automation
- Supports Chrome, Firefox, Safari, and Edge browsers
- Provides AI-enhanced element detection via SHAFT Engine
- Generates Allure test reports
- Remote Selenium Grid support via `REMOTE_DRIVER_ADDRESS` env variable

### 3. Spring AI MCP Server
- STDIO transport: configured via `application.properties` (for Claude Desktop)
- HTTP/SSE transport: configured via `application-http.properties` (for Smithery and web clients)
- **Critical**: `spring.main.banner-mode=off` and `logging.pattern.console=` must remain empty in `application.properties` for STDIO transport to work
- Server name: `shaft-mcp`

## Coding Standards & Best Practices

### Code Style
- Follow existing Java conventions in the codebase
- Use descriptive variable names and method names
- Include JavaDoc comments for public methods
- Use SLF4J logger for logging, not System.out.println

### Tool Development
When adding new MCP tools:
1. Add methods to the appropriate service class (EngineService, BrowserService, or ElementService)
2. Annotate with `@Tool(name = "tool_name", description = "clear description")`
3. Include comprehensive JavaDoc explaining parameters and behavior
4. Handle exceptions properly with try-catch blocks
5. Log significant actions and errors using the logger
6. Test the tool with unit tests in the corresponding test class

### Error Handling
- Always wrap SHAFT operations in try-catch blocks
- Log errors with context using `logger.error()`
- Re-throw exceptions to allow proper error reporting to MCP clients
- Include meaningful error messages

### Testing
- Write unit tests for new service methods
- Use JUnit Jupiter (JUnit 5) for test cases
- Test classes should mirror the structure: `ServiceName` → `ServiceNameTest`
- **Always run tests before merging**: `mkdir -p src/main/resources/properties && REMOTE_DRIVER_ADDRESS=http://localhost:4444/wd/hub mvn clean test -Pjunit`
- Tests run against a Selenium Grid; `src/main/resources/properties/` must pre-exist

### XPath Best Practices
When writing XPath locators for SHAFT Engine:
- **Use parenthesized form for positional selection**: `(//article[@data-testid='result'])[1]` — this selects the globally-first matching element.
- **Avoid bare positional predicates**: `//article[@data-testid='result'][1]` applies the `[1]` predicate relative to each parent node, which can match **all** matching elements when they have different parents (e.g., each inside its own `<li>`).
- SHAFT throws `MultipleElementsFoundException` if a locator matches more than one element for operations like `getDomAttribute`.

## Important Configuration Files

### pom.xml
- Contains project version (must match in Dockerfile, Dockerfile.smithery, application.properties, application-http.properties, server.json, readme.md)
- Defines build commands in properties:
  - `commandToPackage`: `mvn clean package "-DskipTests" "-Dgpg.skip"`
  - `commandToTest`: `mvn clean test`
- Maven plugins for compilation, testing, and packaging

### application.properties
- STDIO transport (default) — used by Claude Desktop
- **Critical**: `spring.main.banner-mode=off` and `logging.pattern.console=` must remain empty for STDIO transport

### application-http.properties
- HTTP/SSE transport — used by Smithery and web-based MCP clients
- Activated via `SPRING_PROFILES_ACTIVE=http`
- Port controlled by `PORT` env variable (default: 8081)
- SSE endpoint: `/mcp`

### Dockerfile
- Downloads released JAR from Maven Central (STDIO transport)
- Published to GitHub Container Registry: `ghcr.io/shafthq/shaft-mcp`

### Dockerfile.smithery
- Downloads released JAR from Maven Central (HTTP/SSE transport)
- Used for Smithery cloud deployment via pre-built JAR
- Installs Google Chrome for browser automation

### Dockerfile.smithery.build
- Multi-stage build from source code (HTTP/SSE transport)
- Used as the primary build Dockerfile referenced in `smithery.yaml`

### smithery.yaml
- Smithery.ai deployment configuration
- References `Dockerfile.smithery.build` as the build Dockerfile
- HTTP start command type for SSE transport

### server.json
- MCP Registry manifest
- Must have `version` and OCI `identifier` tag matching the current project version

## Version Management

When updating the version:
1. Update `<version>` in `pom.xml`
2. Update `spring.ai.mcp.server.version` in `application.properties`
3. Update `spring.ai.mcp.server.version` in `application-http.properties`
4. Update the JAR download URL in `Dockerfile`
5. Update the JAR download URL in `Dockerfile.smithery`
6. Update version in `readme.md` installation instructions
7. Update `version` and OCI `identifier` in `server.json`

The `sync-versions.yml` CI workflow automates steps 2–7 when `pom.xml` changes.

Current version: `10.2.20260422`

## Dependencies

Key dependencies managed in pom.xml:
- `spring-boot-starter-parent` (4.0.5)
- `spring-ai-starter-mcp-server` (1.1.4 via BOM)
- `spring-ai-starter-mcp-server-webmvc` (1.1.4 via BOM) — HTTP/SSE transport
- `SHAFT_ENGINE` (10.2.20260422)
- `aspectjweaver` (1.9.25.1)
- `junit-jupiter-engine` (6.0.3)

## Files to Ignore

The following are auto-generated and must never be committed (already in `.gitignore`):
- `target/` - Maven build artifacts
- `allure-results/`, `allure-report/` - Test reports
- `allurerc.yaml` - Allure configuration auto-generated by SHAFT Engine during tests
- `src/main/resources/properties/` - SHAFT Engine extracted default property files
- IDE-specific files (`.idea/`, `.vscode/`, `.settings/`, etc.)
- `HELP.md` - Auto-generated Spring Boot help

## Cloud Deployment (Smithery)

SHAFT MCP is listed on [Smithery.ai](https://smithery.ai/server/@ShaftHQ/shaft-mcp) and supports zero-install cloud deployment via HTTP/SSE transport.

### Deployment Flow
1. Smithery uses `smithery.yaml` → builds via `Dockerfile.smithery.build`
2. Container starts with `SPRING_PROFILES_ACTIVE=http` → activates `application-http.properties`
3. Server listens on `PORT` env variable (default 8081) with SSE endpoint at `/mcp`
4. Chrome is installed inside the container for browser automation

### Local Smithery Testing
```bash
# Build the Docker image
docker build -f Dockerfile.smithery.build -t shaft-mcp-http .

# Run the container
docker run -p 8081:8081 -e PORT=8081 -e SPRING_PROFILES_ACTIVE=http shaft-mcp-http

# Test the SSE endpoint
curl -N -H "Accept: text/event-stream" http://localhost:8081/mcp
```

## Documentation

### User-Facing Documentation
- `readme.md` - Complete setup and usage guide for end users
- `SMITHERY_DEPLOYMENT.md` - Smithery and cloud hosting deployment guide
- Includes installation instructions for both JAR and Docker
- Documents all 25+ available MCP tools
- Provides usage examples and troubleshooting

### Code Documentation
- Use JavaDoc for all public methods
- Include parameter descriptions and return value documentation
- Document exceptions that may be thrown

## Testing the MCP Server

To test the MCP server locally:

1. Build the project: `mvn clean package -DskipTests -Dgpg.skip`
2. Configure Claude Desktop to use the JAR file
3. Test with prompts like:
   ```
   Use shaft-mcp to launch Chrome, navigate to google.com, and get the page title.
   ```

## Common Tasks

### Adding a New Browser Tool
1. Add method to `BrowserService.java`
2. Annotate with `@Tool`
3. Use `getDriver()` to access the SHAFT WebDriver
4. Call appropriate `driver.browser()` method
5. Add error handling and logging
6. Write unit tests in `EngineServiceTest.java`

### Adding a New Element Tool
1. Add method to `ElementService.java`
2. Annotate with `@Tool`
3. Accept locator parameters (strategy, value)
4. Use `getDriver().element()` for operations
5. Add error handling and logging
6. Write unit tests

### Updating Dependencies
1. Check for updates to Spring Boot, Spring AI, or SHAFT Engine
2. Update versions in `pom.xml`
3. Run `mvn clean test -Pjunit` to verify compatibility
4. Update readme.md if needed

## Review Criteria

Pull requests should:
- [ ] Include clear, focused changes
- [ ] Have **all tests passing** locally (`mkdir -p src/main/resources/properties && REMOTE_DRIVER_ADDRESS=http://localhost:4444/wd/hub mvn clean test -Pjunit`)
- [ ] Build successfully (`mvn clean package -DskipTests -Dgpg.skip`)
- [ ] Include JavaDoc for new public methods
- [ ] Follow existing code style and patterns
- [ ] Update documentation if user-facing features changed
- [ ] Not break existing MCP tools
- [ ] Handle errors gracefully with proper logging

## Known Constraints

- This is an MCP server using STDIO transport — console output must be carefully managed in `application.properties`
- Browser automation requires the target browser to be installed on the host system (or reachable via Selenium Grid)
- SHAFT Engine generates Allure reports and `allurerc.yaml` which should be gitignored
- Docker images must be built with headless/virtual-display browser support
- DuckDuckGo (and similar sites) detect headless Chrome via the `--headless` flag; use the Selenium Grid (which uses Xvfb for non-headless rendering) in tests instead

## Additional Resources

- SHAFT Engine Documentation: https://shafthq.github.io/
- Model Context Protocol Spec: https://modelcontextprotocol.io/
- Spring AI MCP: https://docs.spring.io/spring-ai/reference/api/mcp/index.html
- Maven Central: https://central.sonatype.com/artifact/io.github.shafthq/SHAFT_MCP
- Smithery listing: https://smithery.ai/server/@ShaftHQ/shaft-mcp

