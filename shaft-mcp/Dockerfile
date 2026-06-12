FROM maven:3.9.11-eclipse-temurin-25 AS builder

WORKDIR /build
COPY . .
RUN mvn --batch-mode --no-transfer-progress -pl shaft-mcp -am package -DskipTests -Dgpg.skip \
    && find shaft-mcp/target -maxdepth 1 -name 'SHAFT_MCP-*.jar' \
        ! -name '*-sources.jar' ! -name '*-javadoc.jar' \
        -exec cp {} /build/SHAFT_MCP.jar \; -quit

FROM eclipse-temurin:25-jre

LABEL io.modelcontextprotocol.server.name="io.github.ShaftHQ/shaft-mcp" \
      org.opencontainers.image.source="https://github.com/ShaftHQ/SHAFT_ENGINE" \
      org.opencontainers.image.description="SHAFT MCP server for browser and test automation" \
      org.opencontainers.image.licenses="MIT"

WORKDIR /app
COPY --from=builder /build/SHAFT_MCP.jar /app/SHAFT_MCP.jar

ENV REMOTE_DRIVER_ADDRESS=""

CMD ["java", "-jar", "SHAFT_MCP.jar"]
