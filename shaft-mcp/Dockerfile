FROM maven:3.9.11-eclipse-temurin-25 AS builder

WORKDIR /build
COPY . .
RUN mvn --batch-mode --no-transfer-progress -pl shaft-mcp -am package -DskipTests -Dgpg.skip \
    && find shaft-mcp/target -maxdepth 1 -name 'shaft-mcp-*.jar' \
        ! -name '*-sources.jar' ! -name '*-javadoc.jar' \
        -exec cp {} /build/shaft-mcp.jar \; -quit

FROM eclipse-temurin:25-jre

LABEL io.modelcontextprotocol.server.name="io.github.ShaftHQ/shaft-mcp" \
      org.opencontainers.image.source="https://github.com/ShaftHQ/SHAFT_ENGINE" \
      org.opencontainers.image.description="shaft-mcp server for browser and test automation" \
      org.opencontainers.image.licenses="MIT"

WORKDIR /app
COPY --from=builder /build/shaft-mcp.jar /app/shaft-mcp.jar

ENV REMOTE_DRIVER_ADDRESS=""

CMD ["java", "-jar", "shaft-mcp.jar"]
