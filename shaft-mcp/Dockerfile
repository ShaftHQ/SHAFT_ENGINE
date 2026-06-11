FROM eclipse-temurin:25-jre

# MCP Registry validation label
LABEL io.modelcontextprotocol.server.name="io.github.ShaftHQ/shaft-mcp"

# Download the released JAR from Maven Central
ADD https://repo1.maven.org/maven2/io/github/shafthq/SHAFT_MCP/10.2.20260506/SHAFT_MCP-10.2.20260506.jar /app/SHAFT_MCP.jar

WORKDIR /app

# Set REMOTE_DRIVER_ADDRESS to a Selenium Grid URL (e.g. http://host.docker.internal:4444/wd/hub)
# to make SHAFT MCP connect to browsers running on the host machine instead of launching them
# inside this container.  Leave empty (the default) to launch a local browser.
ENV REMOTE_DRIVER_ADDRESS=""

# Run the main class from the JAR
CMD ["java", "-jar", "SHAFT_MCP.jar"]