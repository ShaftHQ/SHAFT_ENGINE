<h1>
<picture>
  <!-- User prefers light mode: -->
  <source srcset="https://github.com/user-attachments/assets/b2e8454d-97ed-4dd8-91f2-1c09c53ba94e" media="(prefers-color-scheme: light)" width="40"/>

  <!-- User prefers dark mode: -->
  <source srcset="https://github.com/user-attachments/assets/9cb4a7a8-2de7-486c-adb1-ad254af8c58b"  media="(prefers-color-scheme: dark)" width="40"/>

  <!-- User has no color preference: -->
  <img src="https://github.com/user-attachments/assets/016ebb3c-4090-4f07-a9b3-830fdf4cb696"/>
</picture> SHAFT MCP Server Setup Guide
</h1>

<img width="1280" height="720" alt="New Project" src="https://github.com/user-attachments/assets/cda129a7-592b-4e82-8294-dbed364d0e2d" />


## What is SHAFT MCP?

SHAFT MCP is a **Model Context Protocol (MCP) server** that enables Claude Desktop to perform **web automation tasks** using the SHAFT Engine (a Selenium-based test automation framework). This gives Claude powerful browser automation capabilities including:

### Core Features
- **Browser Control**: Launch Chrome, Firefox, Safari, or Edge browsers
- **Element Interaction**: Click, hover, type, clear, drag-and-drop elements
- **AI-Enhanced Element Detection**: Find elements by natural language descriptions
- **Advanced Actions**: JavaScript-based clicks, file uploads, cookie management
- **Browser Navigation**: URL navigation, back/forward, refresh
- **Data Extraction**: Get page source, element text, attributes, CSS values
- **Test Reporting**: Generate detailed Allure test reports

### Available MCP Tools
The server provides 25+ tools for web automation:

**Browser Management:**
- `driver_initialize` - Launch browser (Chrome/Firefox/Safari/Edge)
- `driver_quit` - Close browser
- `browser_navigate` - Navigate to URL
- `browser_refresh` - Refresh page
- `browser_navigate_back/forward` - Browser history navigation
- `browser_maximize_window` - Maximize browser window
- `browser_set_window_size` - Set custom window size
- `browser_fullscreen_window` - Set fullscreen mode

**Element Interaction:**
- `element_click` - Click elements using locators (ID, CSS, XPath, etc.)
- `element_click_ai` - Click elements using AI (natural language)
- `element_click_js` - JavaScript-based clicking
- `element_double_click` - Double-click elements
- `element_hover` - Hover over elements
- `element_type` - Type text into elements
- `element_type_ai` - Type using AI element detection
- `element_append_text` - Append text to existing content
- `element_clear` - Clear text from input fields
- `element_drag_and_drop` - Drag and drop between elements
- `element_drop_file_to_upload` - File upload functionality

**Data Extraction:**
- `element_get_text` - Get element text content
- `element_get_dom_attribute` - Get DOM attributes
- `element_get_dom_property` - Get DOM properties
- `element_get_css_value` - Get CSS property values
- `element_is_displayed/enabled/selected` - Check element states
- `browser_get_page_source` - Get page HTML source
- `browser_get_current_url` - Get current URL
- `browser_get_title` - Get page title

**Session Management:**
- `browser_add_cookie` - Add cookies
- `browser_get_cookie` - Get specific cookie
- `browser_get_all_cookies` - Get all cookies
- `browser_delete_cookie` - Delete specific cookie
- `browser_delete_all_cookies` - Clear all cookies
- `generate_test_report` - Generate Allure test reports

## Distribution

SHAFT MCP is available through multiple channels:

- **GitHub MCP Registry**: Officially listed in the [Model Context Protocol Registry](https://registry.modelcontextprotocol.io/) as `io.github.ShaftHQ/shaft-mcp`
- **Maven Central**: Available as a Java JAR at `io.github.shafthq:SHAFT_MCP`
- **GitHub Container Registry**: Docker images available at `ghcr.io/shafthq/shaft-mcp`
- **Smithery.ai**: Deploy via Smithery for hosted MCP server (see [Deployment Guide](SMITHERY_DEPLOYMENT.md#smithery-deployment))
- **Render.com**: One-click free cloud deployment — no credit card required (see [Deployment Guide](SMITHERY_DEPLOYMENT.md#rendercom-deployment))
- **Fly.io**: Global edge deployment that suspends when idle by default, with $5/month free credit (see [Deployment Guide](SMITHERY_DEPLOYMENT.md#flyio-deployment))

## Deployment Options

SHAFT MCP supports two deployment modes:

### 1. Local Deployment (Claude Desktop)
Uses **STDIO transport** for direct integration with Claude Desktop application. See setup instructions below.

### 2. Remote Deployment (Smithery, Render, Fly.io & Web)
Uses **HTTP/SSE transport** for web-based access and remote hosting platforms like Smithery.ai, Render.com, Fly.io, and more.

📖 **For all remote deployment instructions**, see the comprehensive [Deployment Guide](SMITHERY_DEPLOYMENT.md) which includes:
- Step-by-step Smithery deployment
- Render.com free-tier deployment (no credit card required)
- Fly.io always-on global edge deployment
- Local testing with Docker
- Configuration options and best practices

## Prerequisites

- **Claude Desktop App** (latest version)
- **Java 25** (OpenJDK 25 or newer) - for JAR-based installation
- **OR Docker** - for Docker-based installation
- **Maven** (only if building from source)

## Setup Instructions

You can install SHAFT MCP using either the JAR file or Docker container:

### Option A: Using Docker (Recommended for Remote Browser Control)

When using Docker, SHAFT MCP can connect to a Selenium Server running on your host machine, allowing it to control browsers on your computer rather than inside the container.

#### Step 1: Set up Selenium Server on Host Machine

First, you need to run Selenium Server (Selenium Grid) on your host machine:

```bash
# Pull and run Selenium Grid Standalone with Chrome
docker run -d -p 4444:4444 -p 7900:7900 --shm-size="2g" \
  selenium/standalone-chrome:latest

# Or for Firefox
docker run -d -p 4444:4444 -p 7900:7900 --shm-size="2g" \
  selenium/standalone-firefox:latest
```

You can verify Selenium is running by visiting: `http://localhost:4444`

**Optional**: View the browser session in real-time at `http://localhost:7900` (password: `secret`)

#### Step 2: Pull the SHAFT MCP Docker Image

```bash
docker pull ghcr.io/shafthq/shaft-mcp:latest
```

#### Step 3: Configure Claude Desktop

1. **Close Claude Desktop** completely if it's running

2. **Locate your Claude config file:**
   - **Windows**: `C:\Users\{USERNAME}\AppData\Roaming\Claude\claude_desktop_config.json`
   - **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
   - **Linux**: `~/.config/Claude/claude_desktop_config.json`

3. **Edit the config file** and add the SHAFT MCP server configuration with remote WebDriver settings:

**For Linux:**
```json
{
  "mcpServers": {
    "shaft-mcp": {
      "command": "docker",
      "args": [
        "run",
        "--rm",
        "-i",
        "--network=host",
        "-e", "REMOTE_DRIVER_ADDRESS=http://localhost:4444/wd/hub",
        "ghcr.io/shafthq/shaft-mcp:latest"
      ]
    }
  }
}
```

**For macOS and Windows:**
```json
{
  "mcpServers": {
    "shaft-mcp": {
      "command": "docker",
      "args": [
        "run",
        "--rm",
        "-i",
        "-e", "REMOTE_DRIVER_ADDRESS=http://host.docker.internal:4444/wd/hub",
        "ghcr.io/shafthq/shaft-mcp:latest"
      ]
    }
  }
}
```

**Note**: 
- On **Linux**, `--network=host` lets the container use the host's network stack, so `localhost` resolves to the host machine
- On **macOS and Windows**, Docker runs inside a VM, so `--network=host` does **not** expose the host's network. Use `host.docker.internal` instead, which Docker automatically resolves to the host machine's IP
- If you're using Selenium Grid on a different port, update the `REMOTE_DRIVER_ADDRESS` accordingly

### Option A (Alternative): Using Docker for Local Browser (Not Recommended)

If you want browsers to run inside the Docker container (not recommended due to limitations), you can use the basic configuration:

```json
{
  "mcpServers": {
    "shaft-mcp": {
      "command": "docker",
      "args": [
        "run",
        "--rm",
        "-i",
        "ghcr.io/shafthq/shaft-mcp:latest"
      ]
    }
  }
}
```

However, this requires the Docker image to have browsers and display drivers installed, which is complex and not currently supported.

### Option B: Using JAR File

#### Step 1: Build the MCP Server

1. Navigate to your SHAFT_MCP repository directory:
```bash
cd /path/to/ShaftHQ/SHAFT_MCP
```

2. Build the project (this creates the JAR file):
```bash
mvn clean package -DskipTests -Dgpg.skip
```

This will create: `target/SHAFT_MCP-10.2.20260501.jar`

#### Step 2: Configure Claude Desktop

1. **Close Claude Desktop** completely if it's running

2. **Locate your Claude config file:**
   - **Windows**: `C:\Users\{USERNAME}\AppData\Roaming\Claude\claude_desktop_config.json`
   - **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
   - **Linux**: `~/.config/Claude/claude_desktop_config.json`

3. **Edit the config file** and add the SHAFT MCP server configuration:

```json
{
  "mcpServers": {
    "shaft-mcp": {
      "command": "java",
      "args": [
        "-jar",
        "/FULL/PATH/TO/SHAFT_MCP/target/SHAFT_MCP-10.2.20260501.jar"
      ]
    }
  }
}
```

**Important Notes:**
- Replace `/FULL/PATH/TO/SHAFT_MCP/` with the actual absolute path to your repository
- Ensure the JAR file version matches what was built (check the `target/` directory)
- Use forward slashes `/` even on Windows, or double backslashes `\\\\` for Windows paths

## Verify Installation

1. **Start Claude Desktop**

2. **Check MCP Server Status:**
   - Go to `File > Settings > Developer`
   - Look for `shaft-mcp` in the MCP servers list
   - Confirm the status shows as `running`

3. **Test the server** with this prompt in Claude:
```
Use shaft-mcp to launch Chrome, navigate to google.com, search for "selenium webdriver", and get the page title.
```

## Usage Examples

Once configured, you can use Claude to perform complex web automation tasks:

### Basic Browser Control
```
Use shaft-mcp to:
1. Launch Firefox browser
2. Navigate to https://example.com
3. Get the page title and current URL
```

### Element Interaction
```
Use shaft-mcp to:
1. Launch Chrome
2. Go to a login page
3. Find the username field and type "testuser"
4. Find the password field and type "testpass"
5. Click the login button
```

### AI-Enhanced Element Detection
```
Use shaft-mcp to:
1. Open a browser and go to amazon.com
2. Click on the search box using AI
3. Type "laptop" in the search field
4. Click the search button using AI
```

### Data Extraction
```
Use shaft-mcp to:
1. Navigate to a news website
2. Get all the article headlines
3. Extract the main content text
4. Generate a test report
```

## Troubleshooting

### Common Issues:

1. **"No active browser session found"**
   - Always initialize a browser first using `driver_initialize`

2. **MCP Server not running**
   - Verify the JAR file path is correct and absolute
   - Check that Java 25 is installed and accessible
   - Ensure Claude Desktop was restarted after config changes

3. **Build failures**
   - Verify Java 25 is installed: `java -version`
   - Ensure Maven is available: `mvn -version`
   - Try cleaning first: `mvn clean`

4. **Element not found errors**
   - Use AI-based element detection (`element_click_ai`, `element_type_ai`)
   - Try different locator strategies (CSS, XPath, ID, etc.)
   - Check if the element is visible with `element_is_displayed`

5. **Docker container cannot connect to host Selenium Server**
   - **On Linux**: Make sure you're using `--network=host` in the docker run arguments
   - **On macOS and Windows**: Ensure you're using `host.docker.internal` instead of `localhost` in `REMOTE_DRIVER_ADDRESS` (Docker Desktop on macOS/Windows runs in a VM so `--network=host` does not expose the host network)
   - Verify Selenium Server is running: `curl http://localhost:4444/status`
   - Check firewall settings aren't blocking port 4444
   - Make sure `REMOTE_DRIVER_ADDRESS` is set to the correct Selenium Server URL in the Docker configuration

6. **Browser opens inside Docker container instead of on host**
   - This happens when `REMOTE_DRIVER_ADDRESS` is not properly configured
   - Verify the environment variable is set in the Claude Desktop config
   - Check the SHAFT MCP logs to confirm remote configuration was detected

### Advanced Configuration

You can add additional MCP servers alongside SHAFT MCP:

```json
{
  "mcpServers": {
    "shaft-mcp": {
      "command": "java",
      "args": ["-jar", "/path/to/SHAFT_MCP-10.2.20260501.jar"]
    },
    "github": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"],
      "env": {
        "GITHUB_PERSONAL_ACCESS_TOKEN": "your_token_here"
      }
    }
  }
}
```

### Remote WebDriver with JAR Installation

You can also use remote WebDriver with the JAR installation by setting environment variables before starting Claude Desktop:

**On macOS/Linux:**
```bash
export REMOTE_DRIVER_ADDRESS=http://localhost:4444/wd/hub
# Then start Claude Desktop
```

**On Windows (PowerShell):**
```powershell
$env:REMOTE_DRIVER_ADDRESS="http://localhost:4444/wd/hub"
# Then start Claude Desktop
```

## What This Enables

With SHAFT MCP configured, Claude can:
- **Automate web testing** - Run comprehensive test suites on web applications
- **Perform web scraping** - Extract data from websites with complex interactions
- **Automate repetitive tasks** - Fill forms, upload files, navigate workflows
- **Generate test reports** - Create detailed Allure reports with screenshots and logs
- **Cross-browser testing** - Test across Chrome, Firefox, Safari, and Edge
- **AI-powered element detection** - Find elements by description instead of technical locators

This essentially turns Claude into a powerful web automation assistant that can handle complex browser-based tasks that would normally require manual effort or custom scripting.
