# SHAFT MCP - Cloud Deployment Guide

## Overview

This guide explains how to deploy SHAFT MCP to free cloud hosting platforms. SHAFT MCP supports both STDIO transport (for Claude Desktop) and HTTP/SSE transport (for Smithery, Render.com, Fly.io, and other web-based deployments).

The top 3 **free** cloud platforms for hosting MCP servers are:

| Platform | Free Tier | Always-On | Credit Card | MCP-Specific | Best For |
|----------|-----------|-----------|-------------|--------------|----------|
| [**Smithery.ai**](#smithery-deployment) | Yes | Yes | No | ✅ Yes | MCP-native hosting |
| [**Render.com**](#rendercom-deployment) | Yes (512 MB) | No (sleeps) | No | No | Simple Docker deploys |
| [**Fly.io**](#flyio-deployment) | $5/mo credit | Yes | Yes | No | Always-on global edge |

All three platforms use the **HTTP/SSE transport** mode (`SPRING_PROFILES_ACTIVE=http`) with the MCP endpoint at `/mcp`.

Deployments to all three platforms are automated via the **`deploy-cloud-hosting.yml`** GitHub Actions workflow, which triggers automatically after every successful Maven CD + Docker publish pipeline run.

### Required GitHub Secrets for Automated Deployment

Before automated deployments can run, add these secrets in your repository's **Settings → Secrets → Actions**:

| Secret | Platform | How to Obtain |
|--------|----------|---------------|
| `RENDER_DEPLOY_HOOK_URL` | Render.com | Dashboard → Service → Settings → **Deploy Hook** |
| `FLY_API_TOKEN` | Fly.io | Run `fly tokens create deploy -a shaft-mcp` |

Smithery.ai requires no secret — it auto-deploys from the connected GitHub repository.

---

## Smithery Deployment

### Prerequisites

- GitHub account
- Smithery.ai account
- Docker installed (for local testing)

### Deployment Steps

1. **Push Your Code to GitHub**
   
   Ensure your repository contains:
   - `smithery.yaml` - Smithery configuration file
   - `Dockerfile.smithery` - Optimized Dockerfile for Smithery
   - `src/main/resources/application-http.properties` - HTTP transport configuration

2. **Connect Repository to Smithery**
   
   - Visit [Smithery.ai](https://smithery.ai)
   - Click "Deploy New Server"
   - Connect your GitHub repository: `ShaftHQ/SHAFT_MCP`
   - Smithery will automatically detect the `smithery.yaml` file

3. **Configure Deployment**
   
   Smithery will use the configuration in `smithery.yaml`:
   - Runtime: Container
   - Build: `Dockerfile.smithery.build`
   - Start command: HTTP (port 8081)
   - MCP SSE endpoint: `/mcp`
   
4. **Deploy**
   
   - Smithery will build the Docker image from `Dockerfile.smithery.build`
   - The server will start with HTTP/SSE transport enabled
   - The MCP endpoint will be available at `/mcp`

### Local Testing Before Deployment

Test the HTTP transport locally:

```bash
# Build the Docker image
docker build -f Dockerfile.smithery.build -t shaft-mcp-http .

# Run the container
docker run -p 8081:8081 -e PORT=8081 -e SPRING_PROFILES_ACTIVE=http shaft-mcp-http

# Test the endpoint (in another terminal)
curl -N -H "Accept: text/event-stream" http://localhost:8081/mcp
```

### Configuration Options

The `smithery.yaml` supports the following configuration:

```yaml
browserType: "CHROME"  # Options: CHROME, FIREFOX, SAFARI, EDGE
```

---

## Render.com Deployment

[Render.com](https://render.com) offers a **completely free** Docker web service tier — no credit card required. It's the easiest way to get SHAFT MCP accessible at a public URL.

### Free Tier Specs

| Resource | Limit | Impact |
|----------|-------|--------|
| RAM | 512 MB | Tight for Java + Chrome; JVM tuned |
| CPU | 0.1 shared | Slower but functional |
| Sleep | After 15 min idle | ~30 s cold start on first request |
| Deploy URL | `https://your-app.onrender.com` | Auto-provisioned |

### Quick Deploy (via render.yaml)

This repository includes a `render.yaml` that Render auto-detects:

1. **Fork or connect** your copy of `ShaftHQ/SHAFT_MCP` to your GitHub account
2. Go to [render.com/dashboard](https://render.com/dashboard) → **New +** → **Web Service**
3. Connect your GitHub repository
4. Render will detect `render.yaml` and pre-fill all settings — just click **Create Web Service**
5. Wait 3–5 minutes for the build and first deploy

### Manual Configuration

If you prefer to configure manually instead of using `render.yaml`:

| Setting | Value |
|---------|-------|
| **Environment** | Docker |
| **Dockerfile Path** | `Dockerfile.render` |
| **Plan** | Free |
| **SPRING_PROFILES_ACTIVE** | `http` |

### Testing Your Render Deployment

```bash
# Replace with your actual Render URL
RENDER_URL=https://shaft-mcp.onrender.com

# Test the MCP SSE endpoint
curl -N -H "Accept: text/event-stream" $RENDER_URL/mcp
```

### Preventing Cold Starts

The free tier sleeps after 15 minutes of inactivity. To keep the server warm, ping it periodically:

```bash
# Bounded keep-alive for the SSE endpoint using cron or an external uptime service
curl -s --max-time 5 -H "Accept: text/event-stream" "$RENDER_URL/mcp" -o /dev/null
```

### Local Testing (Render Docker)

```bash
# Build and run locally simulating Render's 512 MB limit
docker build -f Dockerfile.render -t shaft-mcp-render .
docker run --memory=512m -p 8081:8081 -e PORT=8081 shaft-mcp-render

# Test the endpoint
curl -N -H "Accept: text/event-stream" http://localhost:8081/mcp
```

### Troubleshooting Render

- **Out-of-memory crash**: Chrome exhausted the 512 MB budget. Set `REMOTE_DRIVER_ADDRESS` to an external Selenium Grid and avoid launching Chrome inside the container.
- **Slow first response**: Normal cold start — Render spins down free services after 15 minutes of inactivity.

---

## Fly.io Deployment

[Fly.io](https://fly.io) offers a **global edge** platform. A $5/month free credit is included per account (credit card required). This covers a 512 MB shared VM for light usage. By default, machines suspend after idle — the first request after a long idle period may take 2–5 s.

### Free Tier Specs

| Resource | Limit | Impact |
|----------|-------|--------|
| RAM | 512 MB (single VM) | Tuned JVM + Chrome fits |
| CPU | 1 shared vCPU | Adequate for MCP tasks |
| Sleep | Suspends when idle (`min_machines_running = 0`) | Cold start on first request after idle |
| Credit | $5/month included | Covers small VM |
| Deploy URL | `https://shaft-mcp.fly.dev` | Auto-provisioned |

### Prerequisites

```bash
# Install Fly CLI
curl -L https://fly.io/install.sh | sh

# Authenticate
fly auth login
```

### First Deployment

```bash
git clone https://github.com/ShaftHQ/SHAFT_MCP.git
cd SHAFT_MCP

# Creates the Fly app using fly.toml (already in this repo)
fly launch --copy-config --yes

# Fly builds the image, deploys, and gives you a URL
```

### Subsequent Deployments

```bash
fly deploy
```

### Verifying the Deployment

```bash
# Check machine status
fly status

# Stream logs
fly logs

# Test the MCP SSE endpoint
FLY_URL=https://shaft-mcp.fly.dev
curl -N -H "Accept: text/event-stream" $FLY_URL/mcp
```

### Scaling and Regions

```bash
# Add a second region for lower latency
fly regions add lhr   # London

# Scale to two machines
fly scale count 2
```

### Local Testing (Fly Docker)

```bash
# Build and run locally simulating Fly.io's 512 MB limit
docker build -f Dockerfile.fly -t shaft-mcp-fly .
docker run --memory=512m -p 8080:8080 -e PORT=8080 shaft-mcp-fly

# Test the endpoint
curl -N -H "Accept: text/event-stream" http://localhost:8080/mcp
```

### Troubleshooting Fly.io

- **Machine won't start**: Run `fly logs` and look for OOM errors. The JVM tuning in `fly.toml` should keep heap under 384 MB.
- **Slow wake after suspend**: `auto_stop_machines = "suspend"` is set in `fly.toml`. The first request after a long idle period may take 2–5 s. To keep the machine always running, change `min_machines_running` from `0` to `1` in `fly.toml` (note: this will consume more of your $5/month credit).
- **Credit card declined**: Fly.io requires a valid card even for free-tier usage. Use Render.com if you need a no-card option.

---

## Alternative MCP Server Hosting Platforms

While Smithery, Render, and Fly.io are recommended, you have several other hosting options:

### 1. **Cloudflare Workers**
- **Best For**: Edge-hosted, globally distributed MCP servers
- **Pros**: 
  - Free tier (100k requests/day)
  - Minimal cold starts
  - Global edge network
  - One-click deployment
- **Cons**: 
  - Limited to HTTP/SSE transport
  - May not support heavy browser automation
- **Deployment**: Use Cloudflare Workers with custom routing
- **Website**: [https://workers.cloudflare.com/](https://workers.cloudflare.com/)

### 2. **Railway**
- **Best For**: Custom deployments with CI/CD
- **Pros**:
  - Flexible deployment options
  - Good developer experience
  - Support for Docker containers
  - Built-in CI/CD
- **Cons**: 
  - Usage-based pricing can add up
  - Network costs for high traffic
- **Deployment**: Connect GitHub repo and deploy with Docker
- **Website**: [https://railway.app/](https://railway.app/)

### 3. **Glama**
- **Best For**: Beginners and quick prototypes
- **Pros**:
  - Simple UI for deployment
  - Fully hosted solution
  - No infrastructure management
- **Cons**: 
  - Limited free tier
  - Fewer customization options
- **Deployment**: Use their web interface
- **Website**: [https://glama.ai/](https://glama.ai/)

### 4. **Composio**
- **Best For**: Enterprise-grade production deployments
- **Pros**:
  - Managed MCP servers
  - OAuth and token management
  - Production-grade SLAs
  - Built-in integrations
- **Cons**: 
  - Paid plans for managed services
  - More complex setup
- **Deployment**: Use their managed platform
- **Website**: [https://composio.dev/](https://composio.dev/)

### 5. **Pipedream**
- **Best For**: API-heavy MCP servers with workflows
- **Pros**:
  - 2,500+ built-in integrations
  - Workflow automation
  - Strong authentication handling
  - API-first approach
- **Cons**: 
  - Higher costs at scale
  - Learning curve for workflows
- **Deployment**: Use their platform with webhooks
- **Website**: [https://pipedream.com/](https://pipedream.com/)

### 6. **Render**
- **Best For**: Simple container deployments
- **Pros**:
  - Free tier available
  - Easy Docker deployment
  - Auto-scaling
  - Good for small to medium projects
- **Cons**: 
  - Limited free tier resources
  - May have cold starts on free tier
- **Deployment**: Deploy from GitHub with Docker
- **Website**: [https://render.com/](https://render.com/)

### 7. **Fly.io**
- **Best For**: Global distribution and edge deployment
- **Pros**:
  - Global edge network
  - Docker support
  - Good free tier
  - Low latency worldwide
- **Cons**: 
  - Requires Fly CLI for deployment
  - Learning curve for Fly-specific features
- **Deployment**: Use Fly CLI with Docker
- **Website**: [https://fly.io/](https://fly.io/)

### 8. **Google Cloud Run**
- **Best For**: Scalable, serverless container deployments
- **Pros**:
  - Serverless container platform
  - Auto-scaling
  - Pay-per-use pricing
  - Integration with GCP services
- **Cons**: 
  - Requires GCP account and setup
  - Learning curve for GCP
- **Deployment**: Deploy container to Cloud Run
- **Website**: [https://cloud.google.com/run](https://cloud.google.com/run)

### 9. **Azure Container Instances**
- **Best For**: Enterprise Azure deployments
- **Pros**:
  - Fast container startup
  - Azure ecosystem integration
  - Enterprise security features
  - Good for hybrid cloud
- **Cons**: 
  - Higher cost than some alternatives
  - Azure-specific knowledge needed
- **Deployment**: Deploy via Azure Portal or CLI
- **Website**: [https://azure.microsoft.com/en-us/services/container-instances/](https://azure.microsoft.com/en-us/services/container-instances/)

### 10. **AWS App Runner**
- **Best For**: AWS-native deployments
- **Pros**:
  - Simple container deployment on AWS
  - Auto-scaling
  - AWS ecosystem integration
  - Good for existing AWS users
- **Cons**: 
  - AWS account required
  - Can be more expensive
- **Deployment**: Deploy from container registry
- **Website**: [https://aws.amazon.com/apprunner/](https://aws.amazon.com/apprunner/)

## Choosing the Right Platform

| Platform | Best For | Free Tier | Browser Support | Difficulty |
|----------|----------|-----------|-----------------|------------|
| **Smithery** | MCP-specific deployments | Yes | Limited | Easy |
| **Cloudflare Workers** | Edge computing | Yes (100k req) | No | Easy |
| **Railway** | Custom apps with CI/CD | Trial | Yes | Easy |
| **Glama** | Beginners | Limited | No | Very Easy |
| **Composio** | Enterprise | OSS core | Limited | Medium |
| **Pipedream** | API workflows | Yes | Limited | Medium |
| **Render** | Simple deployments | Yes | Yes | Easy |
| **Fly.io** | Global edge | Yes | Yes | Medium |
| **Google Cloud Run** | Scalable serverless | Yes | Yes | Medium |
| **Azure Container Instances** | Enterprise Azure | Trial | Yes | Medium |
| **AWS App Runner** | AWS ecosystem | Limited | Yes | Medium |

### Recommendations

- **For MCP-specific features**: Start with **Smithery**
- **For global edge deployment**: Use **Cloudflare Workers** or **Fly.io**
- **For enterprise production**: Choose **Composio**, **Azure**, or **AWS**
- **For beginners**: Start with **Glama** or **Render**
- **For API-heavy workflows**: Use **Pipedream**
- **For cost-conscious deployments**: Try **Railway** or **Render** free tiers
- **For full browser automation support**: Use **Railway**, **Render**, **Fly.io**, or cloud providers (GCP, Azure, AWS)

## Architecture Differences

### STDIO Transport (Claude Desktop)
- **Use Case**: Local Claude Desktop integration
- **Configuration**: `application.properties` (default)
- **Deployment**: JAR file or Docker with STDIO
- **Profile**: Default (no profile)

### HTTP/SSE Transport (Smithery & Web)
- **Use Case**: Remote web-based access
- **Configuration**: `application-http.properties`
- **Deployment**: Docker container with HTTP endpoint
- **Profile**: `http` (activated via `SPRING_PROFILES_ACTIVE=http`)
- **Endpoint**: `/mcp` (configured via `spring.ai.mcp.server.sse-endpoint=/mcp`)

## Troubleshooting

### Docker Build Fails
- Ensure Java 25 is installed in the base image
- Check that all dependencies are correctly specified in `pom.xml`
- Verify network connectivity for Maven downloads

### HTTP Endpoint Not Responding
- Check that the correct profile is activated: `SPRING_PROFILES_ACTIVE=http`
- Verify the PORT environment variable is set (default: 8081)
- Ensure firewall/security groups allow traffic on the port

### Browser Automation Issues on Smithery
- Some platforms may have limitations on headless browser automation
- Chrome/Chromium must be installed in the Docker image
- Display drivers (Xvfb) may be required for some platforms

## Additional Resources

- [Smithery Documentation](https://smithery.ai/docs)
- [Spring AI MCP Documentation](https://docs.spring.io/spring-ai/reference/api/mcp/)
- [SHAFT Engine Documentation](https://shafthq.github.io/)
- [Model Context Protocol Specification](https://modelcontextprotocol.io/)

## Support

For issues or questions:
- GitHub Issues: [https://github.com/ShaftHQ/SHAFT_MCP/issues](https://github.com/ShaftHQ/SHAFT_MCP/issues)
- SHAFT Documentation: [https://shafthq.github.io/](https://shafthq.github.io/)
