import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const packageRoot = path.resolve(__dirname, "..");
const repoRoot = path.resolve(packageRoot, "..", "..");
const serverPath = path.join(packageRoot, "src", "index.js");

const transport = new StdioClientTransport({
  command: process.execPath,
  args: [serverPath],
  env: {
    ...process.env,
    SHAFT_WORKSPACE: repoRoot,
  },
});

const client = new Client({
  name: "shaft-mcp-smoke-test",
  version: "0.1.0",
});

try {
  await client.connect(transport);
  const tools = await client.listTools();
  const toolNames = tools.tools.map((tool) => tool.name);
  const requiredTools = [
    "shaft_project_info",
    "shaft_run_validation",
    "shaft_extract_allure_failures",
    "shaft_list_report_artifacts",
    "shaft_read_report_artifact",
  ];
  for (const tool of requiredTools) {
    if (!toolNames.includes(tool)) {
      throw new Error(`Missing MCP tool: ${tool}`);
    }
  }

  const info = await client.callTool({
    name: "shaft_project_info",
    arguments: {},
  });
  const text = info.content?.[0]?.text || "";
  if (!text.includes("SHAFT_ENGINE")) {
    throw new Error("shaft_project_info did not return SHAFT_ENGINE metadata.");
  }

  await client.callTool({
    name: "shaft_list_report_artifacts",
    arguments: {},
  });

  await client.callTool({
    name: "shaft_extract_allure_failures",
    arguments: { resultsDir: "allure-results", limit: 5 },
  });

  console.log(`Smoke test passed. Tools: ${toolNames.join(", ")}`);
} finally {
  await client.close();
}
