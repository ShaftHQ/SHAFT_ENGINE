#!/usr/bin/env node

import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { spawn } from "node:child_process";
import { promises as fs } from "node:fs";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";
import { z } from "zod";

const DEFAULT_TIMEOUT_MS = 10 * 60 * 1000;
const MAX_OUTPUT_CHARS = 24_000;
const MAX_ARTIFACT_CHARS = 80_000;
const MAX_FAILURES = 50;

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const packageRoot = path.resolve(__dirname, "..");

const workspaceRoot = await resolveWorkspaceRoot();

const server = new McpServer({
  name: "shaft-mcp-server",
  version: "0.1.0",
});

server.tool(
  "shaft_project_info",
  "Use this first in a SHAFT Engine workspace. Returns repository, Maven, Java config, and report-artifact inventory without running tests.",
  {},
  async () => withToolErrors(async () => {
    const pom = await readTextIfExists(path.join(workspaceRoot, "pom.xml"));
    const packageVersion = firstMatch(pom, /<version>([^<]+)<\/version>/);
    const artifactId = firstMatch(pom, /<artifactId>([^<]+)<\/artifactId>/);
    const groupId = firstMatch(pom, /<groupId>([^<]+)<\/groupId>/);
    const javaVersion = (await readTextIfExists(path.join(workspaceRoot, ".java-version"))).trim();
    const sdkman = (await readTextIfExists(path.join(workspaceRoot, ".sdkmanrc"))).trim();
    const git = await runProcess("git", ["rev-parse", "--short=12", "HEAD"], { timeoutMs: 15_000, allowFailure: true });
    const reports = await collectReportInventory();

    return jsonContent({
      workspaceRoot,
      maven: {
        groupId,
        artifactId,
        version: packageVersion,
      },
      java: {
        javaVersionFile: javaVersion || null,
        sdkman: sdkman || null,
      },
      git: {
        shortHead: git.exitCode === 0 ? git.stdout.trim() : null,
      },
      reports,
    });
  })
);

server.tool(
  "shaft_run_validation",
  "Run an allow-listed SHAFT Maven validation command. Use this for compile/test evidence; it does not execute arbitrary shell commands.",
  {
    profile: z.enum(["validate", "compile", "test-compile", "specific-test", "unit-tests-regex"])
      .describe("Validation profile to run."),
    testSelector: z.string().trim().min(1).max(250).optional()
      .describe("Required for specific-test. Example: testPackage.unitTests.JavaHelperUnitTest"),
    timeoutMs: z.number().int().min(30_000).max(3_600_000).default(DEFAULT_TIMEOUT_MS)
      .describe("Maximum runtime in milliseconds."),
  },
  async ({ profile, testSelector, timeoutMs }) => withToolErrors(async () => {
    const args = mavenArgsForProfile(profile, testSelector);
    const result = await runProcess(mavenCommand(), args, {
      cwd: workspaceRoot,
      timeoutMs,
      allowFailure: true,
      env: process.env,
    });

    return jsonContent({
      command: [mavenCommand(), ...args],
      exitCode: result.exitCode,
      timedOut: result.timedOut,
      stdout: trimAndRedact(result.stdout),
      stderr: trimAndRedact(result.stderr),
      reportInventory: await collectReportInventory(),
    });
  })
);

server.tool(
  "shaft_extract_allure_failures",
  "Read SHAFT Allure result JSON and summarize failed/broken/skipped/passed counts plus failing test names. Use after running tests.",
  {
    resultsDir: z.string().trim().min(1).max(260).default("allure-results")
      .describe("Path to Allure results, relative to the SHAFT workspace."),
    limit: z.number().int().min(1).max(200).default(MAX_FAILURES)
      .describe("Maximum failure records to return."),
  },
  async ({ resultsDir, limit }) => withToolErrors(async () => {
    const resolved = safeWorkspacePath(resultsDir);
    const files = await listFilesIfExists(resolved, (name) => name.endsWith("-result.json"));
    const summary = {
      resultsDir: path.relative(workspaceRoot, resolved) || ".",
      totalResultFiles: files.length,
      statuses: {},
      failures: [],
    };

    for (const file of files) {
      const raw = await fs.readFile(file, "utf8");
      const result = JSON.parse(raw);
      const status = result.status || "unknown";
      summary.statuses[status] = (summary.statuses[status] || 0) + 1;
      if ((status === "failed" || status === "broken") && summary.failures.length < limit) {
        summary.failures.push({
          name: result.fullName || result.name || path.basename(file),
          status,
          message: trimAndRedact(result.statusDetails?.message || "", 2_000),
          trace: trimAndRedact(result.statusDetails?.trace || "", 4_000),
        });
      }
    }

    return jsonContent(summary);
  })
);

server.tool(
  "shaft_list_report_artifacts",
  "List SHAFT-generated report artifacts such as Allure results, Surefire reports, execution summaries, and generated reports.",
  {},
  async () => withToolErrors(async () => jsonContent(await collectReportInventory()))
);

server.tool(
  "shaft_read_report_artifact",
  "Read a text report artifact from known SHAFT report directories. Use for Surefire summaries, execution summaries, and small JSON report files.",
  {
    artifactPath: z.string().trim().min(1).max(500)
      .describe("Relative path under a known report directory, for example target/surefire-reports/TestSuite.txt."),
    maxChars: z.number().int().min(1_000).max(MAX_ARTIFACT_CHARS).default(24_000)
      .describe("Maximum characters to return."),
  },
  async ({ artifactPath, maxChars }) => withToolErrors(async () => {
    const resolved = safeReportArtifactPath(artifactPath);
    const stat = await fs.stat(resolved);
    if (!stat.isFile()) {
      throw new Error(`Report artifact is not a file: ${artifactPath}`);
    }
    const text = await fs.readFile(resolved, "utf8");
    return textContent(trimAndRedact(text, maxChars));
  })
);

server.resource(
  "shaft-agents-guidance",
  "shaft://guidance/agents",
  async (uri) => ({
    contents: [{
      uri: uri.href,
      mimeType: "text/markdown",
      text: await readTextIfExists(path.join(workspaceRoot, "AGENTS.md")),
    }],
  })
);

server.resource(
  "shaft-project-pom",
  "shaft://project/pom",
  async (uri) => ({
    contents: [{
      uri: uri.href,
      mimeType: "application/xml",
      text: await readTextIfExists(path.join(workspaceRoot, "pom.xml")),
    }],
  })
);

const transport = new StdioServerTransport();
await server.connect(transport);

async function resolveWorkspaceRoot() {
  const configured = process.env.SHAFT_WORKSPACE || process.cwd();
  const resolved = path.resolve(configured);
  const pomPath = path.join(resolved, "pom.xml");
  try {
    const pom = await fs.readFile(pomPath, "utf8");
    if (!pom.includes("<artifactId>SHAFT_ENGINE</artifactId>")) {
      throw new Error(`pom.xml at ${pomPath} is not SHAFT_ENGINE`);
    }
  } catch (error) {
    throw new Error(`SHAFT_WORKSPACE must point to a SHAFT_ENGINE checkout. ${error.message}`);
  }
  return resolved;
}

function mavenArgsForProfile(profile, testSelector) {
  const common = ["-Dgpg.skip=true"];
  switch (profile) {
    case "validate":
      return ["validate", ...common];
    case "compile":
      return ["clean", "compile", "-DskipTests", ...common];
    case "test-compile":
      return ["test-compile", ...common];
    case "specific-test":
      if (!testSelector) {
        throw new Error("testSelector is required when profile is specific-test.");
      }
      return ["test", ...common, `-Dtest=${testSelector}`];
    case "unit-tests-regex":
      return [
        "test",
        ...common,
        "-DdefaultElementIdentificationTimeout=5",
        "-DexecutionAddress=local",
        "-DtargetOperatingSystem=WINDOWS",
        "-DheadlessExecution=true",
        "-Dtest=%regex[.*UnitTest.*]",
      ];
    default:
      throw new Error(`Unsupported validation profile: ${profile}`);
  }
}

function mavenCommand() {
  return process.platform === "win32" ? "mvn.cmd" : "mvn";
}

async function collectReportInventory() {
  const knownDirs = [
    "allure-results",
    "allure-report",
    "target/surefire-reports",
    "execution-summary",
    "generatedReport",
    "performanceReport",
    "lighthouse-reports",
    "accessibility-reports",
  ];
  const inventory = {};
  for (const relativeDir of knownDirs) {
    const absoluteDir = path.join(workspaceRoot, relativeDir);
    const files = await listFilesIfExists(absoluteDir);
    inventory[relativeDir] = {
      exists: files.length > 0 || await pathExists(absoluteDir),
      fileCount: files.length,
      sampleFiles: files.slice(0, 25).map((file) => normalizePath(path.relative(workspaceRoot, file))),
    };
  }
  return inventory;
}

async function listFilesIfExists(root, filter = () => true) {
  if (!await pathExists(root)) {
    return [];
  }
  const files = [];
  await walk(root, files, filter);
  files.sort();
  return files;
}

async function walk(current, files, filter) {
  const entries = await fs.readdir(current, { withFileTypes: true });
  for (const entry of entries) {
    const absolute = path.join(current, entry.name);
    if (entry.isDirectory()) {
      await walk(absolute, files, filter);
    } else if (entry.isFile() && filter(entry.name, absolute)) {
      files.push(absolute);
    }
  }
}

async function readTextIfExists(filePath) {
  try {
    return await fs.readFile(filePath, "utf8");
  } catch (error) {
    if (error.code === "ENOENT") {
      return "";
    }
    throw error;
  }
}

async function pathExists(filePath) {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

function runProcess(command, args, options = {}) {
  const {
    cwd = workspaceRoot,
    timeoutMs = DEFAULT_TIMEOUT_MS,
    allowFailure = false,
    env = process.env,
  } = options;

  return new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      cwd,
      env,
      shell: false,
      windowsHide: true,
    });
    let stdout = "";
    let stderr = "";
    let timedOut = false;
    const timeout = setTimeout(() => {
      timedOut = true;
      child.kill("SIGTERM");
    }, timeoutMs);

    child.stdout.on("data", (chunk) => {
      stdout += chunk.toString();
      if (stdout.length > MAX_OUTPUT_CHARS * 2) {
        stdout = stdout.slice(-MAX_OUTPUT_CHARS);
      }
    });
    child.stderr.on("data", (chunk) => {
      stderr += chunk.toString();
      if (stderr.length > MAX_OUTPUT_CHARS * 2) {
        stderr = stderr.slice(-MAX_OUTPUT_CHARS);
      }
    });
    child.on("error", (error) => {
      clearTimeout(timeout);
      if (allowFailure) {
        resolve({ exitCode: -1, timedOut, stdout, stderr: error.message });
      } else {
        reject(error);
      }
    });
    child.on("close", (exitCode) => {
      clearTimeout(timeout);
      const result = { exitCode, timedOut, stdout, stderr };
      if (!allowFailure && exitCode !== 0) {
        reject(new Error(`${command} exited ${exitCode}: ${stderr || stdout}`));
      } else {
        resolve(result);
      }
    });
  });
}

function safeWorkspacePath(relativePath) {
  const resolved = path.resolve(workspaceRoot, relativePath);
  if (!resolved.startsWith(workspaceRoot + path.sep) && resolved !== workspaceRoot) {
    throw new Error(`Path escapes SHAFT workspace: ${relativePath}`);
  }
  return resolved;
}

function safeReportArtifactPath(relativePath) {
  const normalized = normalizePath(relativePath);
  const allowedRoots = [
    "allure-results/",
    "target/surefire-reports/",
    "execution-summary/",
    "generatedReport/",
    "performanceReport/",
    "lighthouse-reports/",
    "accessibility-reports/",
  ];
  if (!allowedRoots.some((root) => normalized.startsWith(root))) {
    throw new Error(`Only known SHAFT report artifacts can be read. Rejected: ${relativePath}`);
  }
  return safeWorkspacePath(normalized);
}

function trimAndRedact(value, maxChars = MAX_OUTPUT_CHARS) {
  const redacted = redactSecrets(String(value ?? ""));
  if (redacted.length <= maxChars) {
    return redacted;
  }
  return `${redacted.slice(0, maxChars)}\n...[truncated ${redacted.length - maxChars} chars]`;
}

function redactSecrets(value) {
  return value
    .replace(/(authorization\s*[:=]\s*)([^\s,;]+)/gi, "$1[REDACTED]")
    .replace(/((?:access[_-]?key|api[_-]?key|api[_-]?secret|token|password|passwd|secret|cookie|set-cookie)\s*[:=]\s*)([^\s,;]+)/gi, "$1[REDACTED]")
    .replace(/(https?:\/\/[^:\s/@]+:)([^@\s/]+)(@)/gi, "$1[REDACTED]$3")
    .replace(/(measurement_id=[^&\s]+&api_secret=)([^&\s]+)/gi, "$1[REDACTED]");
}

function firstMatch(value, regex) {
  return regex.exec(value)?.[1] ?? null;
}

function normalizePath(value) {
  return value.replace(/\\/g, "/");
}

function jsonContent(value) {
  return {
    content: [{ type: "text", text: JSON.stringify(value, null, 2) }],
  };
}

function textContent(value) {
  return {
    content: [{ type: "text", text: value }],
  };
}

async function withToolErrors(handler) {
  try {
    return await handler();
  } catch (error) {
    return {
      content: [{ type: "text", text: `SHAFT MCP tool failed: ${redactSecrets(error.message)}` }],
      isError: true,
    };
  }
}
