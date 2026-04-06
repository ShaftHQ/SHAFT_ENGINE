# Skill: Code Analysis and Optimization

## Skill Name

`code-analysis-and-optimization`

## Description

This skill is designed to ingest source code (e.g., from a file, a URL, or a repository structure) and perform a comprehensive static and dynamic analysis to identify potential performance bottlenecks. It provides actionable, prioritized performance optimization recommendations for a given codebase.

## Required Capabilities

### 1. Repository / Code Ingestion

- **Ability to clone, fetch, or parse** the structure of a Git repository or a set of source files
- Accepts input as:
  - A URL to a GitHub repository
  - Direct source code files or snippets
  - A directory path to a local project
- Parses project structure including build files (e.g., `pom.xml`, `build.gradle`), source directories, and configuration files

### 2. Static Analysis

#### 2.1 Complexity Analysis

- Calculate and report **Big O notation** for major algorithms and data structures used
- Identify methods with high cyclomatic complexity
- Flag deeply nested loops and recursive calls that may indicate exponential or polynomial time complexity
- Analyze sorting, searching, and traversal algorithms for suboptimal implementations

#### 2.2 Memory Profiling

- Identify potential **memory leaks** (e.g., unclosed resources, static collections that grow unbounded)
- Detect **excessive object creation** (e.g., object allocation inside tight loops, unnecessary autoboxing)
- Flag **inefficient data structure choices**:
  - Using `HashMap` vs. `TreeMap` when ordering is unnecessary
  - Using `ArrayList` vs. `LinkedList` based on access patterns
  - Oversized initial capacities or missing initial capacity hints
- Identify missing `try-with-resources` patterns for `AutoCloseable` objects
- Detect `ThreadLocal` variables that are never cleaned up

#### 2.3 Concurrency Analysis

- Identify potential **race conditions** (shared mutable state accessed without synchronization)
- Detect potential **deadlocks** (lock ordering violations, nested synchronized blocks)
- Flag **inefficient synchronization mechanisms**:
  - Overly broad `synchronized` blocks
  - Use of `synchronized` where `ReentrantLock` or `StampedLock` would be more efficient
  - Missing use of concurrent collections (e.g., `ConcurrentHashMap` vs. synchronized `HashMap`)
- Identify thread-unsafe singleton implementations
- Detect improper use of `volatile` keyword

#### 2.4 I/O Bottleneck Identification

- Analyze **file system access** patterns:
  - Unbuffered reads/writes
  - Excessive file open/close operations
  - Missing NIO usage for large file operations
- Identify **network call** inefficiencies:
  - Sequential API calls that could be parallelized
  - Missing connection pooling
  - Lack of timeout configurations
- Analyze **database interaction patterns**:
  - N+1 query problems
  - Missing batch operations
  - Unoptimized query patterns
  - Missing connection pool configuration

### 3. Dynamic Analysis (Simulated / Conceptual)

#### 3.1 Profiling Simulation

- Suggest where to place profiling tools for real-world testing:
  - **Java Flight Recorder (JFR)**: Recommend JFR event configurations for CPU, memory, and I/O profiling
  - **VisualVM**: Suggest heap dump trigger points and CPU sampling configurations
  - **JMH (Java Microbenchmark Harness)**: Identify methods suitable for microbenchmarking
- Provide sample profiling commands and configurations

#### 3.2 Algorithm Refactoring Suggestions

- Propose alternative, more **time-efficient algorithms** for known slow sections:
  - Replace O(n²) with O(n log n) alternatives where applicable
  - Suggest caching/memoization for repeated computations
  - Recommend lazy evaluation for expensive operations
  - Propose stream API optimizations (parallel streams where safe)
- Provide before/after code comparisons

#### 3.3 Dependency Analysis

- Review **third-party libraries and frameworks** for:
  - Known performance pitfalls (e.g., reflection-heavy libraries, blocking I/O in async contexts)
  - Outdated dependencies with known slow implementations
  - Availability of more performant alternatives
  - Unnecessary transitive dependencies adding overhead
- Flag deprecated APIs that may have performance implications

## Output Generation

The skill produces a structured report with the following format for each identified issue:

### Report Structure

```
## Performance Analysis Report

### Summary
- Total issues found: [count]
- Critical: [count] | High: [count] | Medium: [count] | Low: [count]

### Issue #[N]

| Field | Value |
|---|---|
| **Severity Level** | Critical / High / Medium / Low |
| **Category** | Complexity / Memory / Concurrency / I/O / Algorithm / Dependency |
| **Location** | `[file_path]:[line_number]` |
| **Description** | A clear explanation of the performance issue |
| **Impact** | Estimated performance impact and affected scenarios |
| **Suggested Fix** | Concrete, runnable code snippet demonstrating the optimized solution |
```

### Severity Levels

| Level | Definition |
|---|---|
| **Critical** | Causes application crashes, data corruption, or severe performance degradation (e.g., deadlocks, memory leaks in production paths) |
| **High** | Significant performance impact affecting user experience or resource consumption (e.g., O(n²) in hot paths, N+1 queries) |
| **Medium** | Moderate performance impact that should be addressed in normal development cycles (e.g., suboptimal data structures, missing buffering) |
| **Low** | Minor optimization opportunities or best practice improvements (e.g., unnecessary object creation, missing capacity hints) |

## Trigger Condition

This skill should be triggered when:

1. A user provides a **URL to a GitHub repository** for analysis
2. A user provides **source code files directly** (paste, upload, or file path)
3. A user requests a **performance review** or **code optimization** for an existing codebase
4. A user asks to **identify bottlenecks** or **improve performance** in their code

### Example Trigger Prompts

- _"Analyze this repository for performance issues: https://github.com/example/project"_
- _"Review the following Java code for performance bottlenecks: [code]"_
- _"What are the performance issues in my SHAFT test automation framework?"_
- _"Optimize the concurrency patterns in this codebase"_

## Summary of Action

This skill is a sophisticated tool capable of deep static and dynamic code inspection. It provides **actionable, prioritized performance optimization recommendations** for a given codebase by:

1. **Ingesting** the source code from a repository URL, file upload, or direct input
2. **Analyzing** the code across four static analysis dimensions (complexity, memory, concurrency, I/O)
3. **Simulating** dynamic analysis by recommending profiling strategies and tool placements
4. **Suggesting** concrete algorithm refactorings and dependency optimizations
5. **Generating** a structured, severity-prioritized report with runnable fix suggestions

The goal is to empower developers and test automation engineers to proactively identify and resolve performance issues before they impact production systems.

## Usage with SHAFT_ENGINE

When applied to the SHAFT_ENGINE codebase specifically, this skill should pay special attention to:

- **ThreadLocal usage patterns** — SHAFT makes extensive use of ThreadLocal for parallel test execution; verify proper cleanup
- **WebDriver lifecycle management** — Ensure driver instances are properly managed and not leaked
- **Fluent API chain performance** — Analyze method chaining patterns for unnecessary intermediate object creation
- **Test data loading** — Review JSON test data parsing for efficiency in large test suites
- **Synchronization in element actions** — Analyze wait/retry mechanisms for potential timing inefficiencies
- **Allure reporting overhead** — Identify any reporting-related performance bottlenecks during test execution
