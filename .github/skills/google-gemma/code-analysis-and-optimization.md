# Skill: Code Analysis and Optimization

## Skill Name

`code-analysis-and-optimization`

## Description

This skill is designed to ingest source code (e.g., from a file, a URL, or a repository structure) and perform a comprehensive static and dynamic analysis to identify potential performance bottlenecks. It provides actionable, prioritized performance optimization recommendations for a given codebase.

## Prerequisites

- **Target language**: Java 21+ (primary), with general applicability to JVM-based projects
- **Build systems**: Maven (`pom.xml`) or Gradle (`build.gradle` / `build.gradle.kts`)
- **Input format**: Source code via repository URL, file upload, directory path, or inline code snippets
- **Recommended context**: Provide build configuration files and relevant test code alongside application source for complete analysis

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

### 3. Java 21 Specific Analysis

#### 3.1 Virtual Threads

- Identify blocking operations in platform threads that could benefit from **virtual threads** (`Thread.ofVirtual()`)
- Detect `synchronized` blocks that pin virtual threads to carrier threads — recommend `ReentrantLock` instead
- Flag thread pool configurations (`Executors.newFixedThreadPool()`) that could use `Executors.newVirtualThreadPerTaskExecutor()`
- Identify thread-per-request patterns that benefit from virtual thread migration

#### 3.2 Modern Language Features

- Detect opportunities to use **pattern matching** (`instanceof` patterns, switch expressions) for cleaner, more efficient code
- Flag verbose `if-else` chains that can be replaced with **switch pattern matching**
- Identify data carrier classes that should use **records** for reduced memory footprint and clearer intent
- Detect `sealed` class hierarchies that enable exhaustive pattern matching
- Flag text processing using string concatenation where **text blocks** improve readability

#### 3.3 Modern API Usage

- Identify uses of legacy `Date`/`Calendar` APIs — recommend `java.time` replacements
- Detect `Optional` anti-patterns (e.g., `Optional.get()` without `isPresent()`, nested Optionals)
- Flag collection operations that could use **sequenced collections** (`SequencedCollection`, `SequencedMap`)
- Identify string operations that could benefit from `String.formatted()` or `StringTemplate` (preview)

### 4. Dynamic Analysis (Simulated / Conceptual)

#### 4.1 Profiling Simulation

- Suggest where to place profiling tools for real-world testing:
  - **Java Flight Recorder (JFR)**: Recommend JFR event configurations for CPU, memory, and I/O profiling
  - **VisualVM**: Suggest heap dump trigger points and CPU sampling configurations
  - **JMH (Java Microbenchmark Harness)**: Identify methods suitable for microbenchmarking
- Provide sample profiling commands and configurations

#### 4.2 Algorithm Refactoring Suggestions

- Propose alternative, more **time-efficient algorithms** for known slow sections:
  - Replace O(n²) with O(n log n) alternatives where applicable
  - Suggest caching/memoization for repeated computations
  - Recommend lazy evaluation for expensive operations
  - Propose stream API optimizations (parallel streams where safe)
- Provide before/after code comparisons

#### 4.3 Dependency Analysis

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
| **Category** | Complexity / Memory / Concurrency / I/O / Algorithm / Dependency / Java 21 |
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
- _"Check my SHAFT test code for ThreadLocal leaks and WebDriver lifecycle issues"_

## Summary of Action

This skill is a sophisticated tool capable of deep static and dynamic code inspection. It provides **actionable, prioritized performance optimization recommendations** for a given codebase by:

1. **Ingesting** the source code from a repository URL, file upload, or direct input
2. **Analyzing** the code across four static analysis dimensions (complexity, memory, concurrency, I/O)
3. **Evaluating** Java 21 modernization opportunities (virtual threads, pattern matching, records, modern APIs)
4. **Simulating** dynamic analysis by recommending profiling strategies and tool placements
5. **Suggesting** concrete algorithm refactorings and dependency optimizations
6. **Generating** a structured, severity-prioritized report with runnable fix suggestions

The goal is to empower developers and test automation engineers to proactively identify and resolve performance issues before they impact production systems.

## Usage with SHAFT_ENGINE

When applied to the SHAFT_ENGINE codebase specifically, this skill should pay special attention to:

- **ThreadLocal usage patterns** — SHAFT makes extensive use of ThreadLocal for parallel test execution; verify proper cleanup via `.remove()` in `@AfterMethod` / `@AfterClass` lifecycle hooks
- **WebDriver lifecycle management** — Ensure driver instances are properly created in `@BeforeMethod` and quit in `@AfterMethod(alwaysRun = true)` to prevent resource leaks
- **Fluent API chain performance** — Analyze method chaining patterns (`.and().element().assertThat()`) for unnecessary intermediate object creation
- **Test data loading** — Review `SHAFT.TestData.JSON` / `JSONFileManager` parsing for efficiency in large test suites; verify `ThreadLocal<Reader>` cleanup
- **Synchronization in element actions** — Analyze wait/retry mechanisms (e.g., `RetryAnalyzer`) for potential timing inefficiencies
- **Allure reporting overhead** — Identify any reporting-related performance bottlenecks during test execution
- **Virtual thread readiness** — Evaluate whether SHAFT's parallel test execution could benefit from Java 21 virtual threads, and identify `synchronized` blocks that would pin virtual threads

### SHAFT-Specific Analysis Checklist

When analyzing SHAFT code, verify these patterns:

| Pattern | What to Check |
|---|---|
| `ThreadLocal<SHAFT.GUI.WebDriver>` | Verify `driver.remove()` is called on the ThreadLocal itself after `driver.get().quit()` (two separate calls) |
| `@AfterMethod` / `@AfterClass` | Must include `alwaysRun = true` |
| `SHAFT.Properties.flags` | Engine-global state — check for thread safety in parallel test classes |
| `RestActions.buildNewRequest()` | Verify response objects are not retained beyond test scope |
| `Locator.hasTagName().build()` | Check for repeated locator builds that could be cached as `static final` |
| `DriverFactoryHelper` | Verify driver creation/teardown does not leak browser processes |
| `ValidationsExecutor` | Check ThreadLocal cleanup in `finally` blocks |

### Example Analysis Output

Below is an example of what a partial report might look like when applied to SHAFT test code:

```
## Performance Analysis Report

### Summary
- Total issues found: 3
- Critical: 0 | High: 1 | Medium: 1 | Low: 1

### Issue #1

| Field | Value |
|---|---|
| **Severity Level** | High |
| **Category** | Memory |
| **Location** | `src/test/java/testPackage/MyTests.java:45` |
| **Description** | ThreadLocal<WebDriver> is set in @BeforeMethod but never removed in @AfterMethod. Only quit() is called, leaving the ThreadLocal reference. |
| **Impact** | In long-running test suites with parallel execution, leaked ThreadLocal entries can cause memory growth proportional to the number of threads × test methods. |
| **Suggested Fix** | In @AfterMethod, call `driver.get().quit();` then `driver.remove();` as two separate statements on the ThreadLocal variable |

### Issue #2

| Field | Value |
|---|---|
| **Severity Level** | Medium |
| **Category** | Java 21 |
| **Location** | `src/main/java/com/shaft/api/RestActions.java:120` |
| **Description** | Uses `synchronized` block for request building. If migrated to virtual threads, this would pin the virtual thread to a carrier thread. |
| **Impact** | Prevents future virtual thread adoption for parallel API testing without code changes. |
| **Suggested Fix** | Replace `synchronized` with `ReentrantLock` for virtual-thread compatibility |

### Issue #3

| Field | Value |
|---|---|
| **Severity Level** | Low |
| **Category** | Algorithm |
| **Location** | `src/test/java/testPackage/DataDrivenTests.java:30` |
| **Description** | Locator `Locator.hasTagName("input").hasAttribute("name", "q").build()` is rebuilt on every test invocation inside @Test method. |
| **Impact** | Minor overhead from repeated XPath compilation; negligible for small suites but measurable at scale. |
| **Suggested Fix** | Extract locator to a `static final By` field at class level |
```

## Limitations

- **No runtime execution**: This skill performs static analysis and simulated dynamic analysis only — it does not execute code or run profilers
- **Heuristic-based**: Concurrency and memory analysis are based on code patterns, not actual runtime behavior; false positives are possible
- **Scope boundaries**: Analysis depth depends on the amount of source code provided; partial codebases may yield incomplete results
- **Framework assumptions**: SHAFT-specific recommendations assume standard SHAFT patterns; custom extensions may not be fully covered
- **External dependencies**: The skill cannot analyze the internal implementation of third-party libraries, only their usage patterns
