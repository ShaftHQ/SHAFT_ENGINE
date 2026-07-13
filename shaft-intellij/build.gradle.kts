import org.jetbrains.intellij.platform.gradle.IntelliJPlatformType
import org.jetbrains.intellij.platform.gradle.tasks.VerifyPluginTask

plugins {
    java
    id("org.jetbrains.intellij.platform") version "2.17.0"
}

group = providers.gradleProperty("pluginGroup").get()
version = providers.gradleProperty("pluginVersion").get()

repositories {
    mavenCentral()
    intellijPlatform {
        defaultRepositories()
    }
}

dependencies {
    intellijPlatform {
        intellijIdea(providers.gradleProperty("platformVersion").get())
        bundledPlugin("com.intellij.java")
        // JUnitConfiguration/TestNGConfiguration classes live in these separate bundled plugins
        // (see io.github.shafthq.shaft-withJUnit.xml / shaft-withTestNG.xml), not in com.intellij.java.
        bundledPlugin("JUnit")
        bundledPlugin("TestNG-J")
    }
    implementation("com.google.code.gson:gson:2.14.0")
    testImplementation("org.junit.jupiter:junit-jupiter:6.1.0")
    testRuntimeOnly("junit:junit:4.13.2")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

java {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
}

intellijPlatform {
    // IntelliJ's traverseUI searchable-options process hangs after emitting JSON on Windows.
    // The plugin verifier still validates the built ZIP; track re-enabling this task separately.
    buildSearchableOptions = false
    instrumentCode = true

    pluginConfiguration {
        id = "io.github.shafthq.shaft"
        name = "SHAFT"
        version = project.version.toString()
        description = """
            <p>SHAFT's official IntelliJ IDEA companion: record, generate, run, and heal automated tests powered by
            <a href="https://shafthq.github.io">SHAFT Engine</a> — through an Assistant-first, chat-driven workflow
            backed by the SHAFT MCP server.</p>

            <h3>Highlights</h3>
            <ul>
              <li><b>Assistant chat</b> — describe a scenario in plain language and get runnable SHAFT test code;
              slash commands cover recording, inspection, test generation, and failed-test triage.</li>
              <li><b>Web recorder</b> — capture real browser sessions and turn them into SHAFT tests, with
              Pick-Locator results flowing straight into the editor.</li>
              <li><b>Run from the gutter</b> — SHAFT-aware JUnit and TestNG run configurations for test classes
              and methods.</li>
              <li><b>SHAFT Tests panel</b> — recent runs at a glance, with one-click Doctor diagnosis and Healer
              locator repair for failures.</li>
              <li><b>Inspector &amp; locator tools</b> — inspect live pages, propose resilient locators, and
              validate them before committing.</li>
              <li><b>Visual baselines &amp; evidence</b> — triage visual-regression diffs and browse execution
              evidence without leaving the IDE.</li>
              <li><b>Guided setup</b> — a wizard installs and verifies the SHAFT MCP server and the assistant
              runtime (Codex or Gemini) in a couple of clicks.</li>
              <li><b>Advanced tools</b> — the full SHAFT MCP tool catalog, refreshed live, with curated safe
              defaults and explicit approval prompts before anything runs.</li>
            </ul>

            <p>The plugin activates only in projects that use SHAFT and stays out of the way everywhere else.</p>
        """.trimIndent()
        changeNotes = """
            <ul>
              <li>Fixes plugin installation failing with "Failed to load the plugin descriptor" caused by a
              malformed optional JUnit integration descriptor.</li>
              <li>Migrates run-configuration producers off deprecated IntelliJ Platform APIs flagged by the
              JetBrains Plugin Verifier.</li>
              <li>Hardens the release pipeline: builds now fail on deprecated API usage, plugin-structure
              defects, and invalid descriptors before anything reaches the Marketplace.</li>
              <li>Refreshes the Marketplace description and screenshots.</li>
            </ul>
        """.trimIndent()
        ideaVersion {
            sinceBuild = providers.gradleProperty("pluginSinceBuild")
            untilBuild = provider { null }
        }
        vendor {
            name = "ShaftHQ"
            email = "Mohab.MohieElDeen@outlook.com"
            url = "https://shafthq.github.io"
        }
    }

    publishing {
        token = providers.gradleProperty("intellijPlatformPublishingToken")
        channels = listOf("default")
    }

    signing {
        certificateChain = providers.gradleProperty("intellijPlatformCertificateChain")
        privateKey = providers.gradleProperty("intellijPlatformPrivateKey")
        password = providers.gradleProperty("intellijPlatformPrivateKeyPassword")
    }

    pluginVerification {
        // The default failure level (COMPATIBILITY_PROBLEMS only) let the 10.3.20260712 release
        // ship with deprecated API usages and a malformed optional descriptor that made the
        // Marketplace archive uninstallable ("Failed to load the plugin descriptor"). Fail CI on
        // anything that would degrade or break the plugin at Marketplace review or install time.
        failureLevel = listOf(
            VerifyPluginTask.FailureLevel.COMPATIBILITY_PROBLEMS,
            VerifyPluginTask.FailureLevel.DEPRECATED_API_USAGES,
            VerifyPluginTask.FailureLevel.SCHEDULED_FOR_REMOVAL_API_USAGES,
            VerifyPluginTask.FailureLevel.INTERNAL_API_USAGES,
            VerifyPluginTask.FailureLevel.OVERRIDE_ONLY_API_USAGES,
            VerifyPluginTask.FailureLevel.NON_EXTENDABLE_API_USAGES,
            VerifyPluginTask.FailureLevel.PLUGIN_STRUCTURE_WARNINGS,
            VerifyPluginTask.FailureLevel.MISSING_DEPENDENCIES,
            VerifyPluginTask.FailureLevel.INVALID_PLUGIN,
        )
        ides {
            current()
            // Pin the build that rejected 10.3.20260707 (LafManager.removeLafManagerListener removal)
            // so a future platform-API removal is caught locally instead of at Marketplace review time.
            create(IntelliJPlatformType.IntellijIdeaUltimate, "262.8665.81")
        }
    }
}

tasks {
    withType<JavaCompile>().configureEach {
        options.release.set(17)
        // Deprecated platform APIs must fail the build at compile time, before the plugin
        // verifier (and long before Marketplace review) sees them.
        options.compilerArgs.addAll(listOf("-Xlint:deprecation", "-Werror"))
    }

    processResources {
        inputs.property("pluginVersion", project.version.toString())
        filesMatching("messages/ShaftBundle.properties") {
            expand("pluginVersion" to project.version.toString())
        }
    }

    test {
        useJUnitPlatform()
        systemProperty("java.awt.headless", "true")
        systemProperty("shaft.intellij.screenshotDir", System.getProperty("shaft.intellij.screenshotDir", ""))
        systemProperty("shaft.intellij.liveGemini", System.getProperty("shaft.intellij.liveGemini", "false"))
        systemProperty("shaft.intellij.liveWorkflows", System.getProperty("shaft.intellij.liveWorkflows", "false"))
        systemProperty("shaft.intellij.liveMcpCommand", System.getProperty("shaft.intellij.liveMcpCommand", ""))
        systemProperty("shaft.intellij.workspaceRoot", System.getProperty("shaft.intellij.workspaceRoot", ".."))
        systemProperty("shaft.intellij.mcp.applicationDataRoot",
                System.getProperty("shaft.intellij.mcp.applicationDataRoot", ""))
        systemProperty("shaft.intellij.mcp.bootstrapRoot",
                System.getProperty("shaft.intellij.mcp.bootstrapRoot", ""))
        systemProperty("shaft.intellij.liveGeminiModel",
                System.getProperty("shaft.intellij.liveGeminiModel", "gemini-3.5-flash"))
    }
}
