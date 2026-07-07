import org.jetbrains.intellij.platform.gradle.IntelliJPlatformType

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
            SHAFT integration for IntelliJ IDEA with an Assistant-first SHAFT MCP chat surface plus curated recorder,
            playback, Doctor, Healer, Inspector, project, MCP, and guide tools.
        """.trimIndent()
        changeNotes = """
            <ul>
              <li>Adds guided Recorder, Locator, Doctor, Trace, Healer, and locator-proposal workflows.</li>
              <li>Expands Assistant slash commands for recording, inspection, test generation, and failed-test triage.</li>
              <li>Refreshes Advanced Tools from the live SHAFT MCP catalog while preserving curated safe defaults.</li>
              <li>Improves MCP stdio result handling, accessibility metadata, and narrow tool-window tab behavior.</li>
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
        systemProperty("shaft.intellij.liveMcpCommand", System.getProperty("shaft.intellij.liveMcpCommand", ""))
        systemProperty("shaft.intellij.workspaceRoot", System.getProperty("shaft.intellij.workspaceRoot", ".."))
        systemProperty("shaft.intellij.liveGeminiModel",
                System.getProperty("shaft.intellij.liveGeminiModel", "gemini-3.5-flash"))
    }
}
