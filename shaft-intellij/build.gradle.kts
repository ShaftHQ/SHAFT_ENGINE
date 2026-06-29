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
    implementation("org.commonmark:commonmark:0.29.0")
    testImplementation("org.junit.jupiter:junit-jupiter:6.1.0")
    testRuntimeOnly("junit:junit:4.13.2")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

java {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
}

intellijPlatform {
    buildSearchableOptions = true
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
        systemProperty("shaft.intellij.screenshotDir", System.getProperty("shaft.intellij.screenshotDir", ""))
    }
}
