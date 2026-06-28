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
            SHAFT integration for IntelliJ IDEA with recorder, playback, Doctor, Healer, Inspector, MCP, guide search,
            and optional Autobot chat routing through SHAFT MCP.
        """.trimIndent()
        changeNotes = """
            <ul>
              <li>Initial beta IntelliJ IDEA integration for SHAFT.</li>
              <li>Adds thin MCP-backed tool windows for Recorder, Playback, Doctor, Healer, Inspector, MCP, and Guide.</li>
              <li>Adds optional SHAFT Autobot Ask, Plan, and Agent view for local Codex, Claude Code, and Copilot CLI routing.</li>
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
        channels = listOf("beta")
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

    test {
        useJUnitPlatform()
    }
}
