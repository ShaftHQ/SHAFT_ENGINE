pluginManagement {
    repositories {
        gradlePluginPortal()
        mavenCentral()
    }
}

rootProject.name = "shaft-intellij"

// The checked-in Gradle 9.3 wrapper supports running on Java 25. Keep the upper
// bound explicit so a future ambient JDK cannot silently exceed Gradle's tested
// runtime matrix; raise it together with the wrapper and its verification.
val maxSupportedDaemonJavaVersion = JavaVersion.VERSION_25
if (JavaVersion.current() > maxSupportedDaemonJavaVersion) {
    throw GradleException(
        "shaft-intellij's Gradle Daemon supports JDK $maxSupportedDaemonJavaVersion or earlier " +
            "with the checked-in Gradle wrapper. Point JAVA_HOME at JDK 25, or upgrade and " +
            "verify the wrapper before raising this ceiling."
    )
}
