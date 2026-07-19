pluginManagement {
    repositories {
        gradlePluginPortal()
        mavenCentral()
    }
}

rootProject.name = "shaft-intellij"

// Issue #3784: on Windows, running this build's Gradle Daemon on JDK 25 can crash the daemon
// itself (EXCEPTION_ACCESS_VIOLATION inside G1 GC) instead of failing the build normally. CI
// (.github/workflows/intellij-plugin.yml) builds this module on JDK 17 and local dev is
// documented (shaft-mastery intellij-plugin chapter) to use JDK 21 -- both are proven-good, so
// only reject newer, unverified JDKs here rather than pinning to a single exact version (a
// Gradle Daemon JVM criteria / toolchain pin was considered and rejected: it takes precedence
// over JAVA_HOME/-Dorg.gradle.java.home, which would force CI's JDK 17 job onto a JDK 21 it
// never installs). Evaluating this in settings.gradle.kts runs on the real Daemon JVM, before
// any of the IntelliJ Platform Gradle plugin's dependency resolution or task execution starts,
// so a bad JDK is rejected with an actionable message before it ever reaches the crash-prone
// code path.
val daemonJavaVersion = JavaVersion.current()
val maxSupportedDaemonJavaVersion = JavaVersion.VERSION_21
if (daemonJavaVersion > maxSupportedDaemonJavaVersion) {
    throw GradleException(
        "shaft-intellij's Gradle Daemon must run on JDK $maxSupportedDaemonJavaVersion or earlier " +
            "(found JDK $daemonJavaVersion). JDK 25 is known to crash the Gradle Daemon on Windows " +
            "(issue #3784: EXCEPTION_ACCESS_VIOLATION in G1 GC). Point JAVA_HOME at a JDK 21 (or " +
            "17) install, or pass -Dorg.gradle.java.home=<path-to-jdk21> without changing JAVA_HOME."
    )
}
