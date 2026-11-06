#!/usr/bin/env bash
# Rewrite a checked-out appium-flutter-server demo-app Android project onto the
# Flutter 3.47.9 template toolchain so Gradle can run on Java 25 LTS (#7753).
#
# Flutter 3.47.9 is the current stable release (no separate Flutter LTS line).
# Its template and max supported Gradle are 9.3.1, paired with Android Gradle
# Plugin 9.1.0 and Kotlin 2.4.0. Gradle 9.3.1 is the newest Gradle that this
# Flutter release accepts, and Gradle 9.1.0 or newer is what runs on Java 25.
#
# The application id and namespace are kept from the checked-out project.
# Heap is 4G, matching the demo app's previous setting, so a GitHub-hosted
# runner is not asked for the template's 8G default.
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: prepare_flutter_demo_android.sh <demo-app-dir>" >&2
  exit 2
fi

root=$(cd "$1" && pwd)
android="$root/android"
app_build_groovy="$android/app/build.gradle"
app_build_kts="$android/app/build.gradle.kts"

if [ ! -d "$android/app" ]; then
  echo "demo-app android project not found: $android" >&2
  exit 1
fi

read_quoted() {
  local key="$1"
  local file value
  for file in "$app_build_groovy" "$app_build_kts" "$android/build.gradle" "$android/build.gradle.kts"; do
    [ -f "$file" ] || continue
    value=$(sed -n "s/.*${key}[[:space:]]*=\\{0,1\\}[[:space:]]*\"\\([^\"]*\\)\".*/\\1/p" "$file" | head -n 1)
    if [ -n "$value" ]; then
      printf '%s' "$value"
      return 0
    fi
  done
  return 1
}

application_id=$(read_quoted applicationId || true)
namespace=$(read_quoted namespace || true)
if [ -z "$application_id" ]; then
  echo "applicationId not found under $android" >&2
  exit 1
fi
if [ -z "$namespace" ]; then
  namespace="$application_id"
fi

gradle_version=9.3.1
agp_version=9.1.0
kotlin_version=2.4.0

cat >"$android/gradle/wrapper/gradle-wrapper.properties" <<EOF
distributionBase=GRADLE_USER_HOME
distributionPath=wrapper/dists
zipStoreBase=GRADLE_USER_HOME
zipStorePath=wrapper/dists
distributionUrl=https\\://services.gradle.org/distributions/gradle-${gradle_version}-all.zip
EOF

cat >"$android/settings.gradle.kts" <<EOF
// Flutter 3.47.9 template cap: Gradle ${gradle_version}, Android Gradle Plugin ${agp_version}, Kotlin ${kotlin_version}.
pluginManagement {
    val flutterSdkPath =
        run {
            val properties = java.util.Properties()
            file("local.properties").inputStream().use { properties.load(it) }
            val flutterSdkPath = properties.getProperty("flutter.sdk")
            require(flutterSdkPath != null) { "flutter.sdk not set in local.properties" }
            flutterSdkPath
        }

    includeBuild("\$flutterSdkPath/packages/flutter_tools/gradle")

    repositories {
        google()
        mavenCentral()
        gradlePluginPortal()
    }
}

plugins {
    id("dev.flutter.flutter-plugin-loader") version "1.0.0"
    id("com.android.application") version "${agp_version}" apply false
    id("org.jetbrains.kotlin.android") version "${kotlin_version}" apply false
}

include(":app")
EOF

cat >"$android/build.gradle.kts" <<'EOF'
allprojects {
    repositories {
        google()
        mavenCentral()
    }
}

val newBuildDir: Directory =
    rootProject.layout.buildDirectory
        .dir("../../build")
        .get()
rootProject.layout.buildDirectory.value(newBuildDir)

subprojects {
    val newSubprojectBuildDir: Directory = newBuildDir.dir(project.name)
    project.layout.buildDirectory.value(newSubprojectBuildDir)
}
subprojects {
    project.evaluationDependsOn(":app")
}

// Some pinned plugins (permission_handler_android) still compile against SDK 33.
// AndroidX resolved with Flutter 3.47 requires compileSdk 34 or newer. Raising
// every plugin to Flutter's compileSdk 36 makes older Kotlin (package_info_plus
// 8.0.0) fail null checks against the SDK 36 stubs, so only lift projects that
// are still below 34. Reflection keeps this root script off the AGP classpath.
fun forceCompileSdk(project: Project) {
    val android = project.extensions.findByName("android") ?: return
    val current = android.javaClass.methods.firstOrNull { method ->
        method.name == "getCompileSdkVersion" && method.parameterCount == 0
    }?.invoke(android)?.toString()?.filter { it.isDigit() }?.toIntOrNull()
    if (current == null) {
        project.logger.warn("compileSdk lift skipped for ${project.path}: getCompileSdkVersion was not readable")
        return
    }
    if (current >= 34) {
        return
    }
    val setter = android.javaClass.methods.firstOrNull { method ->
        (method.name == "setCompileSdkVersion" || method.name == "compileSdkVersion") &&
            method.parameterTypes.singleOrNull() == Integer.TYPE
    }
    if (setter == null) {
        project.logger.warn(
            "compileSdk lift failed for ${project.path}: no int setCompileSdkVersion " +
                "(current=$current, type=${android.javaClass.name})"
        )
        return
    }
    setter.invoke(android, 34)
}
subprojects {
    if (state.executed) {
        forceCompileSdk(this)
    } else {
        afterEvaluate { forceCompileSdk(this) }
    }
}

// video_player_android 2.5.2 is the only plugin in the pinned demo resolution
// whose own android/build.gradle adds -Werror (example apps are not on this
// classpath). JDK 25 treats Java 8 source/target as a warning, and -Werror
// fails that compile. Other JavaCompile tasks keep their own args.
gradle.projectsEvaluated {
    subprojects {
        if (name != "video_player_android") {
            return@subprojects
        }
        tasks.withType<JavaCompile>().configureEach {
            options.compilerArgs.removeAll(listOf("-Werror"))
        }
    }
}

tasks.register<Delete>("clean") {
    delete(rootProject.layout.buildDirectory)
}
EOF

cat >"$android/gradle.properties" <<'EOF'
org.gradle.jvmargs=-Xmx4G -XX:MaxMetaspaceSize=1G -XX:ReservedCodeCacheSize=512m -XX:+HeapDumpOnOutOfMemoryError
android.useAndroidX=true
# Flutter 3.47.9 template flags. builtInKotlin stays false so the Kotlin
# Gradle plugin version above remains the one Flutter validates.
android.newDsl=false
android.builtInKotlin=false
# JDK 25 javac treats Java 8 source/target as an error under -Werror. The
# pinned demo app still has plugins (video_player_android) that compile as
# Java 8. AGP turns that warning into a failed build unless this is set.
android.javaCompile.suppressSourceTargetDeprecationWarning=true
EOF

cat >"$app_build_kts" <<EOF
plugins {
    id("com.android.application")
    // The Flutter Gradle Plugin must be applied after the Android and Kotlin Gradle plugins.
    id("dev.flutter.flutter-gradle-plugin")
}

android {
    namespace = "${namespace}"
    compileSdk = flutter.compileSdkVersion
    ndkVersion = flutter.ndkVersion

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }

    defaultConfig {
        applicationId = "${application_id}"
        minSdk = flutter.minSdkVersion
        targetSdk = flutter.targetSdkVersion
        versionCode = flutter.versionCode
        versionName = flutter.versionName
    }

    buildTypes {
        release {
            signingConfig = signingConfigs.getByName("debug")
        }
    }
}

kotlin {
    compilerOptions {
        jvmTarget = org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_17
    }
}

flutter {
    source = "../.."
}
EOF

rm -f "$android/settings.gradle" "$android/build.gradle" "$app_build_groovy"
echo "Flutter demo Android toolchain: Gradle ${gradle_version}, AGP ${agp_version}, Kotlin ${kotlin_version}, applicationId ${application_id}"
