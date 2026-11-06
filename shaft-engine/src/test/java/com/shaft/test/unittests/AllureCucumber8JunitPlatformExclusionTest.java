package com.shaft.test.unittests;

import io.qameta.allure.cucumber8jvm.AllureCucumber8Jvm;
import io.qameta.allure.junitplatform.AllureJunitPlatform;
import io.qameta.allure.testng.AllureTestNg;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;

/**
 * allure-java 3.1.1 skips JUnit Platform reporting for the Cucumber engine when
 * {@code allure-cucumber8-jvm} is on the classpath (allure-java#1499 / SHAFT #7758).
 */
public class AllureCucumber8JunitPlatformExclusionTest {
    @Test
    public void junitPlatformSkipsCucumberEngineWhenCucumber8AdapterIsPresent() throws Exception {
        Assert.assertNotNull(AllureCucumber8Jvm.class.getProtectionDomain().getCodeSource(),
                "Cucumber 8 adapter must be on the test classpath so the exclusion can see it");

        String junitPlatformJar = AllureJunitPlatform.class.getProtectionDomain().getCodeSource().getLocation().toString();
        Assert.assertTrue(junitPlatformJar.contains("allure-junit-platform-3.1.1.jar"), junitPlatformJar);

        Method skip = AllureJunitPlatform.class.getDeclaredMethod("shouldSkipCucumberEngine", String.class);
        skip.setAccessible(true);
        Assert.assertEquals(Boolean.TRUE, skip.invoke(null, "cucumber"));
        Assert.assertEquals(Boolean.FALSE, skip.invoke(null, "junit-jupiter"));

        String testNgJar = AllureTestNg.class.getProtectionDomain().getCodeSource().getLocation().toString();
        Assert.assertTrue(testNgJar.contains("allure-testng-3.1.1.jar"), testNgJar);
        try (var bytecode = AllureTestNg.class.getResourceAsStream("AllureTestNg.class")) {
            Assert.assertNotNull(bytecode);
            String testNgBytecode = new String(bytecode.readAllBytes(), StandardCharsets.ISO_8859_1);
            Assert.assertTrue(testNgBytecode.contains("io.qameta.allure.cucumber8jvm.AllureCucumber8Jvm"),
                    "allure-testng 3.1.1 must probe the Cucumber 8 adapter before reporting");
        }
    }
}
