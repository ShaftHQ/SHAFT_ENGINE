package com.shaft.intellij.platform;

import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.ConfigurationFromContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.execution.configurations.SimpleJavaParameters;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.ide.plugins.PluginManagerCore;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.util.registry.Registry;
import com.intellij.openapi.vfs.newvfs.impl.VfsRootAccess;
import com.intellij.openapi.wm.ToolWindowEP;
import com.intellij.psi.PsiElement;
import com.intellij.openapi.actionSystem.impl.SimpleDataContext;
import com.intellij.testFramework.fixtures.LightJavaCodeInsightFixtureTestCase;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.PlatformCoreDataKeys;
import com.shaft.intellij.ShaftToolWindowFactory;
import com.shaft.intellij.project.ShaftProjectDetector;
import com.shaft.intellij.testrunner.ShaftGradleRunConfigurationExtension;
import com.shaft.intellij.testrunner.ShaftTestLineMarkerContributor;
import com.shaft.intellij.testrunner.ShaftTestNgRunConfigurationProducer;
import org.jdom.Element;
import org.jetbrains.plugins.gradle.service.execution.GradleExternalTaskConfigurationType;
import org.jetbrains.plugins.gradle.service.execution.GradleRunConfiguration;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Platform-booted SHAFT journeys (#6425-#6428): descriptor, gutter to run configuration, tool window,
 * no-EDT startup work, Kotlin gutter and Gradle overrides, with slow-operation assertions on.
 */
public class ShaftPlatformIntegrationTest extends LightJavaCodeInsightFixtureTestCase {
    private static final long STARTUP_BUDGET_MILLIS = 200;

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        VfsRootAccess.allowRootAccess(getTestRootDisposable(), new java.io.File(".intellijPlatform").getAbsolutePath());
        Registry.get("ide.slow.operations.assertion").setValue(true, getTestRootDisposable());
        Path root = Path.of(getProject().getBasePath());
        Files.createDirectories(root);
        Files.writeString(root.resolve("pom.xml"), "<project><dependency>io.github.shafthq</dependency></project>");
        myFixture.addClass("package org.testng.annotations; public @interface Test {}");
    }

    public void testPluginDescriptorAndToolWindowAreLoaded() {
        assertNotNull(PluginManagerCore.getPlugin(PluginId.getId("io.github.shafthq.shaft")));
        assertTrue(ToolWindowEP.EP_NAME.getExtensionList().stream()
                .anyMatch(ep -> "SHAFT".equals(ep.id) && ShaftToolWindowFactory.class.getName().equals(ep.factoryClass)));
    }

    public void testGutterResolvesShaftTestNgRunConfiguration() {
        myFixture.configureByText("LoginTest.java", """
                class LoginTest { @org.testng.annotations.Test void log<caret>in() {} }
                """);
        PsiElement identifier = myFixture.getFile().findElementAt(myFixture.getCaretOffset());
        assertNotNull(new ShaftTestLineMarkerContributor().getInfo(identifier));
        var data = SimpleDataContext.builder()
                .add(CommonDataKeys.PROJECT, getProject())
                .add(PlatformCoreDataKeys.MODULE, getModule())
                .add(com.intellij.execution.Location.DATA_KEY, com.intellij.execution.PsiLocation.fromPsiElement(identifier))
                .build();
        ConfigurationFromContext fromContext = RunConfigurationProducer.getInstance(ShaftTestNgRunConfigurationProducer.class)
                .createConfigurationFromContext(ConfigurationContext.getFromContext(data, "test"));
        assertNotNull(fromContext);
        assertTrue(fromContext.getConfiguration().getName().contains("login"));
    }

    public void testKotlinTestMethodGetsTheShaftGutter() {
        myFixture.configureByText("LoginTest.kt", """
                class LoginTest { @org.testng.annotations.Test fun log<caret>in() {} }
                """);
        PsiElement identifier = myFixture.getFile().findElementAt(myFixture.getCaretOffset());
        assertNotNull(new ShaftTestLineMarkerContributor().getInfo(identifier));
    }

    public void testStartupWorkRunsOffTheEdtWithinBudget() throws Exception {
        assertTrue(ApplicationManager.getApplication().isDispatchThread());
        long start = System.nanoTime();
        assertTrue(ShaftProjectDetector.isShaftProject(getProject()));
        AtomicBoolean ranOnEdt = new AtomicBoolean(true);
        java.util.concurrent.Future<?> pending = com.shaft.intellij.settings.ShaftPluginUpgradeActivityAccess
                .schedule(() -> ranOnEdt.set(ApplicationManager.getApplication().isDispatchThread()));
        long scheduledMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start);
        pending.get(10, TimeUnit.SECONDS);
        assertFalse("upgrade check must not run on the EDT", ranOnEdt.get());
        assertTrue("startup work took " + scheduledMillis + " ms", scheduledMillis < STARTUP_BUDGET_MILLIS);
    }

    public void testGradleRunConfigurationGetsShaftOverridesAsSystemProperties() {
        GradleRunConfiguration configuration = new GradleRunConfiguration(getProject(),
                GradleExternalTaskConfigurationType.getInstance().getFactory(), "test");
        ShaftGradleRunConfigurationExtension extension = new ShaftGradleRunConfigurationExtension();
        assertTrue(extension.isApplicableFor(configuration));
        Element element = new Element("shaft");
        element.setAttribute("enabled", "true");
        element.setAttribute("browser", "firefox");
        element.setAttribute("headless", "true");
        com.shaft.intellij.testrunner.ShaftRunConfigurationExtensionSupportAccess.readExternal(configuration, element);
        SimpleJavaParameters parameters = new SimpleJavaParameters();
        extension.updateVMParameters(configuration, parameters, null, DefaultRunExecutor.getRunExecutorInstance());
        assertTrue(parameters.getVMParametersList().getList().contains("-DtargetBrowserName=firefox"));
        assertTrue(parameters.getVMParametersList().getList().contains("-DheadlessExecution=true"));
    }
}
