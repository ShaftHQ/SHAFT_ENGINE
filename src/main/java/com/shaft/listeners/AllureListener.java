package com.shaft.listeners;

import com.shaft.listeners.internal.TestNGListenerHelper;
import io.qameta.allure.listener.ContainerLifecycleListener;
import io.qameta.allure.listener.FixtureLifecycleListener;
import io.qameta.allure.listener.StepLifecycleListener;
import io.qameta.allure.listener.TestLifecycleListener;
import io.qameta.allure.model.FixtureResult;
import io.qameta.allure.model.StepResult;
import io.qameta.allure.model.TestResult;
import io.qameta.allure.model.TestResultContainer;

public class AllureListener implements StepLifecycleListener, FixtureLifecycleListener, TestLifecycleListener,
        ContainerLifecycleListener {

    //Before each step starts inside the methods
    @Override
    public void beforeStepStart(StepResult result) {
        StepLifecycleListener.super.beforeStepStart(result);
    }

    //After each step starts inside the methods
    @Override
    public void afterStepStart(StepResult result) {
        StepLifecycleListener.super.afterStepStart(result);
    }

    //Before each step update inside the methods
    @Override
    public void beforeStepUpdate(StepResult result) {
        StepLifecycleListener.super.beforeStepUpdate(result);
    }

    //After each step update inside the methods
    @Override
    public void afterStepUpdate(StepResult result) {
        StepLifecycleListener.super.afterStepUpdate(result);
    }

    //Before each step Stop inside the methods
    @Override
    public void beforeStepStop(StepResult result) {
        StepLifecycleListener.super.beforeStepStop(result);
    }

    //After each step Stop inside the methods
    @Override
    public void afterStepStop(StepResult result) {
        var iTestResult = TestNGListener.getITestResult();
        if (iTestResult != null) {
            TestNGListenerHelper.updateConfigurationMethods(iTestResult);
        }
    }

    //Before The Class starts
    @Override
    public void beforeContainerStart(TestResultContainer container) {
        ContainerLifecycleListener.super.beforeContainerStart(container);
    }

    //After The Class starts
    @Override
    public void afterContainerStart(TestResultContainer container) {
        ContainerLifecycleListener.super.afterContainerStart(container);
    }

    //Before The Class updates
    @Override
    public void beforeContainerUpdate(TestResultContainer container) {
        ContainerLifecycleListener.super.beforeContainerUpdate(container);
    }

    //After The Class updates
    @Override
    public void afterContainerUpdate(TestResultContainer container) {
        ContainerLifecycleListener.super.afterContainerUpdate(container);
    }

    //Before The Class stops
    @Override
    public void beforeContainerStop(TestResultContainer container) {
        ContainerLifecycleListener.super.beforeContainerStop(container);
    }

    //After The Class stops
    @Override
    public void afterContainerStop(TestResultContainer container) {
        ContainerLifecycleListener.super.afterContainerStop(container);
    }

    //Before The Class writes
    @Override
    public void beforeContainerWrite(TestResultContainer container) {
        ContainerLifecycleListener.super.beforeContainerWrite(container);
    }

    //After The Class writes
    @Override
    public void afterContainerWrite(TestResultContainer container) {
        ContainerLifecycleListener.super.afterContainerWrite(container);
    }

    //Before The Configuration 'SetUp' "and probably 'TearDown' too" starts
    @Override
    public void beforeFixtureStart(FixtureResult result) {
        FixtureLifecycleListener.super.beforeFixtureStart(result);
    }

    //After The Configuration 'SetUp' "and probably 'TearDown' too" starts
    @Override
    public void afterFixtureStart(FixtureResult result) {
        FixtureLifecycleListener.super.afterFixtureStart(result);
    }

    //Before The Configuration 'SetUp' "and probably 'TearDown' too" updates
    @Override
    public void beforeFixtureUpdate(FixtureResult result) {
        FixtureLifecycleListener.super.beforeFixtureUpdate(result);
    }

    //After The Configuration 'SetUp' "and probably 'TearDown' too" updates
    @Override
    public void afterFixtureUpdate(FixtureResult result) {
        FixtureLifecycleListener.super.afterFixtureUpdate(result);
    }

    //Before The Configuration 'SetUp' "and probably 'TearDown' too" stops
    @Override
    public void beforeFixtureStop(FixtureResult result) {
        TestNGListenerHelper.attachConfigurationMethods();
    }

    //After The Configuration 'SetUp' "and probably 'TearDown' too" stops
    @Override
    public void afterFixtureStop(FixtureResult result) {
        FixtureLifecycleListener.super.afterFixtureStop(result);
    }

    //Before The Configuration 'SetUp' starts
    @Override
    public void beforeTestSchedule(TestResult result) {
        TestLifecycleListener.super.beforeTestSchedule(result);
    }

    //After The Configuration 'SetUp' starts
    @Override
    public void afterTestSchedule(TestResult result) {
        TestLifecycleListener.super.afterTestSchedule(result);
    }

    //Before The @test updates
    @Override
    public void beforeTestUpdate(TestResult result) {
        TestLifecycleListener.super.beforeTestUpdate(result);
    }

    //After The @test updates
    @Override
    public void afterTestUpdate(TestResult result) {
        TestLifecycleListener.super.afterTestUpdate(result);
    }

    //Before The @test starts
    @Override
    public void beforeTestStart(TestResult result) {
        TestLifecycleListener.super.beforeTestStart(result);
    }

    //After The @test starts
    @Override
    public void afterTestStart(TestResult result) {
        TestLifecycleListener.super.afterTestStart(result);
    }

    //Before The @test stops
    @Override
    public void beforeTestStop(TestResult result) {
        TestLifecycleListener.super.beforeTestStop(result);
    }

    //After The @test stops
    @Override
    public void afterTestStop(TestResult result) {
        TestLifecycleListener.super.afterTestStop(result);
    }

    //Before The @test writes
    @Override
    public void beforeTestWrite(TestResult result) {
        TestLifecycleListener.super.beforeTestWrite(result);
    }

    //After The @test writes
    @Override
    public void afterTestWrite(TestResult result) {
        TestLifecycleListener.super.afterTestWrite(result);
    }
}
