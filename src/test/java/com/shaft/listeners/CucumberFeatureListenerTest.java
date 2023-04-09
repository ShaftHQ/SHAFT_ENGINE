package com.shaft.listeners;

import io.cucumber.plugin.event.EventHandler;
import io.cucumber.plugin.event.EventPublisher;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertThrows;
import static org.utbot.runtime.utils.java.UtUtils.*;

public final class CucumberFeatureListenerTest {
    ///region Test suites for executable com.shaft.listeners.CucumberFeatureListener.getIsLastFinishedStepOK

    ///region

    @Test
    public void testGetIsLastFinishedStepOK1() throws ClassNotFoundException, IllegalAccessException, NoSuchFieldException {
        Class cucumberFeatureListenerClazz = Class.forName("com.shaft.listeners.CucumberFeatureListener");
        Boolean prevIsLastFinishedStepOK = ((Boolean) getStaticFieldValue(cucumberFeatureListenerClazz, "isLastFinishedStepOK"));
        try {
            setStaticField(cucumberFeatureListenerClazz, "isLastFinishedStepOK", null);

            Boolean actual = CucumberFeatureListener.getIsLastFinishedStepOK();

            assertNull(actual);
        } finally {
            setStaticField(CucumberFeatureListener.class, "isLastFinishedStepOK", prevIsLastFinishedStepOK);
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.listeners.CucumberFeatureListener.getLastStartedScenarioName

    ///region

    @Test
    public void testGetLastStartedScenarioName1() throws ClassNotFoundException, IllegalAccessException, NoSuchFieldException {
        Class cucumberFeatureListenerClazz = Class.forName("com.shaft.listeners.CucumberFeatureListener");
        String prevLastStartedScenarioName = ((String) getStaticFieldValue(cucumberFeatureListenerClazz, "lastStartedScenarioName"));
        try {
            setStaticField(cucumberFeatureListenerClazz, "lastStartedScenarioName", null);

            String actual = CucumberFeatureListener.getLastStartedScenarioName();

            assertNull(actual);
        } finally {
            setStaticField(CucumberFeatureListener.class, "lastStartedScenarioName", prevLastStartedScenarioName);
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.listeners.CucumberFeatureListener.setEventPublisher

    ///region

    @Test
    public void testSetEventPublisher1() throws Exception {
        CucumberFeatureListener cucumberFeatureListener = ((CucumberFeatureListener) createInstance("com.shaft.listeners.CucumberFeatureListener"));
        setField(cucumberFeatureListener, "com.shaft.listeners.CucumberFeatureListener", "featureStartedHandler", null);

        assertThrows(NullPointerException.class, () -> cucumberFeatureListener.setEventPublisher(null));

        EventHandler finalCucumberFeatureListenerFeatureStartedHandler = ((EventHandler) getFieldValue(cucumberFeatureListener, "com.shaft.listeners.CucumberFeatureListener", "featureStartedHandler"));

        assertNull(finalCucumberFeatureListenerFeatureStartedHandler);
    }

    @Test
    public void testSetEventPublisher2() throws Exception {
        CucumberFeatureListener cucumberFeatureListener = ((CucumberFeatureListener) createInstance("com.shaft.listeners.CucumberFeatureListener"));
        setField(cucumberFeatureListener, "com.shaft.listeners.CucumberFeatureListener", "featureStartedHandler", null);
        setField(cucumberFeatureListener, "com.shaft.listeners.CucumberFeatureListener", "featureFinishedHandler", null);
        setField(cucumberFeatureListener, "com.shaft.listeners.CucumberFeatureListener", "caseStartedHandler", null);
        setField(cucumberFeatureListener, "com.shaft.listeners.CucumberFeatureListener", "caseFinishedHandler", null);
        setField(cucumberFeatureListener, "com.shaft.listeners.CucumberFeatureListener", "stepStartedHandler", null);
        setField(cucumberFeatureListener, "com.shaft.listeners.CucumberFeatureListener", "stepFinishedHandler", null);
        EventHandler eventHandlerMock = mock(EventHandler.class);
        setField(cucumberFeatureListener, "com.shaft.listeners.CucumberFeatureListener", "writeEventHandler", eventHandlerMock);
        setField(cucumberFeatureListener, "com.shaft.listeners.CucumberFeatureListener", "embedEventHandler", null);
        EventPublisher publisherMock = mock(EventPublisher.class);

        cucumberFeatureListener.setEventPublisher(publisherMock);
    }
    ///endregion

    ///endregion
}
