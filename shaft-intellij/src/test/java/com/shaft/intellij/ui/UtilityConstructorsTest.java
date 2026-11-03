package com.shaft.intellij.ui;

import com.shaft.intellij.properties.ShaftPropertiesProfileCard;
import com.shaft.intellij.secrets.ShaftSecretsGuardrails;
import com.shaft.intellij.ui.presenter.ShaftAssistantPresenter;
import com.shaft.intellij.ui.presenter.ShaftMcpSetupPresenter;
import com.shaft.intellij.ui.theme.ShaftUiColors;
import com.shaft.intellij.ui.theme.ShaftUiMetrics;
import com.shaft.intellij.wizard.ShaftMavenProjectScaffold;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class UtilityConstructorsTest {
    @Test
    void utilityClassesRejectConstruction() throws Exception {
        assertUtility(ShaftUiMetrics.class);
        assertUtility(ShaftUiColors.class);
        assertUtility(ShaftStatusPresentation.class);
        assertUtility(ShaftMcpSetupPresenter.class);
        assertUtility(ShaftAssistantPresenter.class);
        assertUtility(AllureReportActions.class);
        assertUtility(ShaftSecretsGuardrails.class);
        assertUtility(ShaftMavenProjectScaffold.class);
        assertUtility(ShaftPropertiesProfileCard.class);
    }

    private static void assertUtility(Class<?> type) throws Exception {
        Constructor<?> constructor = type.getDeclaredConstructor();
        constructor.setAccessible(true); // NOPMD - test-only private utility constructor coverage
        InvocationTargetException thrown = assertThrows(InvocationTargetException.class, constructor::newInstance);
        assertTrue(thrown.getCause() instanceof IllegalStateException);
    }
}
