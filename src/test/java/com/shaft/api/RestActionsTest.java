package com.shaft.api;

import com.shaft.api.RequestBuilder.AuthenticationType;
import com.shaft.api.RestActions.ComparisonType;
import com.shaft.api.RestActions.ParametersType;
import com.shaft.api.RestActions.RequestType;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.Timeouts;
import io.restassured.http.Cookies;
import io.restassured.internal.RestAssuredResponseImpl;
import io.restassured.path.json.JsonPath;
import io.restassured.path.xml.XmlPath;
import io.restassured.response.Response;
import io.restassured.response.ResponseBody;
import org.aeonbits.owner.ConfigFactory;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.testng.Assert.*;
import static org.utbot.runtime.utils.java.UtUtils.*;

public final class RestActionsTest {
    ///region Test suites for executable com.shaft.api.RestActions.sendRequest

    ///region
    ///endregion

    ///region OTHER: TIMEOUTS for method sendRequest(com.shaft.api.RestActions.RequestType, java.lang.String, io.restassured.specification.RequestSpecification)
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.prepareReportMessage

    ///region

    @Test
    public void testPrepareReportMessage1() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));

        String actual = restActions.prepareReportMessage(null, 128, null, null, null, null);

        String expected = "";
        assertEquals(expected, actual);
    }

    @Test
    public void testPrepareReportMessage2() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        Response responseMock = mock(Response.class);
        Cookies cookiesMock = mock(Cookies.class);
        (when(cookiesMock.size())).thenReturn(1);
        ArrayList arrayList = new ArrayList();
        Iterator iterator = arrayList.iterator();
        (when(cookiesMock.iterator())).thenReturn(iterator);
        (when(responseMock.getDetailedCookies())).thenReturn(cookiesMock, cookiesMock);
        RequestType requestType = RequestType.PUT;

        assertThrows(NullPointerException.class, () -> restActions.prepareReportMessage(responseMock, 0, requestType, null, null, null));
    }

    @Test
    public void testPrepareReportMessage3() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        Response responseMock = mock(Response.class);
        Cookies cookiesMock = mock(Cookies.class);
        (when(cookiesMock.size())).thenReturn(1);
        (when(responseMock.getDetailedCookies())).thenReturn(cookiesMock, ((Cookies) null));
        RequestType requestType = RequestType.PUT;

        assertThrows(NullPointerException.class, () -> restActions.prepareReportMessage(responseMock, 0, requestType, null, null, null));
    }

    @Test
    public void testPrepareReportMessage4() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        Response responseMock = mock(Response.class);
        Cookies cookiesMock = mock(Cookies.class);
        (when(cookiesMock.size())).thenReturn(-2147483647);
        (when(responseMock.getDetailedCookies())).thenReturn(cookiesMock);
        RequestType requestType = RequestType.PUT;

        assertThrows(NullPointerException.class, () -> restActions.prepareReportMessage(responseMock, 0, requestType, null, null, null));
    }

    @Test
    public void testPrepareReportMessage5() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        Response responseMock = mock(Response.class);
        (when(responseMock.getDetailedCookies())).thenReturn(((Cookies) null));
        RequestType requestType = RequestType.PUT;

        assertThrows(NullPointerException.class, () -> restActions.prepareReportMessage(responseMock, 0, requestType, null, null, null));
    }
    ///endregion

    ///region OTHER: ERROR SUITE for method prepareReportMessage(io.restassured.response.Response, int, com.shaft.api.RestActions.RequestType, java.lang.String, java.lang.String, java.lang.String)

    @Test
    public void testPrepareReportMessage21() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        Response responseMock = mock(Response.class);
        Cookies cookiesMock = mock(Cookies.class);
        (when(cookiesMock.size())).thenReturn(1);
        ArrayList arrayList = new ArrayList();
        Iterator iterator = arrayList.iterator();
        (when(cookiesMock.iterator())).thenReturn(iterator);
        (when(responseMock.getDetailedCookies())).thenReturn(cookiesMock, cookiesMock);
        RequestType requestType = RequestType.PUT;

        assertThrows(NullPointerException.class, () -> restActions.prepareReportMessage(responseMock, 0, requestType, null, null, null));
    }

    @Test
    public void testPrepareReportMessage31() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        Response responseMock = mock(Response.class);
        Cookies cookiesMock = mock(Cookies.class);
        (when(cookiesMock.size())).thenReturn(1);
        (when(responseMock.getDetailedCookies())).thenReturn(cookiesMock, ((Cookies) null));
        RequestType requestType = RequestType.PUT;

        assertThrows(NullPointerException.class, () -> restActions.prepareReportMessage(responseMock, 0, requestType, null, null, null));
    }

    @Test
    public void testPrepareReportMessage41() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        Response responseMock = mock(Response.class);
        Cookies cookiesMock = mock(Cookies.class);
        (when(cookiesMock.size())).thenReturn(-2147483647);
        (when(responseMock.getDetailedCookies())).thenReturn(cookiesMock);
        RequestType requestType = RequestType.PUT;

        assertThrows(NullPointerException.class, () -> restActions.prepareReportMessage(responseMock, 0, requestType, null, null, null));
    }

    @Test
    public void testPrepareReportMessage51() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        Response responseMock = mock(Response.class);
        (when(responseMock.getDetailedCookies())).thenReturn(((Cookies) null));
        RequestType requestType = RequestType.PUT;

        assertThrows(NullPointerException.class, () -> restActions.prepareReportMessage(responseMock, 0, requestType, null, null, null));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.prepareRequestSpecs

    ///region Errors report for prepareRequestSpecs

    public void testPrepareRequestSpecs_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 21 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.compareJSON

    ///region

    @Test
    public void testCompareJSON1() {
        assertThrows(NullPointerException.class, () -> RestActions.compareJSON(null, null, null, null));
    }

    @Test
    public void testCompareJSON2() {
        Response responseMock = mock(Response.class);
        String string = "";
        ComparisonType comparisonType = ComparisonType.CONTAINS;
        String string1 = "";

        assertThrows(NullPointerException.class, () -> RestActions.compareJSON(responseMock, string, comparisonType, string1));
    }
    ///endregion

    ///region OTHER: ERROR SUITE for method compareJSON(io.restassured.response.Response, java.lang.String, com.shaft.api.RestActions.ComparisonType, java.lang.String)

    @Test
    public void testCompareJSON21() {
        Response responseMock = mock(Response.class);
        String string = "";
        ComparisonType comparisonType = ComparisonType.CONTAINS;
        String string1 = "";

        assertThrows(NullPointerException.class, () -> RestActions.compareJSON(responseMock, string, comparisonType, string1));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.buildNewRequest

    ///region

    @Test
    public void testBuildNewRequest1() throws Exception {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            mockedConstruction = mockConstruction(RequestBuilder.class, (RequestBuilder requestBuilderMock, org.mockito.MockedConstruction.Context context) -> {
            });
            RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));

            RequestBuilder actual = restActions.buildNewRequest(null, null);

            RequestBuilder expectedMock = mock(RequestBuilder.class);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "session", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "sessionHeaders", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "sessionCookies", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "sessionConfigs", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "requestType", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "serviceName", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "serviceURI", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "targetStatusCode", 0);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "urlArguments", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "parameters", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "parametersType", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "requestBody", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "contentType", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "authenticationType", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "authenticationUsername", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "authenticationPassword", null);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "appendDefaultContentCharsetToContentTypeIfUndefined", false);
            setField(expectedMock, "com.shaft.api.RequestBuilder", "urlEncodingEnabled", false);
            RestActions actualSession = ((RestActions) getFieldValue(actual, "com.shaft.api.RequestBuilder", "session"));
            assertNull(actualSession);

            Map actualSessionHeaders = ((Map) getFieldValue(actual, "com.shaft.api.RequestBuilder", "sessionHeaders"));
            assertNull(actualSessionHeaders);

            Map actualSessionCookies = ((Map) getFieldValue(actual, "com.shaft.api.RequestBuilder", "sessionCookies"));
            assertNull(actualSessionCookies);

            List actualSessionConfigs = ((List) getFieldValue(actual, "com.shaft.api.RequestBuilder", "sessionConfigs"));
            assertNull(actualSessionConfigs);

            RequestType actualRequestType = ((RequestType) getFieldValue(actual, "com.shaft.api.RequestBuilder", "requestType"));
            assertNull(actualRequestType);

            String actualServiceName = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "serviceName"));
            assertNull(actualServiceName);

            String actualServiceURI = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "serviceURI"));
            assertNull(actualServiceURI);

            int expectedMockTargetStatusCode = ((Integer) getFieldValue(expectedMock, "com.shaft.api.RequestBuilder", "targetStatusCode"));
            int actualTargetStatusCode = ((Integer) getFieldValue(actual, "com.shaft.api.RequestBuilder", "targetStatusCode"));
            assertEquals(expectedMockTargetStatusCode, actualTargetStatusCode);

            String actualUrlArguments = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "urlArguments"));
            assertNull(actualUrlArguments);

            List actualParameters = ((List) getFieldValue(actual, "com.shaft.api.RequestBuilder", "parameters"));
            assertNull(actualParameters);

            ParametersType actualParametersType = ((ParametersType) getFieldValue(actual, "com.shaft.api.RequestBuilder", "parametersType"));
            assertNull(actualParametersType);

            Object actualRequestBody = getFieldValue(actual, "com.shaft.api.RequestBuilder", "requestBody");
            assertNull(actualRequestBody);

            String actualContentType = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "contentType"));
            assertNull(actualContentType);

            AuthenticationType actualAuthenticationType = ((AuthenticationType) getFieldValue(actual, "com.shaft.api.RequestBuilder", "authenticationType"));
            assertNull(actualAuthenticationType);

            String actualAuthenticationUsername = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "authenticationUsername"));
            assertNull(actualAuthenticationUsername);

            String actualAuthenticationPassword = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "authenticationPassword"));
            assertNull(actualAuthenticationPassword);

            boolean actualAppendDefaultContentCharsetToContentTypeIfUndefined = ((Boolean) getFieldValue(actual, "com.shaft.api.RequestBuilder", "appendDefaultContentCharsetToContentTypeIfUndefined"));
            assertFalse(actualAppendDefaultContentCharsetToContentTypeIfUndefined);

            boolean actualUrlEncodingEnabled = ((Boolean) getFieldValue(actual, "com.shaft.api.RequestBuilder", "urlEncodingEnabled"));
            assertFalse(actualUrlEncodingEnabled);

        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///region Errors report for buildNewRequest

    public void testBuildNewRequest_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 19 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.buildNewRequest

    ///region

    @Test
    public void testBuildNewRequest2() throws ClassNotFoundException, IllegalAccessException, NoSuchFieldException, InvocationTargetException, NoSuchMethodException {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            Timeouts prevTimeouts = com.shaft.properties.internal.Properties.timeouts;
            try {
                Properties.timeouts = ConfigFactory.create(Timeouts.class);
                mockedConstruction = mockConstruction(RequestBuilder.class, (RequestBuilder requestBuilderMock, org.mockito.MockedConstruction.Context context) -> {
                });

                RequestBuilder actual = RestActions.buildNewRequest(null, null, null);

                RequestBuilder expectedMock = mock(RequestBuilder.class);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "session", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "sessionHeaders", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "sessionCookies", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "sessionConfigs", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "requestType", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "serviceName", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "serviceURI", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "targetStatusCode", 0);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "urlArguments", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "parameters", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "parametersType", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "requestBody", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "contentType", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "authenticationType", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "authenticationUsername", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "authenticationPassword", null);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "appendDefaultContentCharsetToContentTypeIfUndefined", false);
                setField(expectedMock, "com.shaft.api.RequestBuilder", "urlEncodingEnabled", false);
                RestActions actualSession = ((RestActions) getFieldValue(actual, "com.shaft.api.RequestBuilder", "session"));
                assertNull(actualSession);

                Map actualSessionHeaders = ((Map) getFieldValue(actual, "com.shaft.api.RequestBuilder", "sessionHeaders"));
                assertNull(actualSessionHeaders);

                Map actualSessionCookies = ((Map) getFieldValue(actual, "com.shaft.api.RequestBuilder", "sessionCookies"));
                assertNull(actualSessionCookies);

                List actualSessionConfigs = ((List) getFieldValue(actual, "com.shaft.api.RequestBuilder", "sessionConfigs"));
                assertNull(actualSessionConfigs);

                RequestType actualRequestType = ((RequestType) getFieldValue(actual, "com.shaft.api.RequestBuilder", "requestType"));
                assertNull(actualRequestType);

                String actualServiceName = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "serviceName"));
                assertNull(actualServiceName);

                String actualServiceURI = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "serviceURI"));
                assertNull(actualServiceURI);

                int expectedMockTargetStatusCode = ((Integer) getFieldValue(expectedMock, "com.shaft.api.RequestBuilder", "targetStatusCode"));
                int actualTargetStatusCode = ((Integer) getFieldValue(actual, "com.shaft.api.RequestBuilder", "targetStatusCode"));
                assertEquals(expectedMockTargetStatusCode, actualTargetStatusCode);

                String actualUrlArguments = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "urlArguments"));
                assertNull(actualUrlArguments);

                List actualParameters = ((List) getFieldValue(actual, "com.shaft.api.RequestBuilder", "parameters"));
                assertNull(actualParameters);

                ParametersType actualParametersType = ((ParametersType) getFieldValue(actual, "com.shaft.api.RequestBuilder", "parametersType"));
                assertNull(actualParametersType);

                Object actualRequestBody = getFieldValue(actual, "com.shaft.api.RequestBuilder", "requestBody");
                assertNull(actualRequestBody);

                String actualContentType = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "contentType"));
                assertNull(actualContentType);

                AuthenticationType actualAuthenticationType = ((AuthenticationType) getFieldValue(actual, "com.shaft.api.RequestBuilder", "authenticationType"));
                assertNull(actualAuthenticationType);

                String actualAuthenticationUsername = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "authenticationUsername"));
                assertNull(actualAuthenticationUsername);

                String actualAuthenticationPassword = ((String) getFieldValue(actual, "com.shaft.api.RequestBuilder", "authenticationPassword"));
                assertNull(actualAuthenticationPassword);

                boolean actualAppendDefaultContentCharsetToContentTypeIfUndefined = ((Boolean) getFieldValue(actual, "com.shaft.api.RequestBuilder", "appendDefaultContentCharsetToContentTypeIfUndefined"));
                assertFalse(actualAppendDefaultContentCharsetToContentTypeIfUndefined);

                boolean actualUrlEncodingEnabled = ((Boolean) getFieldValue(actual, "com.shaft.api.RequestBuilder", "urlEncodingEnabled"));
                assertFalse(actualUrlEncodingEnabled);

            } finally {
                com.shaft.properties.internal.Properties.timeouts = prevTimeouts;
            }
        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.addConfigVariable

    ///region

    @Test
    public void testAddConfigVariable1() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        setField(restActions, "com.shaft.api.RestActions", "sessionConfigs", null);

        assertThrows(NullPointerException.class, () -> restActions.addConfigVariable(null));

        List finalRestActionsSessionConfigs = ((List) getFieldValue(restActions, "com.shaft.api.RestActions", "sessionConfigs"));

        assertNull(finalRestActionsSessionConfigs);
    }
    ///endregion

    ///region Errors report for addConfigVariable

    public void testAddConfigVariable_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 19 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.addCookieVariable

    ///region

    @Test
    public void testAddCookieVariable1() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        setField(restActions, "com.shaft.api.RestActions", "sessionCookies", null);

        assertThrows(NullPointerException.class, () -> restActions.addCookieVariable(null, null));

        Map finalRestActionsSessionCookies = ((Map) getFieldValue(restActions, "com.shaft.api.RestActions", "sessionCookies"));

        assertNull(finalRestActionsSessionCookies);
    }
    ///endregion

    ///region Errors report for addCookieVariable

    public void testAddCookieVariable_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 19 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.addHeaderVariable

    ///region

    @Test
    public void testAddHeaderVariable1() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        setField(restActions, "com.shaft.api.RestActions", "sessionHeaders", null);

        assertThrows(NullPointerException.class, () -> restActions.addHeaderVariable(null, null));

        Map finalRestActionsSessionHeaders = ((Map) getFieldValue(restActions, "com.shaft.api.RestActions", "sessionHeaders"));

        assertNull(finalRestActionsSessionHeaders);
    }
    ///endregion

    ///region Errors report for addHeaderVariable

    public void testAddHeaderVariable_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 15 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.evaluateResponseStatusCode

    ///region

    @Test
    public void testEvaluateResponseStatusCode1() throws Exception {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.tools.io.internal.ReportManagerHelper.class);
            (mockedStatic.when(com.shaft.tools.io.internal.ReportManagerHelper::getDiscreteLogging)).thenReturn(false);
            RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));

            assertThrows(NullPointerException.class, () -> restActions.evaluateResponseStatusCode(null, -255));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for evaluateResponseStatusCode

    public void testEvaluateResponseStatusCode_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 1 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.failAction

    ///region

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#failAction(String, Throwable[])}
     */
    @Test(description = "failAction: arg_0 = '', rootCauseException = Throwable[0] -> throw NoClassDefFoundError")
    public void testFailActionThrowsNCDFEWithEmptyStringAndEmptyObjectArray() {
        Throwable[] rootCauseException = {};

        assertThrows(ArrayIndexOutOfBoundsException.class, () -> RestActions.failAction("", rootCauseException));
    }

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#failAction(String, Throwable[])}
     */
    @Test(description = "failAction: arg_0 = '<', rootCauseException = Throwable[0] -> throw NoClassDefFoundError")
    public void testFailActionThrowsNCDFEWithNonEmptyStringAndEmptyObjectArray() {
        Throwable[] rootCauseException = {};

        assertThrows(ArrayIndexOutOfBoundsException.class, () -> RestActions.failAction("<", rootCauseException));
    }

    @Test
    public void testFailAction1() {
        Throwable[] rootCauseException = {null, null, null, null, null, null, null, null, null, null};

        assertThrows(NullPointerException.class, () -> RestActions.failAction(null, rootCauseException));

        Throwable finalRootCauseException0 = rootCauseException[0];
        Throwable finalRootCauseException1 = rootCauseException[1];
        Throwable finalRootCauseException2 = rootCauseException[2];
        Throwable finalRootCauseException3 = rootCauseException[3];
        Throwable finalRootCauseException4 = rootCauseException[4];
        Throwable finalRootCauseException5 = rootCauseException[5];
        Throwable finalRootCauseException6 = rootCauseException[6];
        Throwable finalRootCauseException7 = rootCauseException[7];
        Throwable finalRootCauseException8 = rootCauseException[8];
        Throwable finalRootCauseException9 = rootCauseException[9];

        assertNull(finalRootCauseException0);

        assertNull(finalRootCauseException1);

        assertNull(finalRootCauseException2);

        assertNull(finalRootCauseException3);

        assertNull(finalRootCauseException4);

        assertNull(finalRootCauseException5);

        assertNull(finalRootCauseException6);

        assertNull(finalRootCauseException7);

        assertNull(finalRootCauseException8);

        assertNull(finalRootCauseException9);
    }
    ///endregion

    ///region FUZZER: ERROR SUITE for method failAction(java.lang.String, java.lang.Throwable[])

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#failAction(String, Throwable[])}
     */
    @Test(description = "failAction: failAction: arg_0 = '', rootCauseException = Throwable[0] -> throw NoClassDefFoundError")
    public void testFailActionThrowsNCDFEWithEmptyStringAndEmptyObjectArray1() {
        Throwable[] rootCauseException = {};

        assertThrows(ArrayIndexOutOfBoundsException.class, () -> RestActions.failAction("", rootCauseException));
    }

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#failAction(String, Throwable[])}
     */
    @Test(description = "failAction: failAction: arg_0 = '<', rootCauseException = Throwable[0] -> throw NoClassDefFoundError")
    public void testFailActionThrowsNCDFEWithNonEmptyStringAndEmptyObjectArray1() {
        Throwable[] rootCauseException = {};

        assertThrows(ArrayIndexOutOfBoundsException.class, () -> RestActions.failAction("<", rootCauseException));
    }
    ///endregion

    ///region OTHER: ERROR SUITE for method failAction(java.lang.String, java.lang.Throwable[])

    @Test
    public void testFailAction11() {
        Throwable[] rootCauseException = {null, null, null, null, null, null, null, null, null, null};

        assertThrows(NullPointerException.class, () -> RestActions.failAction(null, rootCauseException));

        Throwable finalRootCauseException0 = rootCauseException[0];
        Throwable finalRootCauseException1 = rootCauseException[1];
        Throwable finalRootCauseException2 = rootCauseException[2];
        Throwable finalRootCauseException3 = rootCauseException[3];
        Throwable finalRootCauseException4 = rootCauseException[4];
        Throwable finalRootCauseException5 = rootCauseException[5];
        Throwable finalRootCauseException6 = rootCauseException[6];
        Throwable finalRootCauseException7 = rootCauseException[7];
        Throwable finalRootCauseException8 = rootCauseException[8];
        Throwable finalRootCauseException9 = rootCauseException[9];

        assertNull(finalRootCauseException0);

        assertNull(finalRootCauseException1);

        assertNull(finalRootCauseException2);

        assertNull(finalRootCauseException3);

        assertNull(finalRootCauseException4);

        assertNull(finalRootCauseException5);

        assertNull(finalRootCauseException6);

        assertNull(finalRootCauseException7);

        assertNull(finalRootCauseException8);

        assertNull(finalRootCauseException9);
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.formatXML

    ///region

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#formatXML(String)}
     */
    @Test(description = "formatXML: arg_0 = ''")
    public void testFormatXMLWithEmptyString() {
        String actual = RestActions.formatXML("");

        String expected = "";
        assertEquals(expected, actual);
    }
    ///endregion

    ///region FUZZER: SUCCESSFUL EXECUTIONS for method formatXML(java.lang.String)

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#formatXML(String)}
     */
    @Test(description = "formatXML: formatXML: arg_0 = ''")
    public void testFormatXMLWithEmptyString1() {
        String actual = RestActions.formatXML("");

        String expected = "";
        assertEquals(expected, actual);
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getResponseBody

    ///region

    @Test
    public void testGetResponseBody1() {
        Response responseMock = mock(Response.class);
        (when(responseMock.getBody())).thenReturn(((ResponseBody) null));

        assertThrows(NullPointerException.class, () -> RestActions.getResponseBody(responseMock));
    }

    @Test
    public void testGetResponseBody2() {
        assertThrows(NullPointerException.class, () -> RestActions.getResponseBody(null));
    }

    @Test
    public void testGetResponseBody3() {
        Response responseMock = mock(Response.class);
        ResponseBody responseBodyMock = mock(ResponseBody.class);
        (when(responseBodyMock.asString())).thenReturn(((String) null));
        (when(responseMock.getBody())).thenReturn(responseBodyMock);

        String actual = RestActions.getResponseBody(responseMock);

        assertNull(actual);
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getResponseJSONValue

    ///region

    @Test
    public void testGetResponseJSONValue1() throws Exception {
        RestAssuredResponseImpl response = ((RestAssuredResponseImpl) createInstance("io.restassured.internal.RestAssuredResponseImpl"));

        assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValue(((Object) response), ((String) null)));
    }

    @Test
    public void testGetResponseJSONValue2() throws Exception {
        RestAssuredResponseImpl response = ((RestAssuredResponseImpl) createInstance("io.restassured.internal.RestAssuredResponseImpl"));
        String jsonPath = "";

        assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValue(((Object) response), jsonPath));
    }

    @Test
    public void testGetResponseJSONValue3() {
        org.mockito.MockedConstruction mockedConstruction = null;
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedConstruction = mockConstruction(org.json.JSONObject.class, (org.json.JSONObject jSONObjectMock, org.mockito.MockedConstruction.Context context) -> (when(jSONObjectMock.toString())).thenReturn(null));
            mockedStatic = mockStatic(JsonPath.class);
            (mockedStatic.when(() -> JsonPath.from(any(String.class)))).thenReturn(((JsonPath) null));
            HashMap response = new HashMap();

            assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValue(response, ((String) null)));
        } finally {
            mockedConstruction.close();
            mockedStatic.close();
        }
    }

    @Test
    public void testGetResponseJSONValue4() {
        org.mockito.MockedConstruction mockedConstruction = null;
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedConstruction = mockConstruction(org.json.JSONObject.class, (org.json.JSONObject jSONObjectMock, org.mockito.MockedConstruction.Context context) -> (when(jSONObjectMock.toString())).thenReturn(null));
            mockedStatic = mockStatic(JsonPath.class);
            JsonPath jsonPathMock = mock(JsonPath.class);
            String string = "";
            (when(jsonPathMock.getString(any()))).thenReturn(string);
            (mockedStatic.when(() -> JsonPath.from(any(String.class)))).thenReturn(jsonPathMock);
            HashMap response = new HashMap();

            assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValue(response, ((String) null)));
        } finally {
            mockedConstruction.close();
            mockedStatic.close();
        }
    }
    ///endregion

    ///region FUZZER: ERROR SUITE for method getResponseJSONValue(java.lang.Object, java.lang.String)

    ///endregion

    ///region OTHER: ERROR SUITE for method getResponseJSONValue(java.lang.Object, java.lang.String)

    @Test
    public void testGetResponseJSONValue21() throws Exception {
        RestAssuredResponseImpl response = ((RestAssuredResponseImpl) createInstance("io.restassured.internal.RestAssuredResponseImpl"));
        String jsonPath = "";

        assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValue(((Object) response), jsonPath));
    }

    @Test
    public void testGetResponseJSONValue41() {
        org.mockito.MockedConstruction mockedConstruction = null;
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedConstruction = mockConstruction(org.json.JSONObject.class, (org.json.JSONObject jSONObjectMock, org.mockito.MockedConstruction.Context context) -> (when(jSONObjectMock.toString())).thenReturn(null));
            mockedStatic = mockStatic(JsonPath.class);
            JsonPath jsonPathMock = mock(JsonPath.class);
            String string = "";
            (when(jsonPathMock.getString(any()))).thenReturn(string);
            (mockedStatic.when(() -> JsonPath.from(any(String.class)))).thenReturn(jsonPathMock);
            HashMap response = new HashMap();

            assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValue(response, ((String) null)));
        } finally {
            mockedConstruction.close();
            mockedStatic.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getResponseJSONValue

    ///region

    @Test
    public void testGetResponseJSONValue5() {
        assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValue(((Response) null), ((String) null)));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getResponseJSONValueAsList

    ///region

    @Test
    public void testGetResponseJSONValueAsList1() {
        assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValueAsList(null, null));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getResponseJSONValueFromList

    ///region

    @Test
    public void testGetResponseJSONValueFromList1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.jayway.jsonpath.JsonPath.class);
            (mockedStatic.when(() -> com.jayway.jsonpath.JsonPath.read(any(String.class), any(String.class), any(com.jayway.jsonpath.Predicate[].class)))).thenReturn(null);
            Response responseMock = mock(Response.class);
            String string = "";
            (when(responseMock.asPrettyString())).thenReturn(string);
            String jsonPathToList = "";
            String jsonPathToValueNeeded = "";

            assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValueFromList(responseMock, jsonPathToList, jsonPathToValueNeeded, string, string));
        } finally {
            mockedStatic.close();
        }
    }

    @Test
    public void testGetResponseJSONValueFromList2() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.jayway.jsonpath.JsonPath.class);
            Object object = new Object();
            (mockedStatic.when(() -> com.jayway.jsonpath.JsonPath.read(any(String.class), any(String.class), any(com.jayway.jsonpath.Predicate[].class)))).thenReturn(object);
            Response responseMock = mock(Response.class);
            (when(responseMock.asPrettyString())).thenReturn(((String) null));

            assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValueFromList(responseMock, null, null, null, null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region OTHER: ERROR SUITE for method getResponseJSONValueFromList(io.restassured.response.Response, java.lang.String, java.lang.String, java.lang.String, java.lang.String)

    @Test
    public void testGetResponseJSONValueFromList11() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.jayway.jsonpath.JsonPath.class);
            (mockedStatic.when(() -> com.jayway.jsonpath.JsonPath.read(any(String.class), any(String.class), any(com.jayway.jsonpath.Predicate[].class)))).thenReturn(null);
            Response responseMock = mock(Response.class);
            String string = "";
            (when(responseMock.asPrettyString())).thenReturn(string);
            String jsonPathToList = "";
            String jsonPathToValueNeeded = "";

            assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValueFromList(responseMock, jsonPathToList, jsonPathToValueNeeded, string, string));
        } finally {
            mockedStatic.close();
        }
    }

    @Test
    public void testGetResponseJSONValueFromList21() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.jayway.jsonpath.JsonPath.class);
            Object object = new Object();
            (mockedStatic.when(() -> com.jayway.jsonpath.JsonPath.read(any(String.class), any(String.class), any(com.jayway.jsonpath.Predicate[].class)))).thenReturn(object);
            Response responseMock = mock(Response.class);
            (when(responseMock.asPrettyString())).thenReturn(((String) null));

            assertThrows(NullPointerException.class, () -> RestActions.getResponseJSONValueFromList(responseMock, null, null, null, null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getResponseStatusCode

    ///region

    @Test
    public void testGetResponseStatusCode1() {
        assertThrows(NullPointerException.class, () -> RestActions.getResponseStatusCode(null));
    }
    ///endregion

    ///region OTHER: ERROR SUITE for method getResponseStatusCode(io.restassured.response.Response)
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getResponseTime

    ///region

    @Test
    public void testGetResponseTime1() {
        assertThrows(NullPointerException.class, () -> RestActions.getResponseTime(null));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getResponseXMLValue

    ///region

    @Test
    public void testGetResponseXMLValue1() {
        Object response = new Object();

        assertThrows(AssertionError.class, () -> RestActions.getResponseXMLValue(response, ((String) null)));
    }

    @Test
    public void testGetResponseXMLValue2() {
        assertThrows(NullPointerException.class, () -> RestActions.getResponseXMLValue(((Object) null), ((String) null)));
    }
    ///endregion

    ///region OTHER: ERROR SUITE for method getResponseXMLValue(java.lang.Object, java.lang.String)

    @Test
    public void testGetResponseXMLValue11() {
        Object response = new Object();

        assertThrows(AssertionError.class, () -> RestActions.getResponseXMLValue(response, ((String) null)));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getResponseXMLValue

    ///region

    @Test
    public void testGetResponseXMLValue3() {
        assertThrows(NullPointerException.class, () -> RestActions.getResponseXMLValue(((Response) null), ((String) null)));
    }

    @Test
    public void testGetResponseXMLValue4() {
        Response responseMock = mock(Response.class);
        (when(responseMock.xmlPath())).thenReturn(((XmlPath) null));

        assertThrows(NullPointerException.class, () -> RestActions.getResponseXMLValue(responseMock, ((String) null)));
    }

    ///endregion

    ///region OTHER: ERROR SUITE for method getResponseXMLValue(io.restassured.response.Response, java.lang.String)

    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getResponseXMLValueAsList

    ///region

    @Test
    public void testGetResponseXMLValueAsList1() {
        Response responseMock = mock(Response.class);
        (when(responseMock.xmlPath())).thenReturn(((XmlPath) null));

        assertThrows(NullPointerException.class, () -> RestActions.getResponseXMLValueAsList(responseMock, null));
    }

    @Test
    public void testGetResponseXMLValueAsList2() {
        assertThrows(NullPointerException.class, () -> RestActions.getResponseXMLValueAsList(null, null));
    }

    @Test
    public void testGetResponseXMLValueAsList3() {
        Response responseMock = mock(Response.class);
        XmlPath xmlPathMock = mock(XmlPath.class);
        Object object = new Object();
        (when(xmlPathMock.get(any()))).thenReturn(object);
        (when(responseMock.xmlPath())).thenReturn(xmlPathMock);

        assertThrows(AssertionError.class, () -> RestActions.getResponseXMLValueAsList(responseMock, null));
    }
    ///endregion

    ///region OTHER: ERROR SUITE for method getResponseXMLValueAsList(io.restassured.response.Response, java.lang.String)

    @Test
    public void testGetResponseXMLValueAsList31() {
        Response responseMock = mock(Response.class);
        XmlPath xmlPathMock = mock(XmlPath.class);
        Object object = new Object();
        (when(xmlPathMock.get(any()))).thenReturn(object);
        (when(responseMock.xmlPath())).thenReturn(xmlPathMock);

        assertThrows(AssertionError.class, () -> RestActions.getResponseXMLValueAsList(responseMock, null));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getServiceURI

    ///region

    @Test
    public void testGetServiceURI1() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        setField(restActions, "com.shaft.api.RestActions", "serviceURI", null);

        String actual = restActions.getServiceURI();

        assertNull(actual);
    }
    ///endregion

    ///region Errors report for getServiceURI

    public void testGetServiceURI_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 15 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getSessionConfigs

    ///region

    @Test
    public void testGetSessionConfigs1() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        setField(restActions, "com.shaft.api.RestActions", "sessionConfigs", null);

        List actual = restActions.getSessionConfigs();

        assertNull(actual);

        List finalRestActionsSessionConfigs = ((List) getFieldValue(restActions, "com.shaft.api.RestActions", "sessionConfigs"));

        assertNull(finalRestActionsSessionConfigs);
    }
    ///endregion

    ///region Errors report for getSessionConfigs

    public void testGetSessionConfigs_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 15 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getSessionCookies

    ///region

    @Test
    public void testGetSessionCookies1() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        setField(restActions, "com.shaft.api.RestActions", "sessionCookies", null);

        Map actual = restActions.getSessionCookies();

        assertNull(actual);

        Map finalRestActionsSessionCookies = ((Map) getFieldValue(restActions, "com.shaft.api.RestActions", "sessionCookies"));

        assertNull(finalRestActionsSessionCookies);
    }
    ///endregion

    ///region Errors report for getSessionCookies

    public void testGetSessionCookies_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 15 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.getSessionHeaders

    ///region

    @Test
    public void testGetSessionHeaders1() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        setField(restActions, "com.shaft.api.RestActions", "sessionHeaders", null);

        Map actual = restActions.getSessionHeaders();

        assertNull(actual);

        Map finalRestActionsSessionHeaders = ((Map) getFieldValue(restActions, "com.shaft.api.RestActions", "sessionHeaders"));

        assertNull(finalRestActionsSessionHeaders);
    }
    ///endregion

    ///region Errors report for getSessionHeaders

    public void testGetSessionHeaders_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 15 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.parseBodyToJson

    ///region

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#parseBodyToJson(Object)}
     */
    @Test(description = "parseBodyToJson: arg_0 = Object()")
    public void testParseBodyToJson() {
        Object body = new Object();

        ByteArrayInputStream actual = ((ByteArrayInputStream) RestActions.parseBodyToJson(body));

        ByteArrayInputStream expected = new ByteArrayInputStream(new byte[0]);
    }

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#parseBodyToJson(Object)}
     */
    @Test(description = "parseBodyToJson: arg_0 = Object()")
    public void testParseBodyToJson1() {
        Object body = new Object();

        ByteArrayInputStream actual = ((ByteArrayInputStream) RestActions.parseBodyToJson(body));

        ByteArrayInputStream expected = new ByteArrayInputStream(new byte[0]);
    }

    @Test
    public void testParseBodyToJson2() {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            mockedConstruction = mockConstruction(org.json.simple.parser.JSONParser.class, (org.json.simple.parser.JSONParser jSONParserMock, org.mockito.MockedConstruction.Context context) -> {
            });
            Object body = new Object();

            ByteArrayInputStream actual = ((ByteArrayInputStream) RestActions.parseBodyToJson(body));

            ByteArrayInputStream expected = new ByteArrayInputStream(new byte[0]);
        } finally {
            mockedConstruction.close();
        }
    }

    @Test
    public void testParseBodyToJson3() {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            mockedConstruction = mockConstruction(org.json.simple.parser.JSONParser.class, (org.json.simple.parser.JSONParser jSONParserMock, org.mockito.MockedConstruction.Context context) -> {
            });

            ByteArrayInputStream actual = ((ByteArrayInputStream) RestActions.parseBodyToJson(((Object) null)));

            ByteArrayInputStream expected = new ByteArrayInputStream(new byte[0]);
        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///region FUZZER: SUCCESSFUL EXECUTIONS for method parseBodyToJson(java.lang.Object)

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#parseBodyToJson(Object)}
     */
    @Test(description = "parseBodyToJson: parseBodyToJson: arg_0 = Object()")
    public void testParseBodyToJson4() {
        Object body = new Object();

        ByteArrayInputStream actual = ((ByteArrayInputStream) RestActions.parseBodyToJson(body));

        ByteArrayInputStream expected = new ByteArrayInputStream(new byte[0]);
    }

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#parseBodyToJson(Object)}
     */
    @Test(description = "parseBodyToJson: parseBodyToJson: arg_0 = Object()")
    public void testParseBodyToJson5() {
        Object body = new Object();

        ByteArrayInputStream actual = ((ByteArrayInputStream) RestActions.parseBodyToJson(body));

        ByteArrayInputStream expected = new ByteArrayInputStream(new byte[0]);
    }
    ///endregion

    ///region OTHER: SUCCESSFUL EXECUTIONS for method parseBodyToJson(java.lang.Object)

    @Test
    public void testParseBodyToJson21() {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            mockedConstruction = mockConstruction(org.json.simple.parser.JSONParser.class, (org.json.simple.parser.JSONParser jSONParserMock, org.mockito.MockedConstruction.Context context) -> {
            });
            Object body = new Object();

            ByteArrayInputStream actual = ((ByteArrayInputStream) RestActions.parseBodyToJson(body));

            ByteArrayInputStream expected = new ByteArrayInputStream(new byte[0]);
        } finally {
            mockedConstruction.close();
        }
    }

    @Test
    public void testParseBodyToJson31() {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            mockedConstruction = mockConstruction(org.json.simple.parser.JSONParser.class, (org.json.simple.parser.JSONParser jSONParserMock, org.mockito.MockedConstruction.Context context) -> {
            });

            ByteArrayInputStream actual = ((ByteArrayInputStream) RestActions.parseBodyToJson(((Object) null)));

            ByteArrayInputStream expected = new ByteArrayInputStream(new byte[0]);
        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.parseBodyToJson

    ///region

    @Test
    public void testParseBodyToJson6() {
        assertThrows(NullPointerException.class, () -> RestActions.parseBodyToJson(((Response) null)));
    }
    ///endregion

    ///region OTHER: ERROR SUITE for method parseBodyToJson(io.restassured.response.Response)
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.passAction

    ///region

    ///region FUZZER: ERROR SUITE for method passAction(java.lang.String)

    ///region OTHER: ERROR SUITE for method passAction(java.lang.String)
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.passAction

    ///region
    ///region FUZZER: ERROR SUITE for method passAction(java.lang.String, java.util.List)
    ///endregion

    ///region OTHER: ERROR SUITE for method passAction(java.lang.String, java.util.List)
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.prepareRequestURL

    ///region

    @Test
    public void testPrepareRequestURL1() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));

        String actual = restActions.prepareRequestURL(null, null, null);

        String expected = "nullnull";
        assertEquals(expected, actual);
    }

    @Test
    public void testPrepareRequestURL2() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        String urlArguments = "";

        String actual = restActions.prepareRequestURL(null, urlArguments, null);

        String expected = "nullnull";
        assertEquals(expected, actual);
    }
    ///endregion

    ///region OTHER: SUCCESSFUL EXECUTIONS for method prepareRequestURL(java.lang.String, java.lang.String, java.lang.String)

    @Test
    public void testPrepareRequestURL11() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));

        String actual = restActions.prepareRequestURL(null, null, null);

        String expected = "nullnull";
        assertEquals(expected, actual);
    }

    @Test
    public void testPrepareRequestURL21() throws Exception {
        RestActions restActions = ((RestActions) createInstance("com.shaft.api.RestActions"));
        String urlArguments = "";

        String actual = restActions.prepareRequestURL(null, urlArguments, null);

        String expected = "nullnull";
        assertEquals(expected, actual);
    }
    ///endregion

    ///region Errors report for prepareRequestURL

    public void testPrepareRequestURL_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 13 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.sendGraphQlRequest

    ///region

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#sendGraphQlRequest(String, String)}
     */
    @Test(description = "sendGraphQlRequest: arg_0 = 'abc', query = '\n\u000B\t\r' -> throw NoClassDefFoundError")
    public void testSendGraphQlRequestThrowsNCDFEWithNonEmptyStringAndBlankString() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequest("abc", "\n\u000B\t\r"));
    }
    ///endregion

    ///region FUZZER: ERROR SUITE for method sendGraphQlRequest(java.lang.String, java.lang.String)

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#sendGraphQlRequest(String, String)}
     */
    @Test(description = "sendGraphQlRequest: sendGraphQlRequest: arg_0 = 'abc', query = '\n\u000B\t\r' -> throw NoClassDefFoundError")
    public void testSendGraphQlRequestThrowsNCDFEWithNonEmptyStringAndBlankString1() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequest("abc", "\n\u000B\t\r"));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.sendGraphQlRequest

    ///region

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#sendGraphQlRequest(String, String, String)}
     */
    @Test(description = "sendGraphQlRequest: arg_0 = '\n\t\r', query = 'variables', variables = '\n\t\r' -> throw NoClassDefFoundError")
    public void testSendGraphQlRequestThrowsNCDFEWithBlankStringsAndNonEmptyString() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequest("\n\t\r", "variables", "\n\t\r"));
    }
    ///endregion

    ///region FUZZER: ERROR SUITE for method sendGraphQlRequest(java.lang.String, java.lang.String, java.lang.String)

    /**
     * @utbot.classUnderTest {@link RestActions}
     * @utbot.methodUnderTest {@link RestActions#sendGraphQlRequest(String, String, String)}
     */
    @Test(description = "sendGraphQlRequest: sendGraphQlRequest: arg_0 = '\n\t\r', query = 'variables', variables = '\n\t\r' -> throw NoClassDefFoundError")
    public void testSendGraphQlRequestThrowsNCDFEWithBlankStringsAndNonEmptyString1() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequest("\n\t\r", "variables", "\n\t\r"));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.sendGraphQlRequest

    ///region

    @Test
    public void testSendGraphQlRequestByFuzzer() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequest("query", "\n\t\r", "\n\t\r", "abc"));
    }
    ///endregion

    ///region FUZZER: ERROR SUITE for method sendGraphQlRequest(java.lang.String, java.lang.String, java.lang.String, java.lang.String)

    @Test
    public void testSendGraphQlRequestByFuzzer1() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequest("query", "\n\t\r", "\n\t\r", "abc"));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.sendGraphQlRequestWithHeader

    ///region

    @Test
    public void testSendGraphQlRequestWithHeaderByFuzzer() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequestWithHeader("abc", "\n\t\r", "abc", "query"));
    }
    ///endregion

    ///region FUZZER: ERROR SUITE for method sendGraphQlRequestWithHeader(java.lang.String, java.lang.String, java.lang.String, java.lang.String)

    @Test
    public void testSendGraphQlRequestWithHeaderByFuzzer1() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequestWithHeader("abc", "\n\t\r", "abc", "query"));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.sendGraphQlRequestWithHeader

    ///region

    @Test
    public void testSendGraphQlRequestWithHeaderByFuzzer2() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequestWithHeader("", "fragment", "\n\t\r", "fragment", "\n\t\r", "query"));
    }

    @Test
    public void testSendGraphQlRequestWithHeader1() {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            Object object = new Object();
            mockedConstruction = mockConstruction(org.json.simple.JSONObject.class, (org.json.simple.JSONObject jSONObjectMock, org.mockito.MockedConstruction.Context context) -> (when(jSONObjectMock.put(any(), any()))).thenReturn(object, object, object));
            String base_URI = "";

            assertThrows(IllegalArgumentException.class, () -> RestActions.sendGraphQlRequestWithHeader(base_URI, null, null, null, null, null));
        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///region FUZZER: ERROR SUITE for method sendGraphQlRequestWithHeader(java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String)

    @Test
    public void testSendGraphQlRequestWithHeaderByFuzzer3() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequestWithHeader("", "fragment", "\n\t\r", "fragment", "\n\t\r", "query"));
    }
    ///endregion

    ///region OTHER: ERROR SUITE for method sendGraphQlRequestWithHeader(java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String)

    @Test
    public void testSendGraphQlRequestWithHeader11() {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            Object object = new Object();
            mockedConstruction = mockConstruction(org.json.simple.JSONObject.class, (org.json.simple.JSONObject jSONObjectMock, org.mockito.MockedConstruction.Context context) -> (when(jSONObjectMock.put(any(), any()))).thenReturn(object, object, object));
            String base_URI = "";

            assertThrows(IllegalArgumentException.class, () -> RestActions.sendGraphQlRequestWithHeader(base_URI, null, null, null, null, null));
        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.api.RestActions.sendGraphQlRequestWithHeader

    ///region

    @Test(enabled = false, description = "Disabled due to sandbox")
    public void testSendGraphQlRequestWithHeaderByFuzzer4() {
        assertThrows(ExceptionInInitializerError.class, () -> RestActions.sendGraphQlRequestWithHeader("\n\t\r", "", "", "query", "\n\t\r"));
    }

    @Test
    public void testSendGraphQlRequestWithHeaderByFuzzer5() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequestWithHeader("query", "\n\t\r", "\n\t\r", "abc", ""));
    }
    ///endregion

    ///region FUZZER: SECURITY for method sendGraphQlRequestWithHeader(java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String)

    @Test(enabled = false, description = "Disabled due to sandbox")
    public void testSendGraphQlRequestWithHeaderByFuzzer6() {
        assertThrows(ExceptionInInitializerError.class, () -> RestActions.sendGraphQlRequestWithHeader("\n\t\r", "", "", "query", "\n\t\r"));
    }
    ///endregion

    ///region FUZZER: ERROR SUITE for method sendGraphQlRequestWithHeader(java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String)

    @Test
    public void testSendGraphQlRequestWithHeaderByFuzzer7() {
        assertThrows(AssertionError.class, () -> RestActions.sendGraphQlRequestWithHeader("query", "\n\t\r", "\n\t\r", "abc", ""));
    }
    ///endregion

    ///endregion
}
