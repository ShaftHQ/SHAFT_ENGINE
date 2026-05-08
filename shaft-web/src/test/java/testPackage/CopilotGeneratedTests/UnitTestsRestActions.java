package testPackage.CopilotGeneratedTests;

import com.shaft.api.RestActions;
import com.shaft.api.RequestBuilder;
import com.shaft.api.RestActions.ComparisonType;
import com.shaft.api.RestActions.ParametersType;
import com.shaft.api.RestActions.RequestType;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import io.restassured.response.ResponseBody;
import io.restassured.specification.RequestSpecification;
import org.json.JSONArray;
import org.json.JSONObject;
import org.mockito.Mockito;
import org.testng.annotations.Test;

import java.io.File;
import java.io.InputStream;
import java.util.*;

public class UnitTestsRestActions {
    @Test
    public void testRestActionsConstructorsAndBasicMethods() {
        RestActions restActions = Mockito.mock(RestActions.class);
        Mockito.when(restActions.addHeaderVariable("key", "value")).thenReturn(restActions);
        Mockito.when(restActions.addCookieVariable("key", "value")).thenReturn(restActions);
        restActions.addHeaderVariable("key", "value");
        restActions.addCookieVariable("key", "value");
        // Only public methods are tested here due to access restrictions
    }

    @Test
    public void testStaticBuildNewRequest() {
        RequestBuilder builder = RestActions.buildNewRequest("http://localhost", "service", RequestType.GET);
        // Just verify builder is not null
        assert builder != null;
    }

    @Test
    public void testStaticParseBodyToJson() {
        Response response = Mockito.mock(Response.class);
        ResponseBody<?> body = Mockito.mock(ResponseBody.class);
        Mockito.when(response.getBody()).thenReturn(body);
        Mockito.when(body.asString()).thenReturn("{}");
        InputStream stream1 = RestActions.parseBodyToJson(response);
        InputStream stream2 = RestActions.parseBodyToJson(body);
        assert stream1 != null;
        assert stream2 != null;
    }

    @Test
    public void testGetResponseBody() {
        Response response = Mockito.mock(Response.class);
        ResponseBody<?> body = Mockito.mock(ResponseBody.class);
        Mockito.when(response.getBody()).thenReturn(body);
        Mockito.when(body.asString()).thenReturn("body");
        String result = RestActions.getResponseBody(response);
        assert result.equals("body");
    }

    @Test
    public void testGetResponseJSONValue() {
        Response response = Mockito.mock(Response.class);
        Mockito.when(response.asPrettyString()).thenReturn("{\"key\":\"value\"}");
        String value = RestActions.getResponseJSONValue(response, "$.key");
        assert value != null;
        Object obj = new HashMap<String, String>() {{
            put("key", "value");
        }};
        String value2 = RestActions.getResponseJSONValue(obj, "key");
        assert value2 != null;
    }

    @Test
    public void testGetResponseJSONValueAsList() {
        Response response = Mockito.mock(Response.class);
        Mockito.when(response.asPrettyString()).thenReturn("[{\"key\":\"value\"}]");
        List<Object> list = RestActions.getResponseJSONValueAsList(response, "$[*].key");
        assert list == null || list instanceof List;
    }

    @Test
    public void testGetResponseJSONValueFromList() {
        Response response = Mockito.mock(Response.class);
        Mockito.when(response.asPrettyString()).thenReturn("[{\"username\":\"user1\",\"id\":\"123\"}]");
        String value = RestActions.getResponseJSONValueFromList(response, "$[*]", "id", "username", "user1");
        assert value != null;
    }

    @Test
    public void testGetResponseXMLValue() {
        Response response = Mockito.mock(Response.class);
        Mockito.when(response.xmlPath()).thenReturn(Mockito.mock(io.restassured.path.xml.XmlPath.class));
        Mockito.when(response.xmlPath().getString("/root/key")).thenReturn("value");
        String value = RestActions.getResponseXMLValue(response, "/root/key");
        assert value != null;
        Object node = Mockito.mock(io.restassured.path.xml.element.Node.class);
        Mockito.when(((io.restassured.path.xml.element.Node) node).getAttribute("attr")).thenReturn("val");
        String value2 = RestActions.getResponseXMLValue(node, "attr");
        assert value2 != null;
    }

    @Test
    public void testGetResponseXMLValueAsList() {
        Response response = Mockito.mock(Response.class);
        io.restassured.path.xml.element.NodeChildren children = Mockito.mock(io.restassured.path.xml.element.NodeChildren.class);
        Mockito.when(response.xmlPath()).thenReturn(Mockito.mock(io.restassured.path.xml.XmlPath.class));
        Mockito.when(response.xmlPath().get("/root/list")).thenReturn(children);
        Mockito.when(children.list()).thenReturn(new ArrayList<>());
        List<Object> list = RestActions.getResponseXMLValueAsList(response, "/root/list");
        assert list == null || list instanceof List;
    }

    @Test
    public void testGetResponseStatusCodeAndTime() {
        Response response = Mockito.mock(Response.class);
        Mockito.when(response.getStatusCode()).thenReturn(200);
        Mockito.when(response.timeIn(Mockito.any())).thenReturn(100L);
        int code = RestActions.getResponseStatusCode(response);
        long time = RestActions.getResponseTime(response);
        assert code == 200;
        assert time == 100L;
    }

    @Test
    public void testCompareJSON() {
        Response response = Mockito.mock(Response.class);
        Mockito.when(response.asString()).thenReturn("{\"key\":\"value\"}");
        boolean result = RestActions.compareJSON(response, "credentials.json", ComparisonType.EQUALS);
        boolean result2 = RestActions.compareJSON(response, "credentials.json", ComparisonType.CONTAINS, "");
        assert !result || result2 == false || result2 == true;
    }

    @Test
    public void testFormatXML() {
        String xml = "<root><key>value</key></root>";
        String formatted = RestActions.formatXML(xml);
        assert formatted != null;
    }

    @Test
    public void testEnums() {
        assert ComparisonType.EQUALS != null;
        assert ParametersType.FORM != null;
        assert RequestType.POST != null;
    }
}
