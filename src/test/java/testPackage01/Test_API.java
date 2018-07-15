package testPackage01;

import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaftEngine.ioActionLibrary.ReportManager;
import com.shaftEngine.restAssuredActionLibrary.RestActions;

import io.restassured.response.Response;

public class Test_API {

	private final String serviceURI = "http://qa.incorta.com:6080/incorta";
	private String serviceName;
	private String requestType;
	private String argument = "";
	private Response response;
	private final static String successStatusCode = "200";

	@BeforeClass(description = "Log in to incorta via API")
	public void authenticate() {
		// Defining request parameters
		serviceName = "/authservice/login";
		requestType = "POST";
		argument = "tenant=demo2&user=admin&Login=Login&pass=admin";

		// Performing Authentication
		RestActions.performRequest(requestType, successStatusCode, serviceURI, serviceName, argument);
		isUserLoggedIn();
	}
	
	@AfterMethod(description = "Attach current test log")
	public void attachTestLog() {
		ReportManager.getTestLog();
	}
	
	@AfterClass(description = "Log out from incorta and attach full execution log")
	public void logOut() {
		// Defining request parameters
		serviceName = "/authservice/logout";
		requestType = "POST";
		argument = "";

		// Performing Request
		RestActions.performRequest(requestType, successStatusCode, serviceURI, serviceName, argument);
		ReportManager.getFullLog();
	}

	
	@Test(priority = 0, description = "TC001 - Is User Logged In" , enabled = true)
	public void isUserLoggedIn(){
		// Defining request parameters
		serviceName = "/service/user/isLoggedIn";
		requestType = "GET";
		argument = "";

		// Performing Request
		response = RestActions.performRequest(requestType, successStatusCode, serviceURI, serviceName, argument);
		//authenticate();
		
	}
	
	@Test(priority = 1, description = "TC002 - GET Users", enabled = true)
	public void SendGETRequestTo_service_user_getUsers_AndAssertThatResponseContainsACertainName() {
		// Defining request parameters
		serviceName = "/service/user/getUsers";
		requestType = "GET";
		argument = "pageSize=1000";

		// Performing Request
		response = RestActions.performRequest(requestType, successStatusCode, serviceURI, serviceName, argument);
		RestActions.assertResponse_JSON_ContainsValue(response, "users.name", "Mohamed ElPrince");
		// RestActions.assertResponse_JSON_ContainsValue(response, "users.batman",
		// "Mohamed ElPrince");
	}
}
