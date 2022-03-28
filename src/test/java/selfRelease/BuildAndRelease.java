package selfRelease;

import com.shaft.api.RequestBuilder.AuthenticationType;
import com.shaft.api.RestActions;
import com.shaft.api.RestActions.RequestType;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import org.testng.annotations.Test;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
//TODO: Make this work
public class BuildAndRelease {
	String releaseVersion = System.getProperty("project.version");
	String baseUri = "https://automatest.jfrog.io/artifactory/SHAFT_ENGINE/";
	String deploymentPath = "io/github/mohabmohie/SHAFT_ENGINE/"+releaseVersion;
	String mavenPath;
	
	@Test
	public void installToLocalRepository() {
		ReportManager.log("Installing SHAFT_Engine v." + releaseVersion + "to the local .m2 repository");
		SHAFT.CLI.performCLIAction().performTerminalCommand("mvn clean install -DskipTests");
		ReportManager.logDiscrete("Installation complete");
	}
	
	@Test(dependsOnMethods = {"installToLocalRepository"})
	public void publishJavaDocs() {
		ReportManager.log("Deploying javadocs to GitHub pages");
		SHAFT.CLI.performCLIAction().performTerminalCommand("mvn resources:resources javadoc:javadoc scm-publish:publish-scm");
		ReportManager.logDiscrete("Deployment completed");
	}
	
	@Test(dependsOnMethods = {"publishJavaDocs"})
	public void releaseToJfrog() {
		ReportManager.log("Releasing SHAFT_Engine to ["+deploymentPath+"]");
		deployFileToJfrog(".jar");
		deployFileToJfrog(".pom");
		deployFileToJfrog("-javadoc.jar");
		deployFileToJfrog("-sources.jar");
		ReportManager.logDiscrete("SHAFT_Engine v."+releaseVersion+" released successfully");
	}
	
	private void deployFileToJfrog(String filePostfix) {
		//https://www.jfrog.com/confluence/display/JFROG/Artifactory+REST+API
		ReportManager.logDiscrete("Releasing "+filePostfix);
		
		List<Object> jarFile = new ArrayList<>();
        jarFile.add(filePostfix.replace(".", "").replace("-", ""));
        String jarPath = System.getProperty("user.home") + "/.m2/repository/io/github/mohabmohie/SHAFT_ENGINE/"+releaseVersion+"/SHAFT_ENGINE-"+releaseVersion+filePostfix;
        jarFile.add(new File(jarPath));
        
        List<List<Object>> parameters = new ArrayList<>();
        parameters.add(jarFile);
		
		//curl -u myUser:myP455w0rd! -X PUT "http://localhost:8081/artifactory/my-repository/my/new/artifact/directory/file.txt" -T Desktop/myNewFile.txt
		SHAFT.API.getDriver(baseUri)
			.buildNewRequest(deploymentPath, RequestType.PUT)
			.setAuthentication("mohab.mohieeldeen@outlook.com", "VDjbW9@@4Ws4DKd", AuthenticationType.BASIC)
			.setParameters(parameters, RestActions.ParametersType.FORM)
			.performRequest();
	}


}
