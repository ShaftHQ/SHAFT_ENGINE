package testPackage01;

import org.testng.annotations.Test;

import com.shaft.io.ExcelFileManager;
import com.shaft.io.ReportManager;

public class Test_testData {
	@Test
	public void f() {
		System.setProperty("testDataFilePath", "src/test/resources/TestDataFiles/testSuite01/TestData.xlsx");
		ExcelFileManager testDataReader = new ExcelFileManager(System.getProperty("testDataFilePath"));

		ReportManager.log("Last Column Number is: [" + testDataReader.getLastColumnNumber() + "]. Zero-based.");
		ReportManager.getTestLog();
	}
}
