package testPackage01;

import org.testng.annotations.Test;

import com.shaft.gui.image.ImageProcessingActions;

public class Test_imageComparison {
    @Test
    public void f() {
	String refrenceFolderPath = "src/test/resources/TestDataFiles/imageComparer/reference";
	String testFolderPath = "src/test/resources/TestDataFiles/imageComparer/test";

	ImageProcessingActions.compareImageFolders(refrenceFolderPath, testFolderPath, 98);
    }
}
