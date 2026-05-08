package testPackage.legacy;

import com.shaft.gui.internal.image.ImageProcessingActions;
import org.testng.annotations.Test;

public class ImageComparisonTests {
    @Test(enabled = false)
    public void f() {
        String refrenceFolderPath = "src/test/resources/TestDataFiles/imageComparer/reference";
        String testFolderPath = "src/test/resources/TestDataFiles/imageComparer/test";

        ImageProcessingActions.compareImageFolders(refrenceFolderPath, testFolderPath, 98);
    }
}
