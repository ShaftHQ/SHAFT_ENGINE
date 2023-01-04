package testPackage01;

import io.github.shafthq.shaft.gui.image.ImageProcessingActions;
import org.testng.annotations.Test;

public class Test_imageComparison {
    @Test
    public void f() {
        String refrenceFolderPath = "src/test/resources/TestDataFiles/imageComparer/reference";
        String testFolderPath = "src/test/resources/TestDataFiles/imageComparer/test";

        ImageProcessingActions.compareImageFolders(refrenceFolderPath, testFolderPath, 98);
    }
}
