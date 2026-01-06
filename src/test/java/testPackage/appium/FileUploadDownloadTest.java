package testPackage.appium;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Test class to demonstrate and validate BrowserStack file upload/download capabilities
 * using pushFile and pullFile methods in TouchActions.
 */
public class FileUploadDownloadTest extends MobileTest {

    /**
     * Test to validate pushFile method with String path parameter.
     * This test creates a sample text file, uploads it to the device,
     * and demonstrates the usage of the pushFile method.
     */
    @Test(groups = {"NativeAndroidDemo"}, enabled = false)
    public void testPushFileWithStringPath() throws IOException {
        // Create a sample file to upload
        String localFilePath = "src/test/resources/testDataFiles/sampleUploadFile.txt";
        File localFile = new File(localFilePath);
        
        // Ensure parent directory exists
        if (!localFile.getParentFile().exists()) {
            localFile.getParentFile().mkdirs();
        }
        
        // Create sample content
        if (!localFile.exists()) {
            Files.writeString(localFile.toPath(), "This is a test file for BrowserStack upload feature.");
        }
        
        // Upload file to device using pushFile
        String devicePath = "/sdcard/Download/sampleUploadFile.txt";
        driver.get().touch()
                .pushFile(devicePath, localFilePath);
        
        // Note: In a real test scenario, you would verify the file exists on the device
        // by interacting with the app's file picker or file manager
    }

    /**
     * Test to validate pushFile method with File object parameter.
     * This test creates a sample PDF-like file, uploads it to the device,
     * and demonstrates the usage of the pushFile method with File object.
     */
    @Test(groups = {"NativeAndroidDemo"}, enabled = false)
    public void testPushFileWithFileObject() throws IOException {
        // Create a sample file to upload
        Path tempFile = Files.createTempFile("test_upload_", ".pdf");
        Files.writeString(tempFile, "This is a sample PDF content for testing.");
        File localFile = tempFile.toFile();
        
        // Upload file to device using pushFile with File object
        String devicePath = "/sdcard/Download/sample_document.pdf";
        driver.get().touch()
                .pushFile(devicePath, localFile);
        
        // Clean up temporary file
        Files.deleteIfExists(tempFile);
        
        // Note: In a real test scenario, you would verify the file exists on the device
        // and can be accessed by the application under test
    }

    /**
     * Test to validate pullFile method.
     * This test assumes a file exists on the device and attempts to download it
     * to the local machine.
     */
    @Test(groups = {"NativeAndroidDemo"}, enabled = false)
    public void testPullFile() throws IOException {
        // First, upload a file to ensure it exists on the device
        String localUploadPath = "src/test/resources/testDataFiles/samplePullFile.txt";
        File uploadFile = new File(localUploadPath);
        
        // Ensure parent directory exists
        if (!uploadFile.getParentFile().exists()) {
            uploadFile.getParentFile().mkdirs();
        }
        
        // Create sample content
        String testContent = "This is a test file to be pulled from the device.";
        if (!uploadFile.exists()) {
            Files.writeString(uploadFile.toPath(), testContent);
        }
        
        String devicePath = "/sdcard/Download/samplePullFile.txt";
        
        // Upload the file first
        driver.get().touch()
                .pushFile(devicePath, localUploadPath);
        
        // Now pull the file back from the device
        String localDownloadPath = "target/downloads/pulledFile.txt";
        driver.get().touch()
                .pullFile(devicePath, localDownloadPath);
        
        // Verify the downloaded file exists and has the expected content
        File downloadedFile = new File(localDownloadPath);
        SHAFT.Validations.assertThat()
                .object(downloadedFile.exists())
                .isTrue()
                .withCustomReportMessage("Downloaded file should exist");
        
        // Read and verify content
        String downloadedContent = Files.readString(downloadedFile.toPath());
        SHAFT.Validations.assertThat()
                .object(downloadedContent)
                .isEqualTo(testContent)
                .withCustomReportMessage("Downloaded file content should match original");
        
        // Clean up downloaded file
        Files.deleteIfExists(downloadedFile.toPath());
    }

    /**
     * Demonstration test showing typical usage pattern for file upload in BrowserStack.
     * This would be used in a real-world scenario where an app needs to upload a file
     * (e.g., profile picture, document upload, etc.)
     */
    @Test(groups = {"NativeAndroidDemo"}, enabled = false)
    public void demonstrateFileUploadUsagePattern() throws IOException {
        // Step 1: Prepare the file to upload
        String imageFilePath = "src/test/resources/testDataFiles/sample_image.jpg";
        File imageFile = new File(imageFilePath);
        
        // For demo purposes, create a placeholder if it doesn't exist
        if (!imageFile.exists()) {
            if (!imageFile.getParentFile().exists()) {
                imageFile.getParentFile().mkdirs();
            }
            Files.writeString(imageFile.toPath(), "Placeholder for image file");
        }
        
        // Step 2: Upload file to device
        String deviceImagePath = "/sdcard/Pictures/profile_picture.jpg";
        driver.get().touch()
                .pushFile(deviceImagePath, imageFile);
        
        // Step 3: In a real test, you would now interact with your app to:
        // - Open file picker
        // - Select the uploaded file
        // - Verify the file was successfully uploaded in the app
        
        // This demonstrates the complete workflow for BrowserStack file upload testing
    }
}
