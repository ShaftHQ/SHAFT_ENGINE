package testPackage.coverage;

import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

public class FileActionsTests {
    @Test
    public void copyFile(){
        SHAFT.CLI.file().copyFile("pom.xml","target/temp/pom.xml");
        SHAFT.Validations.assertThat().file("target/temp/","pom.xml").exists();
    }

    @Test
    public void getFileList(){
        var fileList = SHAFT.CLI.file().getFileList("src/main/java/com/shaft/properties/internal");
        StringBuilder stringBuilder = new StringBuilder();
        fileList.forEach(stringBuilder::append);
        SHAFT.Validations.assertThat().object(stringBuilder.toString()).contains("Allure");
    }

    @Test
    public void listFilesInDirectory(){
        var fileList = SHAFT.CLI.file().listFilesInDirectory("src/main/java/com/shaft/properties/internal");
        SHAFT.Validations.assertThat().object(fileList).contains("Allure");
    }

    @Test
    public void getFileChecksum(){
        var checksum = SHAFT.CLI.file().getFileChecksum(new TerminalActions(), "src/main/java/com/shaft/properties/internal/", "Allure.java");
        SHAFT.Validations.assertThat().object(checksum).isNotNull();
    }

    @Test
    public void readPDF1(){
        var fileContent = SHAFT.CLI.file().readPDF("src/test/resources/testDataFiles/sample.pdf");
        SHAFT.Validations.assertThat().object(fileContent).contains("A Simple PDF File");
    }

    @Test
    public void readPDF2(){
        var fileContent = SHAFT.CLI.file().readPDF("src/test/resources/testDataFiles/","sample.pdf");
        SHAFT.Validations.assertThat().object(fileContent).contains("A Simple PDF File");
    }

    @Test
    public void readFile(){
        var fileContent = SHAFT.CLI.file().readFile("src/test/resources/testDataFiles/","credentials.json");
        SHAFT.Validations.assertThat().object(fileContent).contains("validEmail@domain.com");
    }

    @Test
    public void zipFiles(){
        boolean zipSuccess = SHAFT.CLI.file().zipFiles("src/test/resources/testDataFiles/yaml","target/temp/testDataFiles_yaml.zip");
        SHAFT.Validations.assertThat().object(zipSuccess).isTrue();
    }

    @Test
    public void downloadFile(){
        SHAFT.CLI.file().downloadFile("https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/src/main/resources/images/shaft.png","target/temp/shaft.png");
        SHAFT.Validations.assertThat().file("target/temp/","shaft.png").exists();
    }

}
