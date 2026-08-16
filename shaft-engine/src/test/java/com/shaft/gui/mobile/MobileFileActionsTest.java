package com.shaft.gui.mobile;

import com.shaft.driver.SHAFT;
import com.shaft.gui.driver.MobileFileActionsContract;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;

public class MobileFileActionsTest {
    private Path tempDirectory;

    @BeforeMethod
    public void createTempDirectory() throws IOException {
        tempDirectory = Files.createTempDirectory("shaft-mobile-files-test-");
    }

    @AfterMethod
    public void deleteTempDirectory() throws IOException {
        if (tempDirectory == null || !Files.exists(tempDirectory)) {
            return;
        }
        try (var paths = Files.walk(tempDirectory)) {
            for (Path path : paths.sorted(Comparator.reverseOrder()).toList()) {
                Files.deleteIfExists(path);
            }
        }
    }

    @Test
    public void bytesTextAndFolderConveniencesShouldDelegateToTheExactAppiumInterfaces() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("mobile-files"));
        Mockito.when(driver.pullFile("/device/data.bin")).thenReturn(new byte[]{1, 2, 3});
        Mockito.when(driver.pullFile("/device/message.txt"))
                .thenReturn("hello mobile".getBytes(StandardCharsets.UTF_8));
        Mockito.when(driver.pullFolder("/device/folder")).thenReturn(new byte[]{4, 5, 6});
        MobileActions mobile = new SHAFT.GUI.WebDriver(driver).mobile();

        MobileFileActionsContract files = mobile.files();

        Assert.assertEquals(files.pull("/device/data.bin"), new byte[]{1, 2, 3});
        Assert.assertEquals(files.pullText("/device/message.txt"), "hello mobile");
        Assert.assertEquals(files.pullFolder("/device/folder"), new byte[]{4, 5, 6});
        Assert.assertSame(files.push("/device/data.bin", new byte[]{7, 8})
                .pushText("/device/message.txt", "updated"), files);
        Assert.assertSame(files.and(), mobile);
        Mockito.verify(driver).pushFile("/device/data.bin", new byte[]{7, 8});
        Mockito.verify(driver).pushFile("/device/message.txt", "updated".getBytes(StandardCharsets.UTF_8));
    }

    @Test
    public void localPathConveniencesShouldPublishExactTargetsAndPushLocalBytes() throws IOException {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("mobile-file-paths"));
        byte[] pulled = "replacement".getBytes(StandardCharsets.UTF_8);
        Mockito.when(driver.pullFile("/device/result.txt")).thenReturn(pulled);
        Path target = tempDirectory.resolve("nested/result.txt");
        Files.createDirectories(target.getParent());
        Files.writeString(target, "known-good", StandardCharsets.UTF_8);
        Path source = tempDirectory.resolve("source.txt");
        byte[] submitted = "submitted".getBytes(StandardCharsets.UTF_8);
        Files.write(source, submitted);
        MobileActions mobile = new SHAFT.GUI.WebDriver(driver).mobile();

        Path published = mobile.files().pullTo("/device/result.txt", target);
        mobile.files().pushFrom("/device/source.txt", source);

        Assert.assertEquals(published, target.toAbsolutePath().normalize());
        Assert.assertEquals(Files.readAllBytes(target), pulled);
        Mockito.verify(driver).pushFile("/device/source.txt", submitted);
    }

    @Test
    public void unsupportedAndClosedSessionsShouldFailBeforeAProviderCommand() {
        AppiumDriver generic = Mockito.mock(AppiumDriver.class);
        Mockito.when(generic.getSessionId()).thenReturn(new SessionId("generic-file-session"));
        MobileFileActionsContract unsupported = new SHAFT.GUI.WebDriver(generic).mobile().files();

        Assert.expectThrows(UnsupportedOperationException.class, () -> unsupported.pull("/device/result"));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> unsupported.push("/device/source", new byte[]{1}));

        AndroidDriver closed = Mockito.mock(AndroidDriver.class);
        Mockito.when(closed.getSessionId()).thenReturn(new SessionId("closing-file-session"), (SessionId) null);
        MobileFileActionsContract stale = new SHAFT.GUI.WebDriver(closed).mobile().files();
        Assert.expectThrows(UnsupportedOperationException.class, () -> stale.pull("/device/result"));
        Mockito.verify(closed, Mockito.never()).pullFile(Mockito.anyString());
    }
}
