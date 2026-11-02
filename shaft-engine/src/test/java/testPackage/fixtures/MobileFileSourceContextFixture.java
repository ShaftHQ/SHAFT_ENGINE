package testPackage.fixtures;

import com.shaft.gui.driver.MobileFileActionsContract;

import java.nio.file.Path;

/** Calls mobile file actions from a consumer package so source-context filtering can be verified. */
public final class MobileFileSourceContextFixture {
    private MobileFileSourceContextFixture() {
        throw new IllegalStateException("Utility class");
    }

    public static void invalidPushText(MobileFileActionsContract files) {
        files.pushText(" ", "opaque-source-text-9941");
    }

    public static void unsupportedPull(MobileFileActionsContract files) {
        files.pull("/private/source-path-6724");
    }

    public static void invalidLocalSource(MobileFileActionsContract files) {
        files.pushFrom("/device/target", Path.of("opaque-local-source-8181"));
    }
}
