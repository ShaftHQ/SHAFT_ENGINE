package com.shaft.gui.driver;

import java.nio.file.Path;

/** Mobile device file-transfer actions. */
public interface MobileFileActionsContract {
    /** Pulls one remote file as bytes. */
    byte[] pull(String devicePath);

    /** Pulls one UTF-8 text file. */
    String pullText(String devicePath);

    /** Pulls one remote file into an exact local target. */
    Path pullTo(String devicePath, Path localTarget);

    /** Pulls one remote folder as the provider's ZIP bytes. */
    byte[] pullFolder(String devicePath);

    /** Pushes bytes to one remote file. */
    MobileFileActionsContract push(String devicePath, byte[] content);

    /** Pushes UTF-8 text to one remote file. */
    MobileFileActionsContract pushText(String devicePath, String content);

    /** Pushes a local file to one remote file. */
    MobileFileActionsContract pushFrom(String devicePath, Path localSource);

    /** Returns the owning mobile namespace. */
    MobileActionsContract and();
}
