package com.shaft.cli.internal;

import com.jcraft.jsch.UIKeyboardInteractive;
import com.jcraft.jsch.UserInfo;

/**
 * Minimal {@link UserInfo} + {@link UIKeyboardInteractive} for password-based and keyboard-interactive auth.
 */
public final class SshUserInfoBridge implements UserInfo, UIKeyboardInteractive {
    private final String password;
    private final String keyPassphrase;

    public SshUserInfoBridge(String password, String keyPassphrase) {
        this.password = password;
        this.keyPassphrase = keyPassphrase;
    }

    @Override
    public String getPassphrase() {
        return emptyToNull(keyPassphrase);
    }

    @Override
    public String getPassword() {
        return emptyToNull(password);
    }

    @Override
    public boolean promptPassword(String message) {
        return password != null && !password.isEmpty();
    }

    @Override
    public boolean promptPassphrase(String message) {
        return keyPassphrase != null && !keyPassphrase.isEmpty();
    }

    @Override
    public boolean promptYesNo(String message) {
        return false;
    }

    @Override
    public void showMessage(String message) {
        // Non-interactive run; we do not surface server banners here.
    }

    @Override
    public String[] promptKeyboardInteractive(String destination, String name, String instruction, String[] prompt,
                                              boolean[] echo) {
        if (password == null || password.isEmpty() || prompt == null || prompt.length == 0) {
            return null;
        }
        String[] response = new String[prompt.length];
        for (int i = 0; i < prompt.length; i++) {
            response[i] = password;
        }
        return response;
    }

    private static String emptyToNull(String s) {
        return s == null || s.isEmpty() ? null : s;
    }
}
