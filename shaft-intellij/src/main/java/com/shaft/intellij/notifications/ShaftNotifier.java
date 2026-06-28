package com.shaft.intellij.notifications;

import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.project.Project;

/**
 * SHAFT notification helper.
 */
public final class ShaftNotifier {
    private static final String GROUP_ID = "SHAFT Notifications";

    private ShaftNotifier() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Shows an informational notification.
     *
     * @param project project
     * @param title title
     * @param content content
     */
    public static void info(Project project, String title, String content) {
        notify(project, title, content, NotificationType.INFORMATION);
    }

    /**
     * Shows a warning notification.
     *
     * @param project project
     * @param title title
     * @param content content
     */
    public static void warn(Project project, String title, String content) {
        notify(project, title, content, NotificationType.WARNING);
    }

    private static void notify(Project project, String title, String content, NotificationType type) {
        NotificationGroupManager.getInstance()
                .getNotificationGroup(GROUP_ID)
                .createNotification(title, content, type)
                .notify(project);
    }
}
