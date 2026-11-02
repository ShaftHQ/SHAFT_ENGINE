package com.shaft.intellij.notifications;

import com.intellij.notification.Notification;
import com.intellij.notification.NotificationAction;
import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

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

    /**
     * Shows an informational notification with a single action button.
     *
     * @param project project
     * @param title title
     * @param content content
     * @param actionText label for the action button
     * @param action callback invoked (on the EDT) when the action is clicked; the notification expires first
     */
    public static void infoWithAction(Project project, String title, String content, String actionText, Runnable action) {
        Notification notification = NotificationGroupManager.getInstance()
                .getNotificationGroup(GROUP_ID)
                .createNotification(title, content, NotificationType.INFORMATION);
        notification.addAction(new NotificationAction(actionText) {
            @Override
            public void actionPerformed(@NotNull AnActionEvent event, @NotNull Notification current) {
                current.expire();
                action.run();
            }
        });
        notification.notify(project);
    }

    private static void notify(Project project, String title, String content, NotificationType type) {
        NotificationGroupManager.getInstance()
                .getNotificationGroup(GROUP_ID)
                .createNotification(title, content, type)
                .notify(project);
    }
}
