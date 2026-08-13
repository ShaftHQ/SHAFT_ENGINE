package com.shaft.gui.internal.image;

import com.shaft.gui.image.ImageMatch;

/** Maps screenshot-pixel image matches into W3C viewport pointer coordinates. */
public final class ImageCoordinateMapper {
    private ImageCoordinateMapper() {
    }

    public static int[] toPointerCenter(ImageMatch match, int screenshotWidth, int screenshotHeight,
                                        int viewportWidth, int viewportHeight) {
        if (screenshotWidth <= 0 || screenshotHeight <= 0 || viewportWidth <= 0 || viewportHeight <= 0) {
            throw new IllegalArgumentException("Screenshot and viewport dimensions must be positive.");
        }
        int x = (int) Math.round(match.centerX() * ((double) viewportWidth / screenshotWidth));
        int y = (int) Math.round(match.centerY() * ((double) viewportHeight / screenshotHeight));
        return new int[]{Math.max(0, Math.min(viewportWidth - 1, x)), Math.max(0, Math.min(viewportHeight - 1, y))};
    }
}
