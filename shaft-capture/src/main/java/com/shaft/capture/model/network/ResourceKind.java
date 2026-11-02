package com.shaft.capture.model.network;

/**
 * Category of network resource associated with a captured transaction.
 */
public enum ResourceKind {
    XHR,
    FETCH,
    DOCUMENT,
    WEBSOCKET_HANDSHAKE,
    OTHER,
    STYLESHEET,
    SCRIPT,
    IMAGE,
    FONT,
    MEDIA;

    /**
     * Reports whether this kind is asset-like noise (stylesheet, script, image, font, or media)
     * rather than API/document/websocket traffic.
     *
     * @return {@code true} for asset-type resource kinds
     */
    public boolean isAsset() {
        return switch (this) {
            case STYLESHEET, SCRIPT, IMAGE, FONT, MEDIA -> true;
            default -> false;
        };
    }
}
