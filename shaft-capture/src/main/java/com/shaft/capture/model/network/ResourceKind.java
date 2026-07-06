package com.shaft.capture.model.network;

/**
 * Category of network resource associated with a captured transaction.
 */
public enum ResourceKind {
    XHR,
    FETCH,
    DOCUMENT,
    WEBSOCKET_HANDSHAKE,
    OTHER
}
