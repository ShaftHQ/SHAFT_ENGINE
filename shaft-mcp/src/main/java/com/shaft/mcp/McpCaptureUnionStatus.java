package com.shaft.mcp;

import com.shaft.capture.runtime.CaptureStatus;

/**
 * Union status for the {@code capture_start}/{@code capture_status}/{@code capture_stop} family
 * (design doc amendment A3): {@code engine} names which recording family produced this result, and
 * exactly the matching section below is populated -- the other sections stay {@code null}.
 *
 * <p>{@link ActiveEngine#WEB} (and {@link ActiveEngine#NONE}, which behaves identically since no
 * other engine session is active) is SHAFT's own CDP-managed-browser Capture session, reported via
 * {@code webStatus}. {@link ActiveEngine#PLAYWRIGHT} and {@link ActiveEngine#MOBILE_NATIVE}/
 * {@link ActiveEngine#MOBILE_WEB} both report the shared {@link McpMobileRecordingStatus} recorder
 * status (the same DTO {@link PlaywrightService}'s/{@link MobileService}'s own package-private
 * recorders return) for whichever engine session is active, via {@code playwrightStatus}/
 * {@code mobileStatus} respectively.
 *
 * @param engine the recording family this status describes
 * @param webStatus populated when {@code engine} is {@link ActiveEngine#WEB}; {@code null} otherwise
 * @param playwrightStatus populated when {@code engine} is {@link ActiveEngine#PLAYWRIGHT}; {@code null} otherwise
 * @param mobileStatus populated when {@code engine} is {@link ActiveEngine#MOBILE_NATIVE} or
 *                      {@link ActiveEngine#MOBILE_WEB}; {@code null} otherwise
 */
public record McpCaptureUnionStatus(
        ActiveEngine engine,
        CaptureStatus webStatus,
        McpMobileRecordingStatus playwrightStatus,
        McpMobileRecordingStatus mobileStatus) {
    /**
     * Creates an immutable union status, normalizing a null {@code engine} to {@link ActiveEngine#WEB}.
     */
    public McpCaptureUnionStatus {
        engine = engine == null ? ActiveEngine.WEB : engine;
    }
}
