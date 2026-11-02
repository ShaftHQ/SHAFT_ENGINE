package com.shaft.mcp;

import com.shaft.capture.runtime.CaptureStatus;

/**
 * Union status for the {@code capture_api_start}/{@code capture_api_status}/{@code capture_api_stop}
 * family (design doc Decision 2 absorption of {@code mobile_api_record_start}/{@code _status}/
 * {@code _stop}): {@code engine} names which capture family produced this result, and exactly the
 * matching section below is populated -- the other stays {@code null}.
 *
 * <p>{@link ActiveEngine#WEB}/{@link ActiveEngine#NONE} report SHAFT's own CDP-managed-browser API
 * capture via {@code webStatus}. {@link ActiveEngine#MOBILE_NATIVE}/{@link ActiveEngine#MOBILE_WEB}
 * report the loopback-MITM-proxy mobile API capture via {@code mobileStatus}. The Playwright engine
 * has no dedicated API-capture mechanism, so it is not represented here -- callers on that engine
 * get the same actionable "not supported" error {@code capture_api_start} always returned.
 *
 * @param engine the capture family this status describes
 * @param webStatus populated when {@code engine} is {@link ActiveEngine#WEB}/{@link ActiveEngine#NONE}
 * @param mobileStatus populated when {@code engine} is {@link ActiveEngine#MOBILE_NATIVE}/
 *                      {@link ActiveEngine#MOBILE_WEB}
 */
public record McpCaptureApiUnionStatus(
        ActiveEngine engine,
        CaptureStatus webStatus,
        MobileApiCaptureStatus mobileStatus) {
    /**
     * Creates an immutable union status, normalizing a null {@code engine} to {@link ActiveEngine#WEB}.
     */
    public McpCaptureApiUnionStatus {
        engine = engine == null ? ActiveEngine.WEB : engine;
    }
}
