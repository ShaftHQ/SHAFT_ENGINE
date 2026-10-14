package com.shaft.tools.io.internal;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * One base64 token scanner for published reports and Allure suite widgets.
 * Padding counts toward the token length. A run of alphabet characters that
 * stops before {@code =} misses a short blob whose secret or suite name is only in the decoded bytes.
 */
final class ReportBlobScan {
    static final int PUBLISH_MINIMUM = 32;
    static final int WIDGET_MINIMUM = 16;
    private static final Pattern TOKEN = Pattern.compile(
            "(?<![A-Za-z0-9+/])(?:[A-Za-z0-9+/]{4})+(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?");

    private ReportBlobScan() {
    }

    static Matcher tokens(String text) {
        return TOKEN.matcher(text);
    }

    static boolean atLeast(Matcher token, int minimum) {
        return token.end() - token.start() >= minimum;
    }
}
