package com.shaft.tools.io.internal;

import java.nio.charset.StandardCharsets;
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

    static Matcher tokens(CharSequence text) {
        return TOKEN.matcher(text);
    }

    /**
     * A reusable ISO-8859-1 view of one byte buffer. Regex scans this directly so a
     * large report does not allocate a String per window.
     */
    static final class Latin1Window implements CharSequence {
        private final byte[] data;
        private int length;

        Latin1Window(byte[] data) {
            this.data = data;
        }

        void setLength(int length) {
            this.length = length;
        }

        @Override
        public int length() {
            return length;
        }

        @Override
        public char charAt(int index) {
            return (char) (data[index] & 0xff);
        }

        @Override
        public CharSequence subSequence(int start, int end) {
            return new String(data, start, end - start, StandardCharsets.ISO_8859_1);
        }
    }

    static boolean atLeast(Matcher token, int minimum) {
        return token.end() - token.start() >= minimum;
    }
}
