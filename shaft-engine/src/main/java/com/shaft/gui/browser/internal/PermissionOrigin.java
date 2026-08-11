package com.shaft.gui.browser.internal;

import java.net.IDN;
import java.net.URI;
import java.util.Locale;
import java.util.Objects;

/** Validates and normalizes a credential-free browser permission origin. */
public final class PermissionOrigin {
    private PermissionOrigin() {
        throw new IllegalStateException("Utility class");
    }

    public static String normalize(String origin) {
        String candidate = Objects.requireNonNull(origin, "origin").trim();
        URI uri;
        try {
            uri = URI.create(candidate);
        } catch (IllegalArgumentException exception) {
            throw invalid(exception);
        }
        String authority = uri.getRawAuthority();
        if (!uri.isAbsolute() || uri.getScheme() == null || authority == null || authority.isBlank()
                || authority.contains("@") || uri.getRawQuery() != null || uri.getRawFragment() != null
                || (uri.getRawPath() != null && !uri.getRawPath().isEmpty() && !uri.getRawPath().equals("/"))) {
            throw invalid(null);
        }

        HostAndPort parsed = parseAuthority(authority);
        String normalizedHost;
        try {
            normalizedHost = parsed.ipv6()
                    ? parsed.host().toLowerCase(Locale.ROOT)
                    : IDN.toASCII(parsed.host(), IDN.USE_STD3_ASCII_RULES).toLowerCase(Locale.ROOT);
        } catch (IllegalArgumentException exception) {
            throw invalid(exception);
        }
        if (normalizedHost.isBlank()) {
            throw invalid(null);
        }
        String serializedHost = parsed.ipv6() ? "[" + normalizedHost + "]" : normalizedHost;
        String normalized = uri.getScheme().toLowerCase(Locale.ROOT) + "://" + serializedHost
                + (parsed.port() == null ? "" : ":" + parsed.port());
        try {
            if (URI.create(normalized).getHost() == null) {
                throw invalid(null);
            }
        } catch (IllegalArgumentException exception) {
            throw invalid(exception);
        }
        return normalized;
    }

    private static HostAndPort parseAuthority(String authority) {
        if (authority.startsWith("[")) {
            int end = authority.indexOf(']');
            if (end <= 1) {
                throw invalid(null);
            }
            String remainder = authority.substring(end + 1);
            Integer port = remainder.isEmpty() ? null
                    : remainder.startsWith(":") ? parsePort(remainder.substring(1)) : invalidPort();
            return new HostAndPort(authority.substring(1, end), port, true);
        }
        int firstColon = authority.indexOf(':');
        int lastColon = authority.lastIndexOf(':');
        if (firstColon != lastColon) {
            throw invalid(null);
        }
        String host = firstColon < 0 ? authority : authority.substring(0, firstColon);
        Integer port = firstColon < 0 ? null : parsePort(authority.substring(firstColon + 1));
        return new HostAndPort(host, port, false);
    }

    private static Integer parsePort(String value) {
        try {
            int port = Integer.parseInt(value);
            if (port < 0 || port > 65535) {
                throw invalid(null);
            }
            return port;
        } catch (NumberFormatException exception) {
            throw invalid(exception);
        }
    }

    private static Integer invalidPort() {
        throw invalid(null);
    }

    private static IllegalArgumentException invalid(Throwable cause) {
        return new IllegalArgumentException("Permission origin must contain only a valid scheme, host, and optional port.", cause);
    }

    private record HostAndPort(String host, Integer port, boolean ipv6) { }
}
