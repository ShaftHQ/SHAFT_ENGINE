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
        URI uri = parseUri(candidate);
        validateOriginUri(uri);
        HostAndPort parsed = parseAuthority(uri.getRawAuthority());
        String normalizedHost = normalizeHost(parsed);
        String normalizedScheme = uri.getScheme().toLowerCase(Locale.ROOT);
        Integer normalizedPort = isDefaultPort(normalizedScheme, parsed.port()) ? null : parsed.port();
        String serializedHost = parsed.ipv6() ? "[" + normalizedHost + "]" : normalizedHost;
        String normalized = normalizedScheme + "://" + serializedHost
                + (normalizedPort == null ? "" : ":" + normalizedPort);
        validateNormalizedUri(normalized);
        return normalized;
    }

    private static URI parseUri(String candidate) {
        try {
            return URI.create(candidate);
        } catch (IllegalArgumentException exception) {
            throw invalid(exception);
        }
    }

    private static void validateOriginUri(URI uri) {
        String authority = uri.getRawAuthority();
        if (!uri.isAbsolute() || uri.getScheme() == null || authority == null || authority.isBlank()
                || authority.contains("@") || uri.getRawQuery() != null || uri.getRawFragment() != null
                || (uri.getRawPath() != null && !uri.getRawPath().isEmpty() && !uri.getRawPath().equals("/"))) {
            throw invalid(null);
        }
    }

    private static String normalizeHost(HostAndPort parsed) {
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
        return normalizedHost;
    }

    private static void validateNormalizedUri(String normalized) {
        try {
            if (URI.create(normalized).getHost() == null) {
                throw invalid(null);
            }
        } catch (IllegalArgumentException exception) {
            throw invalid(exception);
        }
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

    private static boolean isDefaultPort(String scheme, Integer port) {
        return port != null && ((port == 80 && (scheme.equals("http") || scheme.equals("ws")))
                || (port == 443 && (scheme.equals("https") || scheme.equals("wss"))));
    }

    private static Integer invalidPort() {
        throw invalid(null);
    }

    private static IllegalArgumentException invalid(Throwable cause) {
        return new IllegalArgumentException("Permission origin must contain only a valid scheme, host, and optional port.", cause);
    }

    private record HostAndPort(String host, Integer port, boolean ipv6) { }
}
