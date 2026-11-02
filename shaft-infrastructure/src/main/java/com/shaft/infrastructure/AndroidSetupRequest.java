package com.shaft.infrastructure;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.nio.charset.StandardCharsets;
import java.util.HexFormat;

/** Immutable Android virtual-device selection bound into a reviewed setup plan. */
public record AndroidSetupRequest(int apiLevel, String deviceProfile, String imageTag, String abi,
                                  String avdName, int ramMb, int cores, int appiumPort) {
    public AndroidSetupRequest {
        deviceProfile = requireToken(deviceProfile, "deviceProfile");
        imageTag = requireToken(imageTag, "imageTag");
        abi = requireToken(abi, "abi");
        avdName = requireToken(avdName, "avdName");
        if (apiLevel != AndroidSetupPlanner.API_LEVEL) {
            throw new IllegalArgumentException("Unsupported Android API level: " + apiLevel);
        }
        if (!deviceProfile.equals(AndroidSetupPlanner.DEVICE_PROFILE)
                || !imageTag.equals(AndroidSetupPlanner.IMAGE_TAG)) {
            throw new IllegalArgumentException("Android device profile and image tag must match the release manifest.");
        }
        if (!abi.equals("host") && !abi.equals("x86_64") && !abi.equals("arm64-v8a")) {
            throw new IllegalArgumentException("Unsupported Android ABI: " + abi);
        }
        if (ramMb < 2048 || ramMb > 32768) {
            throw new IllegalArgumentException("Android emulator RAM must be between 2048 and 32768 MB.");
        }
        if (cores < 1 || cores > 16) {
            throw new IllegalArgumentException("Android emulator cores must be between 1 and 16.");
        }
        if (appiumPort < 1024 || appiumPort > 65535) {
            throw new IllegalArgumentException("Appium port must be between 1024 and 65535.");
        }
        if (appiumPort == 5554 || appiumPort == 5555) {
            throw new IllegalArgumentException("Appium port must not overlap the owned emulator ports 5554/5555.");
        }
    }

    /** Returns the release defaults; the host ABI is resolved during planning. */
    public static AndroidSetupRequest defaults() {
        return new AndroidSetupRequest(AndroidSetupPlanner.API_LEVEL, AndroidSetupPlanner.DEVICE_PROFILE,
                AndroidSetupPlanner.IMAGE_TAG, "host",
                "shaft_pixel_8_api_" + AndroidSetupPlanner.API_LEVEL, AndroidSetupPlanner.RAM_MB,
                AndroidSetupPlanner.CORES, AndroidSetupPlanner.APPIUM_PORT);
    }

    /** Encodes this typed request into the provider-neutral selection boundary. */
    public SetupSelection toSelection() {
        return new SetupSelection(List.of("api_" + apiLevel,
                "device_" + hex(deviceProfile), "tag_" + hex(imageTag), "abi_" + hex(abi),
                "avd_" + hex(avdName), "ram_" + ramMb, "cores_" + cores, "port_" + appiumPort));
    }

    /** Decodes a provider-neutral selection into one exact Android request. */
    public static AndroidSetupRequest fromSelection(SetupSelection selection) {
        Objects.requireNonNull(selection, "selection");
        if (selection.components().isEmpty()) return defaults();
        Map<String, String> values = new LinkedHashMap<>();
        for (String component : selection.components()) {
            int separator = component.indexOf('_');
            if (separator <= 0 || values.put(component.substring(0, separator),
                    component.substring(separator + 1)) != null) {
                throw new IllegalArgumentException("Android setup selection contains duplicate or invalid fields.");
            }
        }
        if (!values.keySet().equals(java.util.Set.of("api", "device", "tag", "abi", "avd", "ram", "cores", "port"))) {
            throw new IllegalArgumentException("Android setup selection must bind every supported field exactly once.");
        }
        try {
            return new AndroidSetupRequest(Integer.parseInt(values.get("api")), unhex(values.get("device")),
                    unhex(values.get("tag")), unhex(values.get("abi")), unhex(values.get("avd")),
                    Integer.parseInt(values.get("ram")), Integer.parseInt(values.get("cores")),
                    Integer.parseInt(values.get("port")));
        } catch (NumberFormatException failure) {
            throw new IllegalArgumentException("Android setup selection contains invalid numeric values.", failure);
        }
    }

    /** Reconstructs the exact request embedded in the Android-emulator plan action. */
    public static AndroidSetupRequest fromPlan(SetupPlan plan) {
        Objects.requireNonNull(plan, "plan");
        if (plan.profile() != SetupProfile.MOBILE_ANDROID) {
            throw new IllegalArgumentException("Plan is not an Android mobile setup plan.");
        }
        SetupAction action = plan.actions().stream()
                .filter(candidate -> candidate.target() == SetupTarget.ANDROID_EMULATOR)
                .findFirst().orElseThrow(() -> new IllegalArgumentException(
                        "Android plan does not contain an emulator action."));
        Map<String, String> values = new LinkedHashMap<>();
        for (String pair : action.version().split(",")) {
            int separator = pair.indexOf('=');
            if (separator <= 0 || values.put(pair.substring(0, separator), pair.substring(separator + 1)) != null) {
                throw new IllegalArgumentException("Android emulator action contains invalid request metadata.");
            }
        }
        try {
            AndroidSetupRequest request = new AndroidSetupRequest(Integer.parseInt(required(values, "api")),
                    required(values, "device"), required(values, "tag"), required(values, "abi"),
                    required(values, "avd"), Integer.parseInt(required(values, "ramMb")),
                    Integer.parseInt(required(values, "cores")), Integer.parseInt(required(values, "port")));
            if (!values.isEmpty()) {
                throw new IllegalArgumentException("Android emulator action contains unsupported metadata: "
                        + values.keySet());
            }
            return request;
        } catch (NumberFormatException failure) {
            throw new IllegalArgumentException("Android emulator action contains invalid numeric metadata.", failure);
        }
    }

    AndroidSetupRequest resolve(SetupArchitecture architecture) {
        String hostAbi = architecture == SetupArchitecture.ARM64 ? "arm64-v8a" : "x86_64";
        String resolvedAbi = abi.equals("host") ? hostAbi : abi;
        if (!resolvedAbi.equals(hostAbi)) {
            throw new IllegalArgumentException("Android ABI " + resolvedAbi + " does not match host architecture "
                    + architecture + '.');
        }
        String resolvedName = avdName.equals("shaft_pixel_8_api_" + AndroidSetupPlanner.API_LEVEL)
                ? avdName + '_' + resolvedAbi.replace('-', '_') : avdName;
        return new AndroidSetupRequest(apiLevel, deviceProfile, imageTag, resolvedAbi, resolvedName,
                ramMb, cores, appiumPort);
    }

    private static String requireToken(String value, String name) {
        if (value == null || !value.matches("[a-zA-Z0-9][a-zA-Z0-9_.-]{0,79}")) {
            throw new IllegalArgumentException(name + " must be a safe Android identifier.");
        }
        return value;
    }

    private static String required(Map<String, String> values, String name) {
        String value = values.remove(name);
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("Android emulator action is missing " + name + '.');
        }
        return value;
    }

    private static String hex(String value) {
        return HexFormat.of().formatHex(value.getBytes(StandardCharsets.UTF_8));
    }

    private static String unhex(String value) {
        try {
            return new String(HexFormat.of().parseHex(value), StandardCharsets.UTF_8);
        } catch (IllegalArgumentException failure) {
            throw new IllegalArgumentException("Android setup selection contains invalid encoded text.", failure);
        }
    }
}
