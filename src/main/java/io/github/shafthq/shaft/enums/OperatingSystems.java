package io.github.shafthq.shaft.enums;
//
//public enum OperatingSystems {
//    LINUX("Linux-64"), MACOS("Mac-64"), WINDOWS("Windows-64"), ANDROID("Android"), IOS("iOS"),
//    FIREFOXOS("FirefoxOS");
//
//    private final String value;
//
//    OperatingSystems(String type) {
//        this.value = type;
//    }
//
//    public String getValue() {
//        return value;
//    }
//}

public interface OperatingSystems {
    String LINUX = "Linux-64";
    String MACOS = "Mac-64";
    String WINDOWS = "Windows-64";
    String ANDROID = "Android";
    String IOS = "iOS";
    String FIREFOXOS = "FirefoxOS";

    static String[] values() {
        return new String[]{LINUX, MACOS, WINDOWS, ANDROID, IOS, FIREFOXOS};
    }
}