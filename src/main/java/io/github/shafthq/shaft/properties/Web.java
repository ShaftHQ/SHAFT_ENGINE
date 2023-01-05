package io.github.shafthq.shaft.properties;

import io.github.shafthq.shaft.enums.Browsers;
import org.aeonbits.owner.Config;
import org.aeonbits.owner.ConfigFactory;

@Config.Sources({"system:properties",
        "file:src/main/resources/properties/WebCapabilities.properties",
        "file:src/main/resources/properties/default/WebCapabilities.properties",
        "classpath:WebCapabilities.properties",
})
public interface Web extends EngineProperties {
    @Key("targetBrowserName")
    @DefaultValue(Browsers.CHROME)
    Browsers targetBrowserName();

    @Key("headlessExecution")
    @DefaultValue("false")
    boolean headlessExecution();

    @Key("isMobileEmulation")
    @DefaultValue("false")
    boolean isMobileEmulation();

    @Key("mobileEmulation.isCustomDevice")
    @DefaultValue("false")
    boolean mobileEmulation_isCustomDevice();

    @Key("mobileEmulation.deviceName")
    @DefaultValue("")
    String mobileEmulation_deviceName();

    @Key("mobileEmulation.width")
    @DefaultValue("")
    String mobileEmulation_width();

    @Key("mobileEmulation.height")
    @DefaultValue("")
    String mobileEmulation_height();

    @Key("mobileEmulation.pixelRatio")
    @DefaultValue("")
    String mobileEmulation_pixelRatio();

    @Key("mobileEmulation.userAgent")
    @DefaultValue("")
    String mobileEmulation_userAgent();

    @Key("baseURL")
    @DefaultValue("")
    String baseURL();

    @Override
    default void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.web = ConfigFactory.create(Web.class, updatedProps);
    }
}
