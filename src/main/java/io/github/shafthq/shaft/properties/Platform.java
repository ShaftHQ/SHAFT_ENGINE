package io.github.shafthq.shaft.properties;

import io.github.shafthq.shaft.enums.OperatingSystems;
import org.aeonbits.owner.Config;
import org.aeonbits.owner.ConfigFactory;

@Config.Sources({"system:properties",
        "file:src/main/resources/properties/ExecutionPlatform.properties",
        "file:src/main/resources/properties/default/ExecutionPlatform.properties",
        "classpath:ExecutionPlatform.properties",
})
public interface Platform extends EngineProperties {
    @Key("SHAFT.CrossBrowserMode")
    @DefaultValue("off")
    String crossBrowserMode();

    @Key("executionAddress")
    @DefaultValue("local")
    String executionAddress();

    @Key("targetOperatingSystem")
    @DefaultValue(OperatingSystems.LINUX)
    String targetOperatingSystem();

    @Key("com.SHAFT.proxySettings")
    @DefaultValue("")
    String proxySettings();

    @Override
    default void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.platform = ConfigFactory.create(Platform.class, updatedProps);
    }
}
