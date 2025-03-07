package com.shaft.properties.internal;

/**
 * @author Kyrillos Nageh
 */
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({
        "system:properties",
        "file:src/main/resources/properties/Api.properties",
        "file:src/main/resources/properties/default/Api.properties",
        "classpath:Api.properties"
})
public interface Api extends EngineProperties<Api> {
    @Key("swaggerUrl")
    @DefaultValue("https://petstore.swagger.io/v2/swagger.json")
    String swaggerUrl();

    @Key("apiBaseUrl")
    @DefaultValue("https://petstore.swagger.io/")
    String apiBaseUrl();


    @Key("apiVersion")
    @DefaultValue("v2")
    String apiVersion();

    static Api getInstance() {
        return ConfigFactory.create(Api.class);
    }

    void setSwaggerUrl(String url);
}

