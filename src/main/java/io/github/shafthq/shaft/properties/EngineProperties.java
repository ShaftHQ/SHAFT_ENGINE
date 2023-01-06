package io.github.shafthq.shaft.properties;

import org.aeonbits.owner.Config;

@Config.LoadPolicy(Config.LoadType.MERGE)
public interface EngineProperties extends Config {
    void setProperty(String key, String value);
}
