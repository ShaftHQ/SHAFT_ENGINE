package io.github.shafthq.shaft.properties;

import org.aeonbits.owner.Config;
import org.aeonbits.owner.Config.LoadPolicy;

@LoadPolicy(Config.LoadType.MERGE)
public interface EngineProperties extends Config {
    SetProperty set();

    interface SetProperty {
    }
}
