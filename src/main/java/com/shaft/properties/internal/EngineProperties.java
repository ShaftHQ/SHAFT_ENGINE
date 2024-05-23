package com.shaft.properties.internal;

import org.aeonbits.owner.Config;
import org.aeonbits.owner.Config.HotReload;
import org.aeonbits.owner.Config.HotReloadType;
import org.aeonbits.owner.Config.LoadPolicy;

// https://matteobaccan.github.io/owner/
@HotReload(type = HotReloadType.ASYNC)
@LoadPolicy(Config.LoadType.MERGE)
public interface EngineProperties<T> extends Config {
    SetProperty set();

    interface SetProperty {
    }
}
