package testPackage.properties;

import org.aeonbits.owner.Config;
import org.aeonbits.owner.Config.HotReload;
import org.aeonbits.owner.Config.HotReloadType;
import org.aeonbits.owner.Config.LoadPolicy;

//TODO: delete this file

@HotReload(type = HotReloadType.ASYNC)
@LoadPolicy(Config.LoadType.MERGE)
public interface EngineProperties extends Config {
    SetProperty set();

    interface SetProperty {
    }
}
