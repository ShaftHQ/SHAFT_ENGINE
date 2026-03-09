package com.shaft.properties.internal;

import org.aeonbits.owner.Config;
import org.aeonbits.owner.Config.HotReload;
import org.aeonbits.owner.Config.HotReloadType;
import org.aeonbits.owner.Config.LoadPolicy;

// https://matteobaccan.github.io/owner/
/**
 * Base interface for all SHAFT engine configuration properties.
 * Built on top of the <a href="https://matteobaccan.github.io/owner/">OWNER</a> library, this
 * interface enforces asynchronous hot-reload and a merge load-policy so that property sources
 * (system properties, files, classpath resources) are combined in priority order.
 *
 * <p>Every concrete properties interface in SHAFT extends this type and provides a
 * {@link #set()} method that returns a concrete {@link SetProperty} fluent builder.
 *
 * <p>Example:
 * <pre>{@code
 * // Access properties via the SHAFT façade
 * boolean openReport = SHAFT.Properties.allure.automaticallyOpen();
 * // Override a property at runtime
 * SHAFT.Properties.allure.set().automaticallyOpen(false);
 * }</pre>
 *
 * @param <T> the concrete properties interface type (self-referential bound for fluent APIs)
 */
@HotReload(type = HotReloadType.ASYNC)
@LoadPolicy(Config.LoadType.MERGE)
public interface EngineProperties<T> extends Config {
    SetProperty set();

    interface SetProperty {
    }
}
