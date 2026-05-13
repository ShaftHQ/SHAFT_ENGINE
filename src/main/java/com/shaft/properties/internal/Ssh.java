package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * SSH / JSch client settings for remote CLI operations.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.ssh.set().strictHostKeyChecking("ask");
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/Ssh.properties",
        "file:src/main/resources/properties/default/Ssh.properties",
        "classpath:Ssh.properties",
})
public interface Ssh extends EngineProperties<Ssh> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.sshOverride.set(ConfigFactory.create(Ssh.class, ThreadLocalPropertiesManager.getOverrides()));
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    /**
     * JSch {@code StrictHostKeyChecking}: typically {@code no}, {@code yes}, or {@code ask}.
     */
    @Key("sshStrictHostKeyChecking")
    @DefaultValue("no")
    String strictHostKeyChecking();

    /**
     * TCP connect timeout in milliseconds passed to {@link com.jcraft.jsch.Session#connect(int)}.
     */
    @Key("sshConnectTimeout")
    @DefaultValue("30000")
    int connectTimeout();

    /**
     * Server alive interval in milliseconds for persistent sessions ({@link com.jcraft.jsch.Session#setServerAliveInterval}).
     * Zero disables keepalive.
     */
    @Key("sshServerAliveInterval")
    @DefaultValue("0")
    int serverAliveInterval();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty strictHostKeyChecking(String value) {
            setProperty("sshStrictHostKeyChecking", value);
            return this;
        }

        public SetProperty connectTimeout(int valueMs) {
            setProperty("sshConnectTimeout", String.valueOf(valueMs));
            return this;
        }

        public SetProperty serverAliveInterval(int valueMs) {
            setProperty("sshServerAliveInterval", String.valueOf(valueMs));
            return this;
        }
    }
}
