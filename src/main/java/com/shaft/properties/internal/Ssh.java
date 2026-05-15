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

    @Key("sshStrictHostKeyChecking")
    @DefaultValue("no")
    String strictHostKeyChecking();

    @Key("sshConnectTimeout")
    @DefaultValue("30000")
    int connectTimeout();

    @Key("sshServerAliveInterval")
    @DefaultValue("0")
    int serverAliveInterval();

    @Key("sshKnownHostsFile")
    @DefaultValue("")
    String knownHostsFile();

    @Key("sshAttachCommandOutputToReport")
    @DefaultValue("false")
    boolean attachCommandOutputToReport();

    @Key("sshCommandOutputReportRedactRegex")
    @DefaultValue("")
    String commandOutputReportRedactRegex();

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

        public SetProperty knownHostsFile(String path) {
            setProperty("sshKnownHostsFile", path);
            return this;
        }

        public SetProperty attachCommandOutputToReport(boolean value) {
            setProperty("sshAttachCommandOutputToReport", String.valueOf(value));
            return this;
        }

        public SetProperty commandOutputReportRedactRegex(String javaRegex) {
            setProperty("sshCommandOutputReportRedactRegex", javaRegex);
            return this;
        }
    }
}
