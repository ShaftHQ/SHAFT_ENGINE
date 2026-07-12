package com.shaft.commandline;

import com.shaft.commandline.command.BrowserCommand;
import com.shaft.commandline.command.CallCommand;
import com.shaft.commandline.command.CaptureCommand;
import com.shaft.commandline.command.DoctorCommand;
import com.shaft.commandline.command.ElementCommand;
import com.shaft.commandline.command.GuideCommand;
import com.shaft.commandline.command.SessionCommand;
import com.shaft.commandline.command.ToolsCommand;
import picocli.CommandLine;
import picocli.CommandLine.Command;

import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;

/**
 * Root command for shaft-cli: a command-line client over the shaft-mcp capability set.
 */
@Command(
        name = "shaft-cli",
        mixinStandardHelpOptions = true,
        versionProvider = ShaftCli.ManifestVersionProvider.class,
        description = "Command-line client over the shaft-mcp capability set.",
        subcommands = {
                ToolsCommand.class,
                CallCommand.class,
                SessionCommand.class,
                BrowserCommand.class,
                ElementCommand.class,
                CaptureCommand.class,
                GuideCommand.class,
                DoctorCommand.class,
                CommandLine.HelpCommand.class})
public final class ShaftCli {

    /**
     * Process entry point.
     *
     * @param args CLI arguments
     */
    public static void main(String[] args) {
        PrintWriter out = utf8Writer(FileDescriptor.out);
        PrintWriter err = utf8Writer(FileDescriptor.err);
        CommandLine commandLine = new CommandLine(new ShaftCli()).setOut(out).setErr(err);
        System.exit(commandLine.execute(args));
    }

    private static PrintWriter utf8Writer(FileDescriptor descriptor) {
        return new PrintWriter(new OutputStreamWriter(new FileOutputStream(descriptor), StandardCharsets.UTF_8), true);
    }

    /**
     * Supplies the {@code --version} string from the jar manifest, falling back to a constant.
     */
    public static final class ManifestVersionProvider implements CommandLine.IVersionProvider {
        @Override
        public String[] getVersion() {
            String version = ShaftCli.class.getPackage().getImplementationVersion();
            return new String[]{"shaft-cli " + (version != null ? version : "(development)")};
        }
    }
}
