package com.shaft.commandline.command;

import com.shaft.commandline.mcp.McpException;
import com.shaft.commandline.session.SessionInfo;
import com.shaft.commandline.session.SessionManager;
import com.shaft.commandline.session.SessionStatus;
import picocli.CommandLine.Command;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Spec;

import java.io.PrintWriter;
import java.util.concurrent.Callable;

/**
 * Manages the persistent shaft-mcp daemon that backs session mode. With no subcommand it prints help.
 */
@Command(mixinStandardHelpOptions = true,
        
        name = "session",
        description = "Manage the shaft-mcp daemon session (start | status | stop).",
        subcommands = {
                SessionCommand.Start.class,
                SessionCommand.Status.class,
                SessionCommand.Stop.class})
public final class SessionCommand implements Callable<Integer> {

    @Spec
    private CommandSpec spec;

    @Override
    public Integer call() {
        spec.commandLine().usage(spec.commandLine().getOut());
        return 0;
    }

    /**
     * Starts the daemon and records the session.
     */
    @Command(mixinStandardHelpOptions = true,
        name = "start", description = "Launch the shaft-mcp daemon and record the session.")
    public static final class Start implements Callable<Integer> {

        @Spec
        private CommandSpec spec;

        private final SessionManager manager;

        /** Uses the real session manager. */
        public Start() {
            this(new SessionManager());
        }

        /** @param manager the session manager */
        public Start(SessionManager manager) {
            this.manager = manager;
        }

        @Override
        public Integer call() {
            PrintWriter out = spec.commandLine().getOut();
            PrintWriter err = spec.commandLine().getErr();
            try {
                SessionInfo info = manager.start();
                out.println("shaft-cli session started on port " + info.port() + " (pid " + info.pid() + ").");
                out.flush();
                return 0;
            } catch (McpException exception) {
                err.println(exception.getMessage());
                err.flush();
                return 1;
            }
        }
    }

    /**
     * Reports whether a recorded session is still running.
     */
    @Command(mixinStandardHelpOptions = true,
        name = "status", description = "Show whether a shaft-mcp session is running.")
    public static final class Status implements Callable<Integer> {

        @Spec
        private CommandSpec spec;

        private final SessionManager manager;

        /** Uses the real session manager. */
        public Status() {
            this(new SessionManager());
        }

        /** @param manager the session manager */
        public Status(SessionManager manager) {
            this.manager = manager;
        }

        @Override
        public Integer call() {
            PrintWriter out = spec.commandLine().getOut();
            SessionStatus status = manager.status();
            if (status.info() == null) {
                out.println("No shaft-cli session recorded.");
                out.flush();
                return 0;
            }
            SessionInfo info = status.info();
            out.println((status.running() ? "running" : "stopped")
                    + " — port " + info.port() + ", pid " + info.pid() + ", started " + info.startedAt());
            out.flush();
            return status.running() ? 0 : 1;
        }
    }

    /**
     * Terminates the recorded session.
     */
    @Command(mixinStandardHelpOptions = true,
        name = "stop", description = "Stop the shaft-mcp daemon and clear the session.")
    public static final class Stop implements Callable<Integer> {

        @Spec
        private CommandSpec spec;

        private final SessionManager manager;

        /** Uses the real session manager. */
        public Stop() {
            this(new SessionManager());
        }

        /** @param manager the session manager */
        public Stop(SessionManager manager) {
            this.manager = manager;
        }

        @Override
        public Integer call() {
            PrintWriter out = spec.commandLine().getOut();
            boolean stopped = manager.stop();
            out.println(stopped ? "shaft-cli session stopped." : "No shaft-cli session to stop.");
            out.flush();
            return 0;
        }
    }
}
