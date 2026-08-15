package com.shaft.ai.local;

import com.shaft.pilot.ai.AiCapabilities;
import com.shaft.pilot.ai.AiProvider;
import com.shaft.pilot.ai.AiProviderAvailability;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.ProcessingLocation;

import java.time.Duration;
import java.util.Objects;
import java.util.function.BooleanSupplier;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

/** SHAFT-owned provider entrypoint for managed local inference. */
public final class ManagedLocalAiProvider implements AiProvider {
    private static final RuntimeClient PROCESS_CLIENT = new RuntimeClient() {
        @Override
        public ManagedLocalAiProcess.Session launch(ManagedLocalAiService.ReadyRuntime runtime, Duration timeout,
                                                    BooleanSupplier shuttingDown) throws Exception {
            var files = new ManagedLocalAiProcess.RuntimeFiles(
                    runtime.cache(), runtime.executable(), runtime.model(), runtime.log());
            var spec = new ManagedLocalAiProcess.RuntimeSpec(runtime.alias(), runtime.threads());
            var request = new ManagedLocalAiProcess.LaunchRequest(files, spec, timeout, shuttingDown);
            return ManagedLocalAiProcess.launchManaged(request, ManagedLocalAiProcess::start,
                    (process, port, key, alias, identityTimeout) ->
                            ManagedLocalAiProcess.requireIdentity(process, port, key, alias, identityTimeout,
                                    ManagedLocalAiProcess::requestIdentity));
        }

        @Override
        public AiResponse infer(ManagedLocalAiProcess.Session session, AiRequest request, long deadline)
                throws Exception {
            return ManagedLocalAiProcess.infer(session, request, ManagedLocalAiProcess::requestInference, deadline);
        }
    };
    private static final ServiceLifecycle SERVICE_LIFECYCLE = new ServiceLifecycle(new ManagedLocalAiService());
    static {
        Runtime.getRuntime().addShutdownHook(Thread.ofPlatform().name("shaft-managed-local-ai-shutdown")
                .unstarted(SERVICE_LIFECYCLE::shutdown));
    }

    final Lifecycle lifecycle;

    /** Creates the service-loadable managed provider. */
    public ManagedLocalAiProvider() {
        this(SERVICE_LIFECYCLE);
    }

    ManagedLocalAiProvider(Lifecycle lifecycle) {
        this.lifecycle = Objects.requireNonNull(lifecycle, "lifecycle");
    }

    @Override
    public String id() {
        return "managed-local";
    }

    @Override
    public AiCapabilities capabilities() {
        return new AiCapabilities(true, false, false, 0, ProcessingLocation.LOCAL);
    }

    @Override
    public AiProviderAvailability availability() {
        try {
            ManagedLocalAiSnapshot snapshot = lifecycle.inspect();
            if (lifecycle.executable() && (snapshot.state() == ManagedLocalAiSnapshot.State.READY
                    || snapshot.state() == ManagedLocalAiSnapshot.State.NOT_PROVISIONED
                    && snapshot.transparentProvisioning())) {
                return AiProviderAvailability.ready();
            }
            return AiProviderAvailability.unavailable(snapshot.action());
        } catch (RuntimeException failure) {
            return AiProviderAvailability.unavailable("Managed local inference status is unavailable.");
        }
    }

    @Override
    public AiResponse execute(AiRequest request) {
        Objects.requireNonNull(request, "request");
        try {
            return lifecycle.execute(request);
        } catch (RuntimeException failure) {
            return AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, id(), "",
                    "Managed local inference is unavailable.", Duration.ZERO, request.deterministicFallback());
        }
    }

    interface Lifecycle {
        boolean executable();
        ManagedLocalAiSnapshot inspect();
        AiResponse execute(AiRequest request);
    }

    interface RuntimeClient {
        ManagedLocalAiProcess.Session launch(ManagedLocalAiService.ReadyRuntime runtime, Duration timeout,
                                             BooleanSupplier shuttingDown) throws Exception;
        AiResponse infer(ManagedLocalAiProcess.Session session, AiRequest request, long deadline) throws Exception;
    }

    static final class ServiceLifecycle implements Lifecycle {
        private final ManagedLocalAiService service;
        private final RuntimeClient runtimeClient;
        final ReentrantLock executionLock = new ReentrantLock();
        volatile ManagedLocalAiProcess.Session session;
        private volatile ManagedLocalAiOperation activeProvisioning;
        private volatile Thread activeExecutionThread;
        private volatile boolean shuttingDown;

        ServiceLifecycle(ManagedLocalAiService service) {
            this(service, PROCESS_CLIENT);
        }

        ServiceLifecycle(ManagedLocalAiService service, RuntimeClient runtimeClient) {
            this.service = Objects.requireNonNull(service, "service");
            this.runtimeClient = Objects.requireNonNull(runtimeClient, "runtimeClient");
        }

        @Override
        public boolean executable() {
            return true;
        }

        @Override
        public ManagedLocalAiSnapshot inspect() {
            try {
                ManagedLocalAiSnapshot snapshot = service.inspect();
                if (snapshot.state() != ManagedLocalAiSnapshot.State.READY && session != null) {
                    retireUnavailableSession();
                }
                return snapshot;
            } catch (RuntimeException failure) {
                if (session != null) {
                    retireUnavailableSession();
                }
                throw failure;
            }
        }

        @Override
        public AiResponse execute(AiRequest request) {
            if (shuttingDown) {
                return unavailable(request, "Managed local inference is shutting down.");
            }
            ExecutionContext context = new ExecutionContext(request);
            try {
                if (!acquire(context)) {
                    return timedOut(request);
                }
                return executeOwned(context);
            } catch (java.util.concurrent.TimeoutException
                     | ManagedLocalAiProcess.DeadlineExceededException timeout) {
                cancel(context.provisioning);
                closeSession(context.locked, context.deadline, timeout);
                return timedOut(request);
            } catch (InterruptedException interrupted) {
                cancel(context.provisioning);
                closeSession(context.locked, context.deadline, interrupted);
                Thread.currentThread().interrupt();
                return AiResponse.failure(AiResponseStatus.ERROR, "managed-local", "",
                        "Managed local inference was interrupted.", Duration.ZERO,
                        request.deterministicFallback());
            } catch (Exception failure) {
                closeSession(context.locked, context.deadline, failure);
                return unavailable(request, "Managed local inference is unavailable.");
            } finally {
                release(context);
            }
        }

        private boolean acquire(ExecutionContext context)
                throws InterruptedException, ManagedLocalAiProcess.DeadlineExceededException {
            context.locked = executionLock.tryLock(
                    ManagedLocalAiProcess.remaining(context.deadline).toNanos(), TimeUnit.NANOSECONDS);
            if (context.locked) {
                activeExecutionThread = Thread.currentThread();
            }
            return context.locked;
        }

        private AiResponse executeOwned(ExecutionContext context) throws Exception {
            if (shuttingDown) {
                return unavailable(context.request, "Managed local inference is shutting down.");
            }
            ManagedLocalAiSnapshot snapshot = service.inspect();
            if (retireIfShuttingDown(context.deadline)) {
                return unavailable(context.request, "Managed local inference is shutting down.");
            }
            snapshot = provisionIfNeeded(context, snapshot);
            if (retireIfShuttingDown(context.deadline)) {
                return unavailable(context.request, "Managed local inference is shutting down.");
            }
            if (snapshot.state() != ManagedLocalAiSnapshot.State.READY) {
                closeSession(context.deadline);
                return unavailable(context.request, snapshot.action());
            }
            ManagedLocalAiService.ReadyRuntime runtime = service.readyRuntime();
            if (retireIfShuttingDown(context.deadline)) {
                return unavailable(context.request, "Managed local inference is shutting down.");
            }
            ensureSession(runtime, context.deadline);
            if (retireIfShuttingDown(context.deadline)) {
                return unavailable(context.request, "Managed local inference is shutting down.");
            }
            return infer(context);
        }

        private ManagedLocalAiSnapshot provisionIfNeeded(ExecutionContext context, ManagedLocalAiSnapshot snapshot)
                throws Exception {
            if (snapshot.state() != ManagedLocalAiSnapshot.State.NOT_PROVISIONED
                    || !snapshot.transparentProvisioning()) {
                return snapshot;
            }
            context.provisioning = service.provision(ignored -> { });
            activeProvisioning = context.provisioning;
            return context.provisioning.completion().get(
                    ManagedLocalAiProcess.remaining(context.deadline).toNanos(), TimeUnit.NANOSECONDS);
        }

        private void ensureSession(ManagedLocalAiService.ReadyRuntime runtime, long deadline) throws Exception {
            if (session != null && session.process().isAlive()
                    && session.matches(runtime.executable(), runtime.model(), runtime.alias(), runtime.threads())) {
                return;
            }
            closeSession(deadline);
            Duration launchTimeout = min(runtime.launchTimeout(), ManagedLocalAiProcess.remaining(deadline));
            session = runtimeClient.launch(runtime, launchTimeout, () -> shuttingDown);
        }

        private AiResponse infer(ExecutionContext context) throws Exception {
            AiResponse response = runtimeClient.infer(session, context.request, context.deadline);
            if (!session.process().isAlive() || session.resourceFailure() != null) {
                closeSession(context.deadline);
            }
            return response;
        }

        private void release(ExecutionContext context) {
            if (!context.locked) {
                return;
            }
            activeProvisioning = null;
            activeExecutionThread = null;
            executionLock.unlock();
        }

        private static AiResponse timedOut(AiRequest request) {
            return AiResponse.failure(AiResponseStatus.TIMEOUT, "managed-local", "",
                    "Managed local inference timed out.", Duration.ZERO, request.deterministicFallback());
        }

        private static void cancel(ManagedLocalAiOperation operation) {
            if (operation != null) {
                operation.cancel();
            }
        }

        private static Duration min(Duration left, Duration right) {
            return left.compareTo(right) <= 0 ? left : right;
        }

        private static AiResponse unavailable(AiRequest request, String reason) {
            return AiResponse.failure(AiResponseStatus.PROVIDER_UNAVAILABLE, "managed-local", "", reason,
                    Duration.ZERO, request.deterministicFallback());
        }

        private void closeSession(boolean locked, long deadline, Throwable primary) {
            if (locked) {
                closeSession(deadline, primary);
            }
        }

        private void closeSession(long deadline) {
            closeSession(deadline, null);
        }

        private void closeSession(long deadline, Throwable primary) {
            ManagedLocalAiProcess.Session closing = session;
            session = null;
            if (closing != null) {
                long nanos = Math.max(0, deadline - System.nanoTime());
                try {
                    closing.close(min(closing.shutdownTimeout(), Duration.ofNanos(nanos)), primary);
                } finally {
                    if (closing.hasSurvivors() && session == null) {
                        session = closing;
                    }
                }
            }
        }

        private boolean retireIfShuttingDown(long deadline) {
            if (!shuttingDown) {
                return false;
            }
            closeSession(deadline);
            ManagedLocalAiProcess.terminateRetainedLaunches();
            return true;
        }

        private void retireUnavailableSession() {
            boolean locked = executionLock.tryLock();
            if (locked) {
                try {
                    closeSession(System.nanoTime() + Duration.ofSeconds(2).toNanos());
                } finally {
                    executionLock.unlock();
                }
            }
        }

        void shutdown() {
            shuttingDown = true;
            cancel(activeProvisioning);
            Thread executing = activeExecutionThread;
            if (executing != null) {
                executing.interrupt();
            }
            boolean locked = false;
            try {
                locked = executionLock.tryLock(2, TimeUnit.SECONDS);
                if (!locked) {
                    ManagedLocalAiProcess.Session closing = session;
                    Throwable failure = new IllegalStateException(
                            "Managed local AI shutdown could not acquire lifecycle ownership.");
                    if (closing != null && closing.forceKillAndAwait(Duration.ofSeconds(2), failure)) {
                        throw new IllegalStateException(
                                "Managed local AI process tree survived forced shutdown.", failure);
                    }
                    ManagedLocalAiProcess.terminateRetainedLaunches();
                    return;
                }
                ManagedLocalAiProcess.Session closing = session;
                session = null;
                if (closing != null) {
                    closing.close();
                }
                ManagedLocalAiProcess.terminateRetainedLaunches();
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
            } finally {
                if (locked) {
                    executionLock.unlock();
                }
            }
        }

        private static final class ExecutionContext {
            private final AiRequest request;
            private final long deadline;
            private ManagedLocalAiOperation provisioning;
            private boolean locked;

            private ExecutionContext(AiRequest request) {
                this.request = request;
                this.deadline = System.nanoTime() + request.timeout().toNanos();
            }
        }
    }
}
