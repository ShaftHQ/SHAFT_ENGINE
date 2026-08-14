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
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

/** SHAFT-owned provider entrypoint for managed local inference. */
public final class ManagedLocalAiProvider implements AiProvider {
    private static final ServiceLifecycle SERVICE_LIFECYCLE = new ServiceLifecycle(new ManagedLocalAiService());
    static {
        Runtime.getRuntime().addShutdownHook(Thread.ofPlatform().name("shaft-managed-local-ai-shutdown")
                .unstarted(SERVICE_LIFECYCLE::closeSession));
    }
    private final Lifecycle lifecycle;

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

    private static final class ServiceLifecycle implements Lifecycle {
        private final ManagedLocalAiService service;
        private final ReentrantLock executionLock = new ReentrantLock();
        private volatile ManagedLocalAiProcess.Session session;
        private volatile ManagedLocalAiOperation activeProvisioning;
        private volatile Thread activeExecutionThread;
        private volatile boolean shuttingDown;

        private ServiceLifecycle(ManagedLocalAiService service) {
            this.service = Objects.requireNonNull(service, "service");
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
            long deadline = System.nanoTime() + request.timeout().toNanos();
            ManagedLocalAiOperation provisioningOperation = null;
            boolean locked = false;
            try {
                locked = executionLock.tryLock(ManagedLocalAiProcess.remaining(deadline).toNanos(),
                        TimeUnit.NANOSECONDS);
                if (!locked) {
                    return timedOut(request);
                }
                activeExecutionThread = Thread.currentThread();
                if (shuttingDown) {
                    return unavailable(request, "Managed local inference is shutting down.");
                }
                ManagedLocalAiSnapshot snapshot = service.inspect();
                if (retireIfShuttingDown(deadline)) {
                    return unavailable(request, "Managed local inference is shutting down.");
                }
                if (snapshot.state() == ManagedLocalAiSnapshot.State.NOT_PROVISIONED
                        && snapshot.transparentProvisioning()) {
                    provisioningOperation = service.provision(ignored -> { });
                    activeProvisioning = provisioningOperation;
                    snapshot = provisioningOperation.completion()
                            .get(ManagedLocalAiProcess.remaining(deadline).toNanos(),
                                    TimeUnit.NANOSECONDS);
                    if (retireIfShuttingDown(deadline)) {
                        return unavailable(request, "Managed local inference is shutting down.");
                    }
                }
                if (snapshot.state() != ManagedLocalAiSnapshot.State.READY) {
                    closeSession(deadline);
                    return unavailable(request, snapshot.action());
                }
                ManagedLocalAiService.ReadyRuntime runtime = service.readyRuntime();
                if (retireIfShuttingDown(deadline)) {
                    return unavailable(request, "Managed local inference is shutting down.");
                }
                if (session == null || !session.process().isAlive()
                        || !session.matches(runtime.executable(), runtime.model(), runtime.alias(), runtime.threads())) {
                    closeSession(deadline);
                    Duration launchTimeout = min(runtime.launchTimeout(), ManagedLocalAiProcess.remaining(deadline));
                    session = ManagedLocalAiProcess.launch(runtime.cache(), runtime.executable(), runtime.model(),
                            runtime.log(), runtime.alias(), runtime.threads(), launchTimeout, () -> shuttingDown,
                            ManagedLocalAiProcess::start,
                            (process, port, key, alias, timeout) -> ManagedLocalAiProcess.requireIdentity(
                                    process, port, key, alias, timeout, ManagedLocalAiProcess::requestIdentity));
                    if (retireIfShuttingDown(deadline)) {
                        return unavailable(request, "Managed local inference is shutting down.");
                    }
                }
                AiResponse response = ManagedLocalAiProcess.infer(session, request,
                        ManagedLocalAiProcess::requestInference, deadline);
                if (!session.process().isAlive()) {
                    closeSession(deadline);
                }
                return response;
            } catch (java.util.concurrent.TimeoutException timeout) {
                cancel(provisioningOperation);
                closeSession(locked, deadline, timeout);
                return timedOut(request);
            } catch (ManagedLocalAiProcess.DeadlineExceededException timeout) {
                cancel(provisioningOperation);
                closeSession(locked, deadline, timeout);
                return timedOut(request);
            } catch (InterruptedException interrupted) {
                cancel(provisioningOperation);
                closeSession(locked, deadline, interrupted);
                Thread.currentThread().interrupt();
                return AiResponse.failure(AiResponseStatus.ERROR, "managed-local", "",
                        "Managed local inference was interrupted.", Duration.ZERO,
                        request.deterministicFallback());
            } catch (Exception failure) {
                closeSession(locked, deadline, failure);
                return unavailable(request, "Managed local inference is unavailable.");
            } finally {
                if (locked) {
                    activeProvisioning = null;
                    activeExecutionThread = null;
                    executionLock.unlock();
                }
            }
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

        private void closeSession() {
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
    }
}
