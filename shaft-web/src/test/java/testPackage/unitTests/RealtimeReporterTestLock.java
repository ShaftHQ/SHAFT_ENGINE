package testPackage.unitTests;

import java.util.concurrent.locks.ReentrantLock;

final class RealtimeReporterTestLock {
    static final ReentrantLock LOCK = new ReentrantLock();

    private RealtimeReporterTestLock() {
        throw new IllegalStateException("Utility class");
    }
}
