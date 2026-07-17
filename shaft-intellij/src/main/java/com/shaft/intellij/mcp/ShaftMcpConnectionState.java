package com.shaft.intellij.mcp;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Tracks the MCP connection state and notifies listeners of state changes.
 */
public final class ShaftMcpConnectionState {
    private final AtomicBoolean connected = new AtomicBoolean(true);
    private final List<Runnable> stateChangeListeners = new ArrayList<>();

    /**
     * Returns true if the MCP connection is active.
     *
     * @return true if connected
     */
    public boolean isConnected() {
        return connected.get();
    }

    /**
     * Sets the connection state.
     *
     * @param isConnected true to mark as connected, false to mark as disconnected
     */
    public void setConnected(boolean isConnected) {
        boolean changed = connected.getAndSet(isConnected) != isConnected;
        if (changed) {
            notifyListeners();
        }
    }

    /**
     * Registers a listener to be notified when the connection state changes.
     *
     * @param listener callback to invoke on state change
     */
    public synchronized void addStateChangeListener(Runnable listener) {
        if (listener != null) {
            stateChangeListeners.add(listener);
        }
    }

    /**
     * Removes a previously registered state change listener.
     *
     * <p><b>Caution:</b> this is a plain {@code List.remove(Object)}, matched by {@code equals()}/
     * identity. The caller MUST pass the exact same {@code Runnable} reference that was registered
     * via {@link #addStateChangeListener}. A freshly evaluated method-reference or lambda
     * expression (e.g. writing {@code this::onStateChanged} again at the removal call site) is a
     * distinct object and is not guaranteed to be identity-equal to the one passed at registration
     * time, so it silently fails to remove anything -- no exception, no log. Capture the listener
     * once in a field and reuse that same reference for both add and remove (issue #3621).
     *
     * @param listener listener to remove
     */
    public synchronized void removeStateChangeListener(Runnable listener) {
        stateChangeListeners.remove(listener);
    }

    private synchronized void notifyListeners() {
        for (Runnable listener : new ArrayList<>(stateChangeListeners)) {
            listener.run();
        }
    }

    /**
     * Test-support accessor for the number of currently registered state-change listeners. Not
     * used by production code; exists so tests (e.g. for issue #3621) can assert that
     * addStateChangeListener/removeStateChangeListener registration stays balanced across a
     * component's lifecycle instead of leaking.
     *
     * @return number of currently registered listeners
     */
    public synchronized int listenerCount() {
        return stateChangeListeners.size();
    }
}
