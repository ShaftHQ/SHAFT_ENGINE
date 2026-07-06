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
}
