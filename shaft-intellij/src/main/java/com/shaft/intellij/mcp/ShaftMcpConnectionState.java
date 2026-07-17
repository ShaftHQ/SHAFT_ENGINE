package com.shaft.intellij.mcp;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Tracks the MCP connection state and notifies listeners of state changes.
 *
 * <p>Starts {@link State#UNKNOWN} rather than assuming a connection is live (issue #3624):
 * asserting CONNECTED before any real verification let a broken MCP command look healthy for up
 * to one heartbeat interval after the panel opened.
 */
public final class ShaftMcpConnectionState {
    /**
     * Three-state connection model (issue #3624). {@link #UNKNOWN} is the honest initial state
     * before the first heartbeat probe completes; only {@link #CONNECTED} and {@link #DISCONNECTED}
     * result from an actual verification.
     */
    public enum State {
        UNKNOWN,
        CONNECTED,
        DISCONNECTED
    }

    private final AtomicReference<State> state = new AtomicReference<>(State.UNKNOWN);
    private final List<Runnable> stateChangeListeners = new ArrayList<>();

    /**
     * Returns the current three-state connection state.
     *
     * @return current state
     */
    public State state() {
        return state.get();
    }

    /**
     * Returns true only once a probe has actually verified the connection is live. Returns false
     * for both {@link State#UNKNOWN} and {@link State#DISCONNECTED} -- source-compatible with
     * every caller written against the old boolean contract.
     *
     * @return true if connected
     */
    public boolean isConnected() {
        return state.get() == State.CONNECTED;
    }

    /**
     * Returns true before any probe has run or completed.
     *
     * @return true if no verification has happened yet
     */
    public boolean isUnknown() {
        return state.get() == State.UNKNOWN;
    }

    /**
     * Records a verified connection result.
     *
     * @param isConnected true to mark as connected, false to mark as disconnected
     */
    public void setConnected(boolean isConnected) {
        State newState = isConnected ? State.CONNECTED : State.DISCONNECTED;
        State previous = state.getAndSet(newState);
        if (previous != newState) {
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
