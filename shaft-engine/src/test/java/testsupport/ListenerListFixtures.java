package testsupport;

import com.shaft.listeners.TestNGListener;
import org.testng.ITestListener;

import java.util.ArrayList;
import java.util.List;

/**
 * Test fixtures that pass null into APIs NullAway treats as non-null. The helpers live outside
 * {@code com.shaft}, which is the only annotated package.
 */
public final class ListenerListFixtures {
    private ListenerListFixtures() {
    }

    /**
     * Returns a three-element list with a null entry between the two listeners.
     *
     * @param before listener placed before the null entry
     * @param after listener placed after the null entry
     * @return the three-element list
     */
    public static List<ITestListener> listWithNullElement(ITestListener before, ITestListener after) {
        List<ITestListener> listeners = new ArrayList<>();
        listeners.add(before);
        listeners.add(null);
        listeners.add(after);
        return listeners;
    }

    /**
     * Clears the result bound by {@link TestNGListener#bindITestResult(ITestResult)}.
     */
    public static void clearBoundTestResult() {
        TestNGListener.bindITestResult(null);
    }
}
