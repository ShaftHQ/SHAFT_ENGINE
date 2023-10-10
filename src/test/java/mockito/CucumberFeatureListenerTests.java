package mockito;

import com.shaft.listeners.CucumberFeatureListener;
import io.cucumber.plugin.event.EventPublisher;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;

public class CucumberFeatureListenerTests {
    // mock creation
    CucumberFeatureListener mockedCucumberFeatureListener = mock();
    EventPublisher publisher = mock();

    @Test
    public void doesNothing() {
        mockedCucumberFeatureListener.setEventPublisher(publisher);
    }
}
