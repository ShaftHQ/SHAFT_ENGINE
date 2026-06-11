package example;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.shaft.pilot.ai.AiRequest;

/**
 * Compile-only consumer proving feature modules need only Pilot contracts.
 */
public final class PilotCoreConsumer {
    private PilotCoreConsumer() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Creates a deterministic contract request without loading direct providers.
     *
     * @return request
     */
    public static AiRequest request() {
        return AiRequest.builder("consumer-fixture", JsonNodeFactory.instance.objectNode()).build();
    }
}
