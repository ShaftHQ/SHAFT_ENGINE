package testPackage.unitTests;

import com.shaft.api.GrpcActions;
import com.shaft.driver.SHAFT;
import io.grpc.ManagedChannel;
import io.grpc.Server;
import io.grpc.inprocess.InProcessChannelBuilder;
import io.grpc.inprocess.InProcessServerBuilder;
import io.grpc.protobuf.services.HealthStatusManager;
import io.grpc.protobuf.services.ProtoReflectionServiceV1;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

/**
 * Stub-free unary gRPC calls through server reflection (#6406).
 */
public class GrpcActionsTest {
    private Server server;
    private ManagedChannel channel;

    /**
     * Starts an in-process server with health checking and reflection.
     *
     * @throws Exception when the server cannot start
     */
    @BeforeClass
    public void startServer() throws Exception {
        String name = InProcessServerBuilder.generateName();
        server = InProcessServerBuilder.forName(name).directExecutor()
                .addService(new HealthStatusManager().getHealthService())
                .addService(ProtoReflectionServiceV1.newInstance())
                .build().start();
        channel = InProcessChannelBuilder.forName(name).directExecutor().build();
    }

    /**
     * A unary call returns JSON that the existing assertions can check.
     */
    @Test
    public void unaryCallReturnsJsonForAssertions() {
        GrpcActions.Response response = SHAFT.API.grpc(channel).unary("grpc.health.v1.Health/Check", "{\"service\":\"\"}");
        Assert.assertTrue(response.isOk(), response.toString());
        SHAFT.Validations.assertThat().object(response.json("$.status")).isEqualTo("SERVING").perform();
    }

    /**
     * Error statuses come back as codes with descriptions instead of exceptions.
     */
    @Test
    public void errorStatusIsReported() {
        GrpcActions.Response response = SHAFT.API.grpc(channel).unary("grpc.health.v1.Health/Check", "{\"service\":\"missing\"}");
        Assert.assertEquals(response.statusCode(), "NOT_FOUND");
        Assert.assertFalse(response.isOk());
        Assert.assertNotNull(response.description());
    }

    /**
     * A descriptor set replaces reflection, and the deadline is configurable.
     *
     * @throws Exception when the descriptor set cannot be written
     */
    @Test
    public void descriptorSetWorksWithoutReflection() throws Exception {
        java.nio.file.Path set = java.nio.file.Files.createTempFile("health", ".desc");
        com.google.protobuf.DescriptorProtos.FileDescriptorSet.newBuilder()
                .addFile(io.grpc.health.v1.HealthProto.getDescriptor().toProto()).build()
                .writeTo(java.nio.file.Files.newOutputStream(set));
        GrpcActions.Response response = new GrpcActions(channel).withDescriptorSet(set).withTimeout(5)
                .unary("grpc.health.v1.Health/Check", "{}");
        Assert.assertEquals(response.json("$.status"), "SERVING");
    }

    /**
     * A target string opens its own channel; an unreachable target fails and the channel closes.
     */
    @Test
    public void targetChannelFailsWhenUnreachableAndCloses() {
        GrpcActions grpc = new GrpcActions("localhost:1").withTimeout(2);
        try {
            Assert.expectThrows(RuntimeException.class, () -> grpc.unary("grpc.health.v1.Health/Check", "{}"));
        } finally {
            grpc.close();
        }
    }

    /**
     * Unknown methods fail with a clear message.
     */
    @Test
    public void unknownMethodFailsClearly() {
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class,
                () -> SHAFT.API.grpc(channel).unary("grpc.health.v1.Health/Nope", "{}"));
        Assert.assertTrue(error.getMessage().contains("Nope"), error.getMessage());
    }

    /**
     * Stops the server and channel.
     */
    @AfterClass(alwaysRun = true)
    public void stopServer() {
        channel.shutdownNow();
        server.shutdownNow();
    }
}
