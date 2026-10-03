package com.shaft.api;

import com.google.protobuf.DescriptorProtos.FileDescriptorProto;
import com.google.protobuf.DescriptorProtos.FileDescriptorSet;
import com.google.protobuf.Descriptors.Descriptor;
import com.google.protobuf.Descriptors.FileDescriptor;
import com.google.protobuf.Descriptors.MethodDescriptor;
import com.google.protobuf.Descriptors.ServiceDescriptor;
import com.google.protobuf.DynamicMessage;
import com.google.protobuf.util.JsonFormat;
import io.grpc.CallOptions;
import io.grpc.Channel;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.Status;
import io.grpc.StatusRuntimeException;
import io.grpc.protobuf.ProtoUtils;
import io.grpc.reflection.v1.ServerReflectionGrpc;
import io.grpc.reflection.v1.ServerReflectionRequest;
import io.grpc.reflection.v1.ServerReflectionResponse;
import io.grpc.stub.ClientCalls;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.qameta.allure.Allure;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * Unary gRPC calls without generated stubs (#6406). Message types come from a descriptor set or server reflection;
 * requests and responses are JSON, so results work with {@link RestActions#getResponseJSONValue(Object, String)} and
 * {@code SHAFT.Validations}. Each call is an Allure step with the request, response and status code.
 *
 * <pre>{@code
 * GrpcActions.Response response = SHAFT.API.grpc("localhost:50051")
 *         .unary("grpc.health.v1.Health/Check", "{\"service\":\"\"}");
 * SHAFT.Validations.assertThat().object(response.json("$.status")).isEqualTo("SERVING").perform();
 * }</pre>
 */
public class GrpcActions {
    private final Channel channel;
    private final ManagedChannel ownedChannel;
    private final Map<String, FileDescriptor> files = new HashMap<>();
    private long timeoutSeconds = 30;

    /**
     * Opens a plaintext channel to {@code host:port} (or any gRPC target string).
     *
     * @param target gRPC target
     */
    public GrpcActions(String target) {
        ownedChannel = ManagedChannelBuilder.forTarget(target).usePlaintext().build();
        channel = ownedChannel;
    }

    /**
     * Uses a caller-managed channel, for example TLS or in-process.
     *
     * @param channel the channel
     */
    public GrpcActions(Channel channel) {
        this.channel = channel;
        ownedChannel = null;
    }

    /**
     * Loads message types from a {@code protoc --descriptor_set_out --include_imports} file instead of reflection.
     *
     * @param descriptorSet path to the descriptor set
     * @return this
     */
    public GrpcActions withDescriptorSet(Path descriptorSet) {
        try (InputStream in = Files.newInputStream(descriptorSet)) {
            addFiles(FileDescriptorSet.parseFrom(in).getFileList());
            return this;
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot read gRPC descriptor set " + descriptorSet, e);
        }
    }

    /**
     * Sets the per-call deadline.
     *
     * @param seconds deadline in seconds
     * @return this
     */
    public GrpcActions withTimeout(long seconds) {
        timeoutSeconds = seconds;
        return this;
    }

    /**
     * Performs a unary call; non-OK statuses are returned, not thrown.
     *
     * @param fullMethodName {@code package.Service/Method}
     * @param jsonRequest    request message as JSON
     * @return the status and JSON response
     */
    public Response unary(String fullMethodName, String jsonRequest) {
        return Allure.step("gRPC " + fullMethodName, step -> {
            ReportManagerHelper.attach("JSON", "gRPC request", jsonRequest);
            MethodDescriptor method = method(fullMethodName);
            DynamicMessage.Builder request = DynamicMessage.newBuilder(method.getInputType());
            JsonFormat.parser().merge(jsonRequest, request);
            Response response;
            try {
                DynamicMessage reply = ClientCalls.blockingUnaryCall(channel, grpcMethod(method),
                        CallOptions.DEFAULT.withDeadlineAfter(timeoutSeconds, TimeUnit.SECONDS), request.build());
                response = new Response(Status.Code.OK.name(), "", JsonFormat.printer().print(reply));
            } catch (StatusRuntimeException e) {
                response = new Response(e.getStatus().getCode().name(),
                        e.getStatus().getDescription() == null ? "" : e.getStatus().getDescription(), "{}");
            }
            step.parameter("status", response.statusCode());
            ReportManagerHelper.attach("JSON", "gRPC response " + response.statusCode(),
                    response.isOk() ? response.json() : response.statusCode() + ": " + response.description());
            return response;
        });
    }

    /**
     * Shuts down the channel this instance opened; caller-managed channels are left alone.
     */
    public void close() {
        if (ownedChannel != null) {
            ownedChannel.shutdownNow();
        }
    }

    private MethodDescriptor method(String fullMethodName) throws Exception {
        int slash = fullMethodName.lastIndexOf('/');
        if (slash < 1) {
            throw new IllegalArgumentException("Expected package.Service/Method but got " + fullMethodName);
        }
        String serviceName = fullMethodName.substring(0, slash);
        ServiceDescriptor service = findService(serviceName);
        if (service == null) {
            addFiles(reflect(serviceName));
            service = findService(serviceName);
        }
        MethodDescriptor method = service == null ? null : service.findMethodByName(fullMethodName.substring(slash + 1));
        if (method == null) {
            throw new IllegalArgumentException("gRPC method not found: " + fullMethodName);
        }
        return method;
    }

    private ServiceDescriptor findService(String serviceName) {
        for (FileDescriptor file : files.values()) {
            ServiceDescriptor service = file.findServiceByName(serviceName.substring(serviceName.lastIndexOf('.') + 1));
            if (service != null && service.getFullName().equals(serviceName)) {
                return service;
            }
        }
        return null;
    }

    private List<FileDescriptorProto> reflect(String symbol) throws Exception {
        var call = ServerReflectionGrpc.newBlockingV2Stub(channel).withDeadlineAfter(timeoutSeconds, TimeUnit.SECONDS)
                .serverReflectionInfo();
        ServerReflectionResponse response;
        try {
            call.write(ServerReflectionRequest.newBuilder().setFileContainingSymbol(symbol).build());
            call.halfClose();
            response = call.read();
        } catch (io.grpc.StatusException e) {
            throw new IllegalStateException("Server reflection failed for " + symbol + ": " + e.getStatus(), e);
        } finally {
            call.cancel("done", null);
        }
        if (response.hasErrorResponse()) {
            throw new IllegalArgumentException("Server reflection cannot resolve " + symbol + ": " + response.getErrorResponse().getErrorMessage());
        }
        List<FileDescriptorProto> protos = new ArrayList<>();
        for (var bytes : response.getFileDescriptorResponse().getFileDescriptorProtoList()) {
            protos.add(FileDescriptorProto.parseFrom(bytes));
        }
        return protos;
    }

    private void addFiles(List<FileDescriptorProto> protos) {
        Map<String, FileDescriptorProto> byName = new HashMap<>();
        protos.forEach(proto -> byName.put(proto.getName(), proto));
        protos.forEach(proto -> build(proto.getName(), byName));
    }

    private FileDescriptor build(String name, Map<String, FileDescriptorProto> byName) {
        FileDescriptor known = files.get(name);
        if (known != null) {
            return known;
        }
        FileDescriptorProto proto = byName.get(name);
        if (proto == null) {
            throw new IllegalArgumentException("gRPC descriptor dependency missing: " + name);
        }
        List<FileDescriptor> dependencies = new ArrayList<>();
        proto.getDependencyList().forEach(dependency -> dependencies.add(build(dependency, byName)));
        try {
            FileDescriptor file = FileDescriptor.buildFrom(proto, dependencies.toArray(FileDescriptor[]::new));
            files.put(name, file);
            return file;
        } catch (com.google.protobuf.Descriptors.DescriptorValidationException e) {
            throw new IllegalArgumentException("Invalid gRPC descriptor " + name, e);
        }
    }

    private static io.grpc.MethodDescriptor<DynamicMessage, DynamicMessage> grpcMethod(MethodDescriptor method) {
        Descriptor input = method.getInputType();
        Descriptor output = method.getOutputType();
        return io.grpc.MethodDescriptor.<DynamicMessage, DynamicMessage>newBuilder()
                .setType(io.grpc.MethodDescriptor.MethodType.UNARY)
                .setFullMethodName(io.grpc.MethodDescriptor.generateFullMethodName(method.getService().getFullName(), method.getName()))
                .setRequestMarshaller(ProtoUtils.marshaller(DynamicMessage.getDefaultInstance(input)))
                .setResponseMarshaller(ProtoUtils.marshaller(DynamicMessage.getDefaultInstance(output)))
                .build();
    }

    /**
     * Result of a unary call.
     *
     * @param statusCode  gRPC status code name, for example {@code OK} or {@code NOT_FOUND}
     * @param description status description, empty when OK
     * @param json        response message as JSON, {@code {}} when not OK
     */
    public record Response(String statusCode, String description, String json) {
        /**
         * Reports whether the call returned {@code OK}.
         *
         * @return {@code true} when OK
         */
        public boolean isOk() {
            return Status.Code.OK.name().equals(statusCode);
        }

        /**
         * Extracts a value from the JSON response.
         *
         * @param jsonPath JSONPath, for example {@code $.status}
         * @return the value, or {@code null}
         */
        public String json(String jsonPath) {
            return RestActions.getResponseJSONValue((Object) json, jsonPath);
        }
    }
}
