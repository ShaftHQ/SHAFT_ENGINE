package com.shaft.tools.internal.support;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class CreateVirtualThread {
    private final ExecutorService service;
    public CreateVirtualThread() {
        service = Executors.newVirtualThreadPerTaskExecutor();
    }
    public void runThreadWithTask(Runnable task){
        service.submit(task);
    }
    public void stopThreadNow(){
        service.shutdownNow();
    }
    public void stopThreadAfterTimeout(int timeout) throws InterruptedException {
        service.shutdown();
        service.awaitTermination(timeout, TimeUnit.SECONDS);
    }
}
