package io.github.shafthq.SHAFT_MCP;

import org.springframework.ai.support.ToolCallbacks;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

import java.io.IOException;
import java.util.List;

@SpringBootApplication
public class ShaftMcpApplication {

    /**
     * The main entry point for the ShaftMcpApplication.
     * @param args command-line arguments
     */
	public static void main(String[] args) throws IOException {
        new SpringApplication(ShaftMcpApplication.class).run(args);
	}

    /**
     * Registers the ShaftService tool callbacks.
     * @param engineService the ShaftService instance
     * @return a list of ToolCallback instances
     */
	@Bean
	public List<ToolCallback> shaftTools(EngineService engineService, BrowserService browserService, ElementService elementService) {
        var engineServiceList = List.of(ToolCallbacks.from(engineService));
        var browserServiceList = List.of(ToolCallbacks.from(browserService));
        var elementServiceList = List.of(ToolCallbacks.from(elementService));

        var serviceList = new java.util.ArrayList<ToolCallback>();
        serviceList.addAll(engineServiceList);
        serviceList.addAll(browserServiceList);
        serviceList.addAll(elementServiceList);
        return serviceList;
	}
}
