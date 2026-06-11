package io.github.shafthq.SHAFT_MCP;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
class ShaftMcpApplicationTests {
	SHAFT.GUI.WebDriver driver;
	@Test
	void contextLoads() {
		driver.browser().navigateToURL("https://shafthq.github.io/")
				.element().assertThat(By.tagName("h1")).text().contains("SHAFT");
	}
	@BeforeEach
	void setUp() {
		driver = new SHAFT.GUI.WebDriver();
	}
	@AfterEach
	void tearDown() {
		if (driver != null) {
			driver.quit();
		}
	}
}
