package com.shaft.gui.element;

import java.awt.Point;
import java.time.Duration;

import org.openqa.selenium.Dimension;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.interactions.Pause;
import org.openqa.selenium.interactions.PointerInput;
import org.openqa.selenium.interactions.Sequence;

import com.google.common.collect.ImmutableList;
import com.shaft.gui.element.TouchActions.SwipeDirection;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;

public class TouchActionsHelper {
	/**
	 * Credit to:
	 * https://www.youtube.com/watch?v=lOoBodZMpA0
	 * https://github.com/sunilpatro1985/AppiumTest_Java_And_iOS/blob/388e05d0033f4a75d8dd820f9d6d518310fd4c3e/src/main/java/base/Util.java#L115
	 */

	private static Dimension windowSize;
	private static Duration SCROLL_DUR = Duration.ofMillis(1000);
	private static int ANDROID_SCROLL_DIVISOR = 3;

	private static Dimension getWindowSize(WebDriver driver) {
		if (windowSize == null) {
			windowSize = driver.manage().window().getSize();
		}
		return windowSize;
	}

	protected static void scroll(WebDriver driver, SwipeDirection dir, double distance) {
		if (distance < 0 || distance > 1) {
			throw new Error("Scroll distance must be between 0 and 1");
		}
		Dimension size = getWindowSize(driver);
		Point midPoint = new Point((int) (size.width * 0.5), (int) (size.height * 0.5));
		int top = midPoint.y - (int) ((size.height * distance) * 0.5);
		int bottom = midPoint.y + (int) ((size.height * distance) * 0.5);
		int left = midPoint.x - (int) ((size.width * distance) * 0.5);
		int right = midPoint.x + (int) ((size.width * distance) * 0.5);
		if (dir == SwipeDirection.UP) {
			swipe(driver, new Point(midPoint.x, top), new Point(midPoint.x, bottom), SCROLL_DUR);
		} else if (dir == SwipeDirection.DOWN) {
			swipe(driver, new Point(midPoint.x, bottom), new Point(midPoint.x, top), SCROLL_DUR);
		} else if (dir == SwipeDirection.LEFT) {
			swipe(driver, new Point(left, midPoint.y), new Point(right, midPoint.y), SCROLL_DUR);
		} else {
			swipe(driver, new Point(right, midPoint.y), new Point(left, midPoint.y), SCROLL_DUR);
		}
	}

	private static void swipe(WebDriver driver, Point start, Point end, Duration duration) {
		boolean isAndroid = driver instanceof AndroidDriver<?>;

		PointerInput input = new PointerInput(PointerInput.Kind.TOUCH, "finger1");
		Sequence swipe = new Sequence(input, 0);
		swipe.addAction(input.createPointerMove(Duration.ZERO, PointerInput.Origin.viewport(), start.x, start.y));
		swipe.addAction(input.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
		if (isAndroid) {
			duration = duration.dividedBy(ANDROID_SCROLL_DIVISOR);
		} else {
			swipe.addAction(new Pause(input, duration));
			duration = Duration.ZERO;
		}
		swipe.addAction(input.createPointerMove(duration, PointerInput.Origin.viewport(), end.x, end.y));
		swipe.addAction(input.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
		((AppiumDriver<?>) driver).perform(ImmutableList.of(swipe));
	}
}
