package com.shaft.heal.internal;

import com.shaft.heal.model.HealingCandidate;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;

record RankedCandidate(WebElement element, By locator, HealingCandidate report) {
}
