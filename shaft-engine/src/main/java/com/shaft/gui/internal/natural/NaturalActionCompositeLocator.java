package com.shaft.gui.internal.natural;

import com.shaft.gui.internal.locator.CompositeLocator;
import org.openqa.selenium.By;
import org.openqa.selenium.support.pagefactory.ByAll;

import java.util.Arrays;
import java.util.List;

final class NaturalActionCompositeLocator extends ByAll implements CompositeLocator {
    private final List<By> alternatives;

    NaturalActionCompositeLocator(By... locators) {
        super(locators);
        this.alternatives = List.copyOf(Arrays.asList(locators));
    }

    @Override
    public List<By> alternatives() {
        return alternatives;
    }
}
