package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.assertions.LocatorAssertions;
import com.microsoft.playwright.assertions.PlaywrightAssertions;
import com.microsoft.playwright.options.BoundingBox;
import com.shaft.gui.driver.ElementAssertions;
import com.shaft.gui.driver.ElementRectangle;
import com.shaft.gui.driver.ElementTarget;
import com.shaft.gui.driver.ShaftLocator;
import com.shaft.gui.playwright.element.ElementActions;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.VisualProcessingProvider;
import com.shaft.driver.SHAFT;
import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.ValidationsHelper;
import com.shaft.validation.ValidationEnums;
import org.mockito.MockedStatic;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;
import org.testng.annotations.AfterMethod;

import java.util.concurrent.atomic.AtomicReference;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import java.lang.reflect.Field;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

@SuppressWarnings("PMD.AvoidAccessibilityAlteration")
public class PlaywrightElementValidationTargetTest {
    @AfterMethod(alwaysRun = true)
    public void resetVerificationState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test
    public void everyPortableStarterShouldResolveAgainstThePageCurrentAtTerminalExecution() {
        assertLazyTarget(starter -> new PlaywrightDriverAssertions(starter.session()).element(starter.target()));
        assertLazyTarget(starter -> new PlaywrightDriverVerifications(starter.session()).element(starter.target()));
        assertLazyTarget(starter -> new ElementActions(starter.session()).assertThat(starter.target()));
        assertLazyTarget(starter -> new ElementActions(starter.session()).verifyThat(starter.target()));
    }

    @Test
    public void everyExistingPortableStarterShouldResolveItsExactLocatorAgainstTheCurrentPage() {
        assertLazyExistingStarter("#save", ValidationEnums.ValidationCategory.HARD_ASSERT,
                starter -> new PlaywrightDriverAssertions(starter.session()).element(starter.shaftLocator()));
        assertLazyExistingStarter("[id=\"save\"]", ValidationEnums.ValidationCategory.HARD_ASSERT,
                starter -> new PlaywrightDriverAssertions(starter.session()).element(starter.by()));
        assertLazyExistingStarter("#save", ValidationEnums.ValidationCategory.SOFT_ASSERT,
                starter -> new PlaywrightDriverVerifications(starter.session()).element(starter.shaftLocator()));
        assertLazyExistingStarter("[id=\"save\"]", ValidationEnums.ValidationCategory.SOFT_ASSERT,
                starter -> new PlaywrightDriverVerifications(starter.session()).element(starter.by()));
        assertLazyExistingStarter("#save", ValidationEnums.ValidationCategory.HARD_ASSERT,
                starter -> new ElementActions(starter.session()).assertThat(starter.shaftLocator()));
        assertLazyExistingStarter("[id=\"save\"]", ValidationEnums.ValidationCategory.HARD_ASSERT,
                starter -> new ElementActions(starter.session()).assertThat(starter.by()));
        assertLazyExistingStarter("#save", ValidationEnums.ValidationCategory.SOFT_ASSERT,
                starter -> new ElementActions(starter.session()).verifyThat(starter.shaftLocator()));
        assertLazyExistingStarter("[id=\"save\"]", ValidationEnums.ValidationCategory.SOFT_ASSERT,
                starter -> new ElementActions(starter.session()).verifyThat(starter.by()));
    }

    @Test
    public void everyNativeLocatorStarterShouldUseTheExactLocatorWithoutReadingThePage() throws Exception {
        assertNativeStarter(ValidationEnums.ValidationCategory.HARD_ASSERT,
                starter -> new PlaywrightDriverAssertions(starter.session()).element(starter.locator()));
        assertNativeStarter(ValidationEnums.ValidationCategory.SOFT_ASSERT,
                starter -> new PlaywrightDriverVerifications(starter.session()).element(starter.locator()));
        assertNativeStarter(ValidationEnums.ValidationCategory.HARD_ASSERT,
                starter -> new ElementActions(starter.session()).assertThat(starter.locator()));
        assertNativeStarter(ValidationEnums.ValidationCategory.SOFT_ASSERT,
                starter -> new ElementActions(starter.session()).verifyThat(starter.locator()));
    }

    @Test
    public void intermediateValueBuilderShouldResolveAgainstThePageCurrentAtComparison() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page initialPage = mock(Page.class);
        Page currentPage = mock(Page.class);
        Locator currentLocator = mock(Locator.class);
        LocatorAssertions locatorAssertions = mock(LocatorAssertions.class);
        AtomicReference<Page> selectedPage = new AtomicReference<>(initialPage);
        when(session.page()).thenAnswer(ignored -> selectedPage.get());
        when(currentPage.locator("#save")).thenReturn(currentLocator);
        when(currentPage.locator("[id=\"save\"]")).thenReturn(currentLocator);
        when(currentLocator.textContent()).thenReturn("Saved");
        ElementTarget target = ElementTarget.located(ShaftLocator.css("#save"));

        NativeValidationsBuilder text = new PlaywrightDriverAssertions(session).element(target).text();
        verify(initialPage, never()).locator(any(String.class));

        selectedPage.set(currentPage);
        try (MockedStatic<PlaywrightAssertions> playwrightAssertions = mockStatic(PlaywrightAssertions.class)) {
            playwrightAssertions.when(() -> PlaywrightAssertions.assertThat(currentLocator)).thenReturn(locatorAssertions);
            text.isEqualTo("Saved");
        }

        verify(initialPage, never()).locator(any(String.class));
        verify(currentPage).locator("#save");
        verify(locatorAssertions).hasText("Saved");
    }

    @Test
    public void everyPortableStarterShouldRejectNullBeforeProviderAccess() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Assert.expectThrows(NullPointerException.class,
                () -> new PlaywrightDriverAssertions(session).element((ElementTarget) null));
        Assert.expectThrows(NullPointerException.class,
                () -> new PlaywrightDriverVerifications(session).element((ElementTarget) null));
        Assert.expectThrows(NullPointerException.class,
                () -> new ElementActions(session).assertThat((ElementTarget) null));
        Assert.expectThrows(NullPointerException.class,
                () -> new ElementActions(session).verifyThat((ElementTarget) null));
        verify(session, never()).page();
    }

    @Test
    public void missingCurrentPageShouldFailClosedWithoutUsingThePreviousPage() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page initialPage = mock(Page.class);
        AtomicReference<Page> selectedPage = new AtomicReference<>(initialPage);
        when(session.page()).thenAnswer(ignored -> selectedPage.get());
        ElementAssertions assertions = new PlaywrightDriverAssertions(session)
                .element(ElementTarget.located(ShaftLocator.css("#save")));

        selectedPage.set(null);
        NullPointerException exception = Assert.expectThrows(NullPointerException.class, assertions::exists);
        Assert.assertEquals(exception.getMessage(), "page");
        verify(initialPage, never()).locator(any(String.class));
    }

    @Test
    public void closedCurrentPageShouldFailClosedBeforeLocatorResolution() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page closedPage = mock(Page.class);
        when(session.page()).thenReturn(closedPage);
        when(closedPage.isClosed()).thenReturn(true);
        ElementAssertions assertions = new PlaywrightDriverAssertions(session)
                .element(ElementTarget.located(ShaftLocator.css("#save")));

        IllegalStateException exception = Assert.expectThrows(IllegalStateException.class, assertions::exists);
        Assert.assertEquals(exception.getMessage(), "Playwright page is closed.");
        verify(closedPage, never()).locator(any(String.class));
    }

    @Test
    public void screenshotTerminalShouldResolveThePortableTargetAtInvocation() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page currentPage = mock(Page.class);
        Locator currentLocator = mock(Locator.class);
        when(session.page()).thenReturn(currentPage);
        when(currentPage.locator("#save")).thenReturn(currentLocator);
        when(currentLocator.screenshot()).thenReturn(new byte[]{1});
        ElementAssertions assertions = new PlaywrightDriverAssertions(session)
                .element(ElementTarget.located(ShaftLocator.css("#save")));

        try (MockedStatic<ImageProcessingActions> images = mockStatic(ImageProcessingActions.class)) {
            images.when(() -> ImageProcessingActions.getReferenceImage(anyString())).thenReturn(null);
            images.when(() -> ImageProcessingActions.compareScreenshotAgainstBaseline(
                            anyString(), any(byte[].class), any(), any(), any()))
                    .thenReturn(new VisualProcessingProvider.ScreenshotComparisonResult(
                            true, new byte[0], 0, 0.0));
            Assert.expectThrows(NullPointerException.class, assertions::matchesScreenshot);
        }
        verify(currentPage).locator(any(String.class));
        verify(currentLocator).screenshot();
    }

    @Test
    public void assertionAndVerificationStartersShouldRetainTheirCategories() throws Exception {
        PlaywrightSession session = mock(PlaywrightSession.class);
        ElementTarget target = ElementTarget.located(ShaftLocator.css("#save"));
        Assert.assertEquals(category(new PlaywrightDriverAssertions(session).element(target)),
                ValidationEnums.ValidationCategory.HARD_ASSERT);
        Assert.assertEquals(category(new ElementActions(session).assertThat(target)),
                ValidationEnums.ValidationCategory.HARD_ASSERT);
        Assert.assertEquals(category(new PlaywrightDriverVerifications(session).element(target)),
                ValidationEnums.ValidationCategory.SOFT_ASSERT);
        Assert.assertEquals(category(new ElementActions(session).verifyThat(target)),
                ValidationEnums.ValidationCategory.SOFT_ASSERT);
    }

    @Test
    public void everyPlaywrightStarterShouldExecuteFocusedCategories() {
        assertFocusedStarter(starter -> new PlaywrightDriverAssertions(starter.session()).element(starter.target()));
        assertFocusedStarter(starter -> new PlaywrightDriverVerifications(starter.session()).element(starter.target()));
        assertFocusedStarter(starter -> new ElementActions(starter.session()).assertThat(starter.target()));
        assertFocusedStarter(starter -> new ElementActions(starter.session()).verifyThat(starter.target()));
    }

    @Test
    public void focusedElementCategoriesShouldUseCurrentProviderValues() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        Locator locator = mock(Locator.class);
        LocatorAssertions locatorAssertions = mock(LocatorAssertions.class);
        when(session.page()).thenReturn(page);
        when(page.locator("#save")).thenReturn(locator);
        when(locator.page()).thenReturn(page);
        when(locator.count()).thenReturn(2);
        when(locator.boundingBox()).thenReturn(boundingBox(1.5, 2.5, 30.5, 40.5));
        when(locator.ariaSnapshot()).thenReturn("- button \"Save\"");
        doAnswer(invocation -> {
            BooleanSupplier condition = invocation.getArgument(0);
            condition.getAsBoolean();
            return null;
        }).when(page).waitForCondition(any(BooleanSupplier.class));
        ElementAssertions assertions = new PlaywrightDriverAssertions(session)
                .element(ElementTarget.located(ShaftLocator.css("#save")));

        try (MockedStatic<PlaywrightAssertions> playwrightAssertions = mockStatic(PlaywrightAssertions.class)) {
            playwrightAssertions.when(() -> PlaywrightAssertions.assertThat(locator)).thenReturn(locatorAssertions);
            assertions.elementCount().isEqualTo(2);
            assertions.elementRectangle().isEqualTo(new ElementRectangle(1.5, 2.5, 30.5, 40.5));
            assertions.elementAccessibleName().isEqualTo("Save");
            assertions.elementRole().isEqualTo("button");
        }

        verify(locatorAssertions).hasCount(2);
        verify(locatorAssertions).hasAccessibleName("Save");
        verify(locator).boundingBox();
        verify(locator, times(2)).ariaSnapshot();
    }

    @Test
    public void focusedCategoriesShouldNotCoerceUnsupportedComparisonsIntoNativeEquality() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        Locator locator = mock(Locator.class);
        LocatorAssertions locatorAssertions = mock(LocatorAssertions.class);
        when(session.page()).thenReturn(page);
        when(page.locator("#save")).thenReturn(locator);
        when(locator.page()).thenReturn(page);
        when(locator.count()).thenReturn(2);
        when(locator.ariaSnapshot()).thenReturn("- button \"Save\"");
        when(locatorAssertions.not()).thenReturn(locatorAssertions);
        doAnswer(invocation -> {
            BooleanSupplier condition = invocation.getArgument(0);
            condition.getAsBoolean();
            return null;
        }).when(page).waitForCondition(any(BooleanSupplier.class));
        ElementAssertions assertions = new PlaywrightDriverAssertions(session)
                .element(ElementTarget.located(ShaftLocator.css("#save")));

        try (MockedStatic<PlaywrightAssertions> playwrightAssertions = mockStatic(PlaywrightAssertions.class)) {
            playwrightAssertions.when(() -> PlaywrightAssertions.assertThat(locator)).thenReturn(locatorAssertions);
            Assert.expectThrows(AssertionError.class, () -> assertions.elementCount().isEqualTo(2.9));
            Assert.expectThrows(AssertionError.class, () -> assertions.elementCount().isEqualTo(Long.MAX_VALUE));
            Assert.expectThrows(AssertionError.class, () -> assertions.elementCount().doesNotEqual(2));
            assertions.elementCount().doesNotEqual(3);
            assertions.elementAccessibleName().contains("ave");
            assertions.elementAccessibleName().matchesRegex("S.*e");
            assertions.elementAccessibleName().equalsIgnoringCaseSensitivity("save");
            Assert.expectThrows(AssertionError.class,
                    () -> assertions.elementAccessibleName().doesNotEqual("Save"));
            assertions.elementAccessibleName().doesNotEqual("Cancel");
        }

        verify(locatorAssertions, never()).hasCount(2);
        verify(locatorAssertions, never()).hasCount(-1);
        verify(locatorAssertions, never()).hasAccessibleName("ave");
        verify(locatorAssertions, never()).hasAccessibleName("S.*e");
        verify(locatorAssertions, never()).hasAccessibleName("save");
    }

    @Test
    public void nativeCountShouldAcceptEveryExactlyRepresentableNumberType() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        Locator locator = mock(Locator.class);
        LocatorAssertions locatorAssertions = mock(LocatorAssertions.class);
        when(session.page()).thenReturn(page);
        when(page.locator("#save")).thenReturn(locator);
        when(locator.count()).thenReturn(2);
        ElementAssertions assertions = new PlaywrightDriverAssertions(session)
                .element(ElementTarget.located(ShaftLocator.css("#save")));

        try (MockedStatic<PlaywrightAssertions> playwrightAssertions = mockStatic(PlaywrightAssertions.class)) {
            playwrightAssertions.when(() -> PlaywrightAssertions.assertThat(locator)).thenReturn(locatorAssertions);
            for (Number expected : java.util.List.of((byte) 2, (short) 2, 2, 2L,
                    BigInteger.TWO, BigDecimal.valueOf(2), 2F, 2D)) {
                assertions.elementCount().isEqualTo(expected);
            }
        }

        verify(locatorAssertions, times(8)).hasCount(2);
    }

    @Test
    public void focusedFallbackValuesShouldRetryUntilTheComparisonMatches() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        Locator locator = mock(Locator.class);
        when(session.page()).thenReturn(page);
        when(page.locator("#save")).thenReturn(locator);
        when(locator.page()).thenReturn(page);
        when(locator.boundingBox()).thenReturn(null, boundingBox(1, 2, 3, 4));
        when(locator.ariaSnapshot()).thenReturn("- link \"Save\"", "- button \"Save\"");
        doAnswer(invocation -> {
            BooleanSupplier condition = invocation.getArgument(0);
            Assert.assertFalse(condition.getAsBoolean());
            Assert.assertTrue(condition.getAsBoolean());
            return null;
        }).when(page).waitForCondition(any(BooleanSupplier.class));
        ElementAssertions assertions = new PlaywrightDriverAssertions(session)
                .element(ElementTarget.located(ShaftLocator.css("#save")));

        assertions.elementRectangle().isEqualTo(new ElementRectangle(1, 2, 3, 4));
        assertions.elementRole().isEqualTo("button");

        verify(locator, times(2)).boundingBox();
        verify(locator, times(2)).ariaSnapshot();
        verify(page, times(2)).waitForCondition(any(BooleanSupplier.class));
    }

    @Test
    public void focusedFallbackShouldNeverCompareProviderExceptionMessagesAsValues() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        Page locatorPage = mock(Page.class);
        when(locator.page()).thenReturn(locatorPage);
        when(locator.ariaSnapshot()).thenThrow(new IllegalStateException("button"));
        doAnswer(invocation -> {
            BooleanSupplier condition = invocation.getArgument(0);
            condition.getAsBoolean();
            throw new IllegalStateException("poll ended");
        }).when(locatorPage).waitForCondition(any(BooleanSupplier.class));
        ElementAssertions assertions = new PlaywrightDriverAssertions(session).element(locator);

        Assert.expectThrows(AssertionError.class, () -> assertions.elementRole().isEqualTo("button"));

        verify(locatorPage).waitForCondition(any(BooleanSupplier.class));
    }

    @Test
    public void focusedFallbackWithoutAnObservationShouldFailEvenWhenExpectedIsNull() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        Page locatorPage = mock(Page.class);
        when(locator.page()).thenReturn(locatorPage);
        doThrow(new IllegalStateException("closed")).when(locatorPage)
                .waitForCondition(any(BooleanSupplier.class));

        Assert.expectThrows(AssertionError.class,
                () -> new PlaywrightDriverAssertions(session).element(locator).elementRole().isEqualTo(null));
    }

    @Test
    public void focusedCategoryFailureShouldThrowOnlyForHardStarters() {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        LocatorAssertions locatorAssertions = mock(LocatorAssertions.class);
        try (MockedStatic<PlaywrightAssertions> playwrightAssertions = mockStatic(PlaywrightAssertions.class)) {
            playwrightAssertions.when(() -> PlaywrightAssertions.assertThat(locator)).thenReturn(locatorAssertions);
            doThrow(new AssertionError("wrong count")).when(locatorAssertions).hasCount(2);

            Assert.expectThrows(AssertionError.class,
                    () -> new PlaywrightDriverAssertions(session).element(locator).elementCount().isEqualTo(2));
            Assert.assertNull(ValidationsHelper.getVerificationErrorToForceFail());

            new PlaywrightDriverVerifications(session).element(locator).elementCount().isEqualTo(2);
            Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
            ValidationsHelper.resetVerificationStateAfterFailing();
        }
    }

    private static void assertLazyTarget(Function<Starter, ElementAssertions> createAssertions) {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page initialPage = mock(Page.class);
        Page currentPage = mock(Page.class);
        Locator currentLocator = mock(Locator.class);
        LocatorAssertions locatorAssertions = mock(LocatorAssertions.class);
        AtomicReference<Page> selectedPage = new AtomicReference<>(initialPage);
        when(session.page()).thenAnswer(ignored -> selectedPage.get());
        when(currentPage.locator("#save")).thenReturn(currentLocator);
        when(currentLocator.count()).thenReturn(1);
        ElementTarget target = ElementTarget.located(ShaftLocator.css("#save"));

        ElementAssertions assertions;
        try {
            assertions = createAssertions.apply(new Starter(session, target));
        } catch (RuntimeException exception) {
            Assert.fail("Creating Playwright assertions must not resolve a portable target.", exception);
            return;
        }
        verify(initialPage, never()).locator(any(String.class));

        selectedPage.set(currentPage);
        try (MockedStatic<PlaywrightAssertions> playwrightAssertions = mockStatic(PlaywrightAssertions.class)) {
            playwrightAssertions.when(() -> PlaywrightAssertions.assertThat(currentLocator)).thenReturn(locatorAssertions);
            assertions.exists();
        }

        verify(initialPage, never()).locator(any(String.class));
        verify(currentPage).locator("#save");
        verify(currentPage, never()).locator("[id=\"save\"]");
        verify(locatorAssertions).isAttached();
    }

    private static void assertFocusedStarter(Function<Starter, ElementAssertions> createAssertions) {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page page = mock(Page.class);
        Locator locator = mock(Locator.class);
        LocatorAssertions locatorAssertions = mock(LocatorAssertions.class);
        when(session.page()).thenReturn(page);
        when(page.locator("#save")).thenReturn(locator);
        when(locator.count()).thenReturn(1);
        ElementTarget target = ElementTarget.located(ShaftLocator.css("#save"));

        try (MockedStatic<PlaywrightAssertions> playwrightAssertions = mockStatic(PlaywrightAssertions.class)) {
            playwrightAssertions.when(() -> PlaywrightAssertions.assertThat(locator)).thenReturn(locatorAssertions);
            createAssertions.apply(new Starter(session, target)).elementCount().isEqualTo(1);
        }

        verify(locatorAssertions).hasCount(1);
    }

    private static void assertLazyExistingStarter(String expectedSelector,
                                                  ValidationEnums.ValidationCategory expectedCategory,
                                                  Function<ExistingStarter, ElementAssertions> createAssertions) {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Page initialPage = mock(Page.class);
        Page currentPage = mock(Page.class);
        Locator currentLocator = mock(Locator.class);
        LocatorAssertions locatorAssertions = mock(LocatorAssertions.class);
        AtomicReference<Page> selectedPage = new AtomicReference<>(initialPage);
        when(session.page()).thenAnswer(ignored -> selectedPage.get());
        when(currentPage.locator(expectedSelector)).thenReturn(currentLocator);
        when(currentLocator.count()).thenReturn(1);

        ElementAssertions assertions = createAssertions.apply(
                new ExistingStarter(session, By.id("save"), ShaftLocator.css("#save")));
        verify(initialPage, never()).locator(any(String.class));

        selectedPage.set(currentPage);
        try (MockedStatic<PlaywrightAssertions> playwrightAssertions = mockStatic(PlaywrightAssertions.class)) {
            playwrightAssertions.when(() -> PlaywrightAssertions.assertThat(currentLocator)).thenReturn(locatorAssertions);
            assertions.exists();
        }

        verify(initialPage, never()).locator(any(String.class));
        verify(currentPage).locator(expectedSelector);
        String alternateSelector = "#save".equals(expectedSelector) ? "[id=\"save\"]" : "#save";
        verify(currentPage, never()).locator(alternateSelector);
        verify(locatorAssertions).isAttached();
        try {
            Assert.assertEquals(category(assertions), expectedCategory);
        } catch (Exception exception) {
            Assert.fail("Could not inspect the validation category.", exception);
        }
    }

    private static void assertNativeStarter(ValidationEnums.ValidationCategory expectedCategory,
                                            Function<NativeStarter, ElementAssertions> createAssertions) throws Exception {
        PlaywrightSession session = mock(PlaywrightSession.class);
        Locator locator = mock(Locator.class);
        LocatorAssertions locatorAssertions = mock(LocatorAssertions.class);
        when(locator.count()).thenReturn(1);
        ElementAssertions assertions = createAssertions.apply(new NativeStarter(session, locator));

        try (MockedStatic<PlaywrightAssertions> playwrightAssertions = mockStatic(PlaywrightAssertions.class)) {
            playwrightAssertions.when(() -> PlaywrightAssertions.assertThat(locator)).thenReturn(locatorAssertions);
            assertions.exists();
        }

        verify(session, never()).page();
        verify(locatorAssertions).isAttached();
        Assert.assertEquals(category(assertions), expectedCategory);
    }

    private record Starter(PlaywrightSession session, ElementTarget target) {
    }

    private record ExistingStarter(PlaywrightSession session, By by, ShaftLocator shaftLocator) {
    }

    private record NativeStarter(PlaywrightSession session, Locator locator) {
    }

    private static BoundingBox boundingBox(double x, double y, double width, double height) {
        BoundingBox box = new BoundingBox();
        box.x = x;
        box.y = y;
        box.width = width;
        box.height = height;
        return box;
    }

    private static Object category(ElementAssertions assertions) throws Exception {
        Field field = PlaywrightElementValidationsBuilder.class.getDeclaredField("validationCategory");
        field.setAccessible(true);
        return field.get(assertions);
    }
}
