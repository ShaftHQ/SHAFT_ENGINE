package testPackage.locator;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.Role;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import org.openqa.selenium.By;
import testPackage.TestPageServer;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.function.Supplier;

public class LocatorBuilderTest {

    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL(TestPageServer.url("locatorBuilderFixture.html"));
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (driver.get() != null) {
            driver.get().quit();
            driver.remove();
        }
        LocatorBuilder.cleanup();
        ShadowLocatorBuilder.cleanup();
    }

    @Test
    public void xpathModeShouldResolveLocatorMethodsAgainstFixture() {
        assertLocatorCases(List.of(
                new LocatorCase("hasTagName", () -> Locator.hasTagName("article").build(), "data-locator-case", "tag-name"),
                new LocatorCase("hasAnyTagName", () -> Locator.hasAnyTagName().hasAttribute("data-any-tag").build(), "data-locator-case", "any-tag"),
                new LocatorCase("hasAttribute", () -> Locator.hasAnyTagName().hasAttribute("data-present").build(), "data-locator-case", "has-attribute"),
                new LocatorCase("hasAttributeValue", () -> Locator.hasAnyTagName().hasAttribute("data-exact", "exact-value").build(), "data-locator-case", "has-attribute-value"),
                new LocatorCase("containsAttribute", () -> Locator.hasAnyTagName().containsAttribute("data-partial", "needle").build(), "data-locator-case", "contains-attribute"),
                new LocatorCase("hasId", () -> Locator.hasAnyTagName().hasId("exact-id-target").build(), "data-locator-case", "has-id"),
                new LocatorCase("containsId", () -> Locator.hasAnyTagName().containsId("partial-id").build(), "data-locator-case", "contains-id"),
                new LocatorCase("hasClass", () -> Locator.hasAnyTagName().hasClass("exact-class").build(), "data-locator-case", "has-class"),
                new LocatorCase("containsClass", () -> Locator.hasAnyTagName().containsClass("partial-class").build(), "data-locator-case", "contains-class"),
                new LocatorCase("hasText", () -> Locator.hasAnyTagName().hasText("Exact text target").build(), "data-locator-case", "has-text"),
                new LocatorCase("containsText", () -> Locator.hasTagName("div").containsText("partial text target").build(), "data-locator-case", "contains-text"),
                new LocatorCase("and", () -> Locator.hasAnyTagName().and().hasAttribute("data-and-target").build(), "data-locator-case", "and-method"),
                new LocatorCase("hasIndex", () -> Locator.hasTagName("mark").hasIndex(2).build(), "data-locator-case", "second-index"),
                new LocatorCase("isFirst", () -> Locator.hasTagName("mark").isFirst().build(), "data-locator-case", "first-index"),
                new LocatorCase("isLast", () -> Locator.hasTagName("mark").isLast().build(), "data-locator-case", "last-index")
        ));
    }

    @Test
    public void cssModeShouldResolveLocatorMethodsInsideShadowDomAgainstFixture() {
        By shadowHost = By.id("shadow-host");
        assertLocatorCases(List.of(
                new LocatorCase("hasTagName", () -> Locator.hasTagName("section").insideShadowDom(shadowHost).build(), "data-css-case", "tag-name"),
                new LocatorCase("hasAnyTagName", () -> Locator.hasAnyTagName().hasAttribute("data-any-tag-shadow").insideShadowDom(shadowHost).build(), "data-css-case", "any-tag"),
                new LocatorCase("hasAttribute", () -> Locator.hasAnyTagName().hasAttribute("data-present-shadow").insideShadowDom(shadowHost).build(), "data-css-case", "has-attribute"),
                new LocatorCase("hasAttributeValue", () -> Locator.hasAnyTagName().hasAttribute("data-exact-shadow", "exact-value").insideShadowDom(shadowHost).build(), "data-css-case", "has-attribute-value"),
                new LocatorCase("containsAttribute", () -> Locator.hasAnyTagName().containsAttribute("data-partial-shadow", "needle").insideShadowDom(shadowHost).build(), "data-css-case", "contains-attribute"),
                new LocatorCase("hasId", () -> Locator.hasAnyTagName().hasId("shadow-exact-id-target").insideShadowDom(shadowHost).build(), "data-css-case", "has-id"),
                new LocatorCase("containsId", () -> Locator.hasAnyTagName().containsId("partial-id").insideShadowDom(shadowHost).build(), "data-css-case", "contains-id"),
                new LocatorCase("hasClass", () -> Locator.hasAnyTagName().hasClass("shadow-exact-class").insideShadowDom(shadowHost).build(), "data-css-case", "has-class"),
                new LocatorCase("containsClass", () -> Locator.hasAnyTagName().containsClass("shadow-partial-class").insideShadowDom(shadowHost).build(), "data-css-case", "contains-class"),
                new LocatorCase("hasText", () -> Locator.hasTagName("strong").hasText("Exact shadow text target").insideShadowDom(shadowHost).build(), "data-css-case", "has-text"),
                new LocatorCase("containsText", () -> Locator.hasTagName("em").containsText("shadow partial text target").insideShadowDom(shadowHost).build(), "data-css-case", "contains-text"),
                new LocatorCase("and", () -> Locator.hasTagName("u").and().hasAttribute("data-and-shadow").insideShadowDom(shadowHost).build(), "data-css-case", "and-method"),
                new LocatorCase("hasIndex", () -> Locator.hasTagName("mark").hasIndex(2).insideShadowDom(shadowHost).build(), "data-css-case", "second-index"),
                new LocatorCase("isFirst", () -> Locator.hasTagName("mark").isFirst().insideShadowDom(shadowHost).build(), "data-css-case", "first-index"),
                new LocatorCase("isLast", () -> Locator.hasTagName("mark").isLast().insideShadowDom(shadowHost).build(), "data-css-case", "last-index")
        ));
    }

    @Test
    public void xpathModeShouldResolveAllRoleLocatorsAgainstFixture() {
        assertLocatorCases(List.of(
                new LocatorCase("roleButton", () -> Locator.hasRole(Role.BUTTON).and().hasAttribute("data-role-case", "button").build(), "data-role-case", "button"),
                new LocatorCase("roleLink", () -> Locator.hasRole(Role.LINK).and().hasAttribute("data-role-case", "link").build(), "data-role-case", "link"),
                new LocatorCase("roleTextbox", () -> Locator.hasRole(Role.TEXTBOX).and().hasAttribute("data-role-case", "textbox").build(), "data-role-case", "textbox"),
                new LocatorCase("roleCheckbox", () -> Locator.hasRole(Role.CHECKBOX).and().hasAttribute("data-role-case", "checkbox").build(), "data-role-case", "checkbox"),
                new LocatorCase("roleRadio", () -> Locator.hasRole(Role.RADIO).and().hasAttribute("data-role-case", "radio").build(), "data-role-case", "radio"),
                new LocatorCase("roleCombobox", () -> Locator.hasRole(Role.COMBOBOX).and().hasAttribute("data-role-case", "combobox").build(), "data-role-case", "combobox"),
                new LocatorCase("roleHeading", () -> Locator.hasRole(Role.HEADING).and().containsText("SHAFT").build(), "class", "hero__title"),
                new LocatorCase("roleImage", () -> Locator.hasRole(Role.IMAGE).and().hasAttribute("data-role-case", "image").build(), "data-role-case", "image"),
                new LocatorCase("roleList", () -> Locator.hasRole(Role.LIST).and().hasAttribute("data-role-case", "list").build(), "data-role-case", "list"),
                new LocatorCase("roleListItem", () -> Locator.hasRole(Role.LISTITEM).and().hasAttribute("data-role-case", "listitem").build(), "data-role-case", "listitem"),
                new LocatorCase("roleTable", () -> Locator.hasRole(Role.TABLE).and().hasAttribute("data-role-case", "table").build(), "data-role-case", "table"),
                new LocatorCase("roleTableRow", () -> Locator.hasRole(Role.TABLE_ROW).and().hasAttribute("data-role-case", "table-row").build(), "data-role-case", "table-row"),
                new LocatorCase("roleTableCell", () -> Locator.hasRole(Role.TABLE_CELL).and().hasAttribute("data-role-case", "table-cell").build(), "data-role-case", "table-cell"),
                new LocatorCase("roleTableColumnHeader", () -> Locator.hasRole(Role.TABLE_COLUMNHEADER).and().hasAttribute("data-role-case", "table-columnheader").build(), "data-role-case", "table-columnheader")
        ));
    }

    @Test
    public void hasTagName() {

        By locator = SHAFT.GUI.Locator.hasTagName("h1").build();
        driver.get().assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsText() {
        By locator = Locator.hasTagName("h1").containsText("Unified Test Automation Engine").build();
        driver.get().assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsAttribute() {
        By locator = Locator.hasTagName("h1").containsAttribute("class", "hero").build();
        driver.get().assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsClass() {
        By locator = Locator.hasTagName("h1").containsClass("hero").build();
        driver.get().assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void containsIdAndIsFirst() {
        By locator = Locator.hasTagName("*").containsId("_docusaurus").isFirst().build();
        driver.get().assertThat().element(locator).cssProperty("display").contains("flex").perform();
    }

    @Test
    public void hasAttributeAndContainsIdAndIsFirst() {
        By locator = Locator.hasTagName("*").hasAttribute("id").containsId("_docusaurus").isFirst().build();
        driver.get().assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasIdAndContainsId() {
        By locator = Locator.hasTagName("*").hasId("__docusaurus").build();
        driver.get().assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasAttributeWithValue() {
        By locator = SHAFT.GUI.Locator.hasAnyTagName()
                .and().hasAttribute("id", "__docusaurus")
                .build();
        driver.get().assertThat().element(locator).attribute("display").contains("null").perform();
    }

    @Test
    public void hasClass() {
        By locator = Locator.hasTagName("h1").hasClass("hero__title").build();
        driver.get().assertThat().element(locator).text().contains("SHAFT").perform();
    }

    @Test
    public void hasText() {
        By locator = Locator.hasTagName("h1").hasText("SHAFT: Unified Test Automation Engine").build();
        driver.get().assertThat().element(locator).attribute("class").contains("hero__title").perform();
    }

    @Test
    public void hasIndex() {
        By locator = Locator.hasTagName("p").hasIndex(1).build();
        driver.get().assertThat().element(locator).text().contains("Write once, test everywhere").perform();
    }

    @Test
    public void isFirst() {
        By locator = Locator.hasTagName("p").isFirst().build();
        driver.get().assertThat().element(locator).text().contains("Write once, test everywhere").perform();
    }

    @Test
    public void isLast() {
        By locator = Locator.hasTagName("p").isLast().build();
        driver.get().assertThat().element(locator).text().contains("Ready to transform your test automation?").perform();
    }

    @Test
    public void byRelation() {
        By locator = Locator.hasTagName("a").containsText("Get Started Free").byRelation().below(Locator.hasTagName("h1").containsText("SHAFT").build());
        driver.get().assertThat().element(locator).text().contains("Get Started Free").perform();
    }

    @Test
    public void byAxis_followingSibling() {
        By locator = Locator.hasTagName("h1").containsText("SHAFT").byAxis().followingSibling("p").build();
        driver.get().assertThat().element(locator).text().contains("Write once, test everywhere").perform();
    }

    @Test
    public void byAxis_chain_precedingSibling() {
        By locator = Locator.hasTagName("h1").containsText("SHAFT").byAxis().followingSibling("p").byAxis().precedingSibling("h1").hasIndex(1).build();
        driver.get().assertThat().element(locator).text().isEqualTo("SHAFT: Unified Test Automation Engine").perform();
    }

    @Test
    public void byRoleHeading() {
        By locator = Locator.hasRole(Role.HEADING).and().containsText("SHAFT").build();
        driver.get().assertThat().element(locator).text().isEqualTo("SHAFT: Unified Test Automation Engine").perform();
    }

    @Test
    public void byRoleButton() {
        By locator = Locator.hasRole(Role.BUTTON).and().containsText("Get Started").isFirst().build();
        driver.get().assertThat().element(locator).text().isEqualTo("Get Started Free").perform();
    }

    @Test
    public void byRoleLink() {
        By locator = Locator.hasRole(Role.LINK).and().containsText("Selenium").build();
        driver.get().assertThat().element(locator).text().isEqualTo("official Selenium ecosystem frameworks").perform();
    }

    @Test
    public void byRoleTextbox() {
        By locator = Locator.hasRole(Role.TEXTBOX).and().byRelation().toRightOf(Locator.hasTagName("label").containsText("# Tests to be automated:").build());
        driver.get().assertThat().element(locator).attribute("value").isEqualTo("100").perform();
    }

    @Test
    public void containsClassInsideShadowDom() {
        By shadowHost = By.id("shadow-host");
        By locator = Locator.hasTagName("span")
                .containsClass("wrapper")
                .insideShadowDom(shadowHost)
                .build();
        driver.get().assertThat().element(locator).text().contains("Shadow content").perform();
    }

    @Test
    public void containsAttributeInsideShadowDom() {
        By shadowHost = By.id("shadow-host");
        By locator = Locator.hasTagName("input")
                .containsAttribute("name", "shadow")
                .insideShadowDom(shadowHost)
                .build();
        driver.get().assertThat().element(locator).attribute("value").isEqualTo("shadow-value").perform();
    }

    private void assertLocatorCases(List<LocatorCase> locatorCases) {
        for (LocatorCase locatorCase : locatorCases) {
            try {
                driver.get().assertThat().element(locatorCase.locator().get())
                        .attribute(locatorCase.attribute())
                        .isEqualTo(locatorCase.expectedValue())
                        .perform();
            } catch (RuntimeException | AssertionError failure) {
                throw new AssertionError("Locator case failed: " + locatorCase.name(), failure);
            }
        }
    }

    private record LocatorCase(String name, Supplier<By> locator, String attribute, String expectedValue) {
    }
}
