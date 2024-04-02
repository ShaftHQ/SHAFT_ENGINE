package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

import static com.shaft.driver.SHAFT.Validations.assertThat;

public class YAMLFileManagerTests {
    private SHAFT.TestData.YAML yaml;

    @BeforeMethod
    public void setUp() {
        yaml = new SHAFT.TestData.YAML("src/test/resources/testDataFiles/yaml/yaml_test_data.yaml");
    }

    @Test
    public void getObject() {
        assertThat()
                .object(yaml.get("object"))
                .isEqualTo("object")
                .perform();
    }

    @Test
    public void getString() {
        assertThat()
                .object(yaml.getString("string"))
                .isEqualTo("text")
                .perform();
    }

    @Test
    public void getLongString() {
        assertThat()
                .object(yaml.getString("long-text"))
                .isEqualTo("this is not a normal string it" +
                        " spans more than" +
                        " one line" +
                        " see?\n")
                .perform();
    }

    @Test
    public void getLongString2() {
        assertThat()
                .object(yaml.getString("long-text2"))
                .isEqualTo("""
                        this is not a normal string it
                        spans more than
                        one line
                        see?
                        """)
                .perform();
    }

    @Test
    public void getIntegerAsString() {
        assertThat()
                .object(yaml.getTestData("integer"))
                .isEqualTo("10")
                .perform();
    }

    @Test
    public void getInteger() {
        assertThat()
                .object(yaml.getInteger("integer"))
                .isEqualTo(10)
                .perform();
    }

    @Test
    public void getHexAsInteger() {
        assertThat()
                .object(yaml.getInteger("hex-decimal"))
                .isEqualTo(4820)
                .perform();
    }

    @Test
    public void getOctalAsInteger() {
        assertThat()
                .object(yaml.getInteger("octal"))
                .isEqualTo(9946)
                .perform();
    }

    @Test
    public void getDouble() {
        assertThat()
                .object(yaml.getDouble("double"))
                .isEqualTo(5.3)
                .perform();
    }

    @Test
    public void getExponentialAsDouble() {
        assertThat()
                .object(yaml.getDouble("exponential"))
                .isEqualTo(1230150.0)
                .perform();
    }

    @Test
    public void getInfinityAsDouble() {
        assertThat()
                .object(yaml.getDouble("infinity"))
                .isEqualTo(Double.POSITIVE_INFINITY)
                .perform();
    }

    @Test
    public void getNegativeInfinityAsDouble() {
        assertThat()
                .object(yaml.getDouble("negative-infinity"))
                .isEqualTo(Double.NEGATIVE_INFINITY)
                .perform();
    }

    @Test
    public void getNaNAsDouble() {
        assertThat()
                .object(yaml.getDouble("not-a-number"))
                .isEqualTo(Double.NaN)
                .perform();
    }

    @Test
    public void getLong() {
        assertThat()
                .object(yaml.getLong("long"))
                .isEqualTo(10L)
                .perform();
    }

    @Test
    public void getBoolean() {
        assertThat()
                .object(yaml.getBoolean("boolean"))
                .isTrue()
                .perform();
    }

    @Test
    public void getBoolean1() {
        assertThat()
                .object(yaml.getBoolean("boolean1"))
                .isFalse()
                .perform();
    }

    // TODO: has issue related to time zone always use the local time zone
//    @Test
    public void getDate() {
        assertThat()
                .object(yaml.getDate("date"))
                .isEqualTo("2010-02-11T00:00:00")
                .perform();
    }

    // TODO: has issue related to time zone always use the local time zone
//    @Test
    public void getDateTime() {
        assertThat()
                .object(yaml.getDate("date-time"))
                .isEqualTo("2010-02-11T11:02:57")
                .perform();
    }

    @Test
    public void getNull0() {
        assertThat()
                .object(yaml.get("null0"))
                .isNull()
                .perform();
    }

    @Test
    public void getNull1() {
        assertThat()
                .object(yaml.get("null1"))
                .isNull()
                .perform();
    }

    @Test
    public void getAsString() {
        assertThat()
                .object(yaml.getAs("string", String.class))
                .isEqualTo("text")
                .perform();
    }

    @Test
    public void getMap() {
        assertThat()
                .object(yaml.getMapAs("map", String.class))
                .isEqualTo(Map.of("m1", "m1", "m2", "m2"))
                .perform();
    }

    @Test
    public void getList() {
        assertThat()
                .object(yaml.getListAs("list", String.class))
                .isEqualTo(List.of("l1", "l2"))
                .perform();
    }

    @Test
    public void nestedList() {
        assertThat()
                .object(yaml.getString("nested-list[0][0][0][0][0][0]"))
                .isEqualTo("You caught me")
                .perform();
    }

    @Test
    public void nestedMap() {
        assertThat()
                .object(yaml.getString("nested-map.a.b.c.d"))
                .isEqualTo("I covered by maps:D")
                .perform();
    }

    @Test
    public void mixMapAdList() {
        assertThat()
                .object(yaml.getString("mix-map-list.m1[1].l1.m2[1].l3"))
                .isEqualTo("HOW DID YOU FIND ME!!!")
                .perform();
    }

    @Test
    public void getValueWithWrongType() {
        Assert.assertThrows(
                AssertionError.class,
                () -> yaml.getAs("integer", Boolean.class)
        );
    }

    @Test
    public void getNotExistedKey() {
        Assert.assertThrows(
                AssertionError.class,
                () -> yaml.get("not-existed-key")
        );
    }

    @Test()
    public void getInvalidLong() {
        Assert.assertThrows(
                AssertionError.class,
                () -> yaml.getLong("invalid-long")
        );
    }
}
