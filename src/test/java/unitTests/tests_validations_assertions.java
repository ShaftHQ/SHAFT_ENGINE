package unitTests;

import org.testng.annotations.Test;

import com.shaft.validation.Assertions;

public class tests_validations_assertions {
	@Test
	public void assertEquals_true_expectedToPass() {
		Assertions.assertEquals(1, 1, true);
	}

	@Test
	public void assertEquals_true_expectedToFail() {
		Assertions.assertEquals(1, 2, true);
	}

	@Test
	public void assertEquals_false_expectedToPass() {
		Assertions.assertEquals(1, 2, false);
	}

	@Test
	public void assertEquals_false_expectedToFail() {
		Assertions.assertEquals(1, 1, false);
	}

	@Test
	public void assertNull_true_expectedToPass() {
		Assertions.assertNull(null, true);
	}

	@Test
	public void assertNull_true_expectedToFail() {
		Assertions.assertNull(1, true);
	}

	@Test
	public void assertNull_false_expectedToPass() {
		Assertions.assertNull(1, false);
	}

	@Test
	public void assertNull_false_expectedToFail() {
		Assertions.assertNull(null, false);
	}

	@Test
	public void assertGreaterThanOrEquals_true_greaterThan_expectedToPass() {
		Assertions.assertGreaterThanOrEquals(1, 2, true);
	}

	@Test
	public void assertGreaterThanOrEquals_true_equals_expectedToPass() {
		Assertions.assertGreaterThanOrEquals(1, 1, true);
	}

	@Test
	public void assertGreaterThanOrEquals_true_greaterThan_expectedToFail() {
		Assertions.assertGreaterThanOrEquals(2, 1, true);
	}

	@Test
	public void assertGreaterThanOrEquals_true_equals_expectedToFail() {
		Assertions.assertGreaterThanOrEquals(1, 0, true);
	}

	@Test
	public void assertGreaterThanOrEquals_false_greaterThan_expectedToPass() {
		Assertions.assertGreaterThanOrEquals(2, 1, false);
	}

	@Test
	public void assertGreaterThanOrEquals_false_equals_expectedToPass() {
		Assertions.assertGreaterThanOrEquals(2, 1, false);
	}

	@Test
	public void assertGreaterThanOrEquals_false_greaterThan_expectedToFail() {
		Assertions.assertGreaterThanOrEquals(1, 2, false);
	}

	@Test
	public void assertGreaterThanOrEquals_false_equals_expectedToFail() {
		Assertions.assertGreaterThanOrEquals(1, 1, false);
	}

}
