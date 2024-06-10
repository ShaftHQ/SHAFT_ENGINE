package com.shaft.tools.internal.support;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.By;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.testng.Assert;

import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


@SuppressWarnings("unused")
public class JavaHelper {

    private JavaHelper() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Takes two parameters, one is the array of special characters that are needed
     * to be replaced, and the text needed to be updated It converts text with
     * %40%23%24%25%26 ..etc (special characters) to return it with
     * %5C%5C%40%5C%5C%23%5C%5C%24%5C%5C%25%5C%5C%26
     *
     * @param specialCharactersArray an array of the special characters that will be
     *                               escaped
     * @param text                   the string that will have its special
     *                               characters escaped
     * @return updated texts with escaped special characters
     */
    public static String replaceRegex(String[] specialCharactersArray, String text) {
        // @#$%&
        // \\@\\#\\$\\%\\&

        String oldChar;
        for (int i = 0; i < (specialCharactersArray.length); i++) {
            oldChar = specialCharactersArray[i];
            specialCharactersArray[i] = ("\\" + specialCharactersArray[i]);
            text = text.replace(oldChar, specialCharactersArray[i]);
        }
        return text;
    }

    public static String removeSpecialCharacters(String text) {
        StringBuilder cleanString = new StringBuilder();
        if (text != null) {
            for (int i = 0; i < text.toCharArray().length; i++) {
                var character = String.valueOf(text.toCharArray()[i]);
                if (Pattern.compile("[^a-z0-9]", Pattern.CASE_INSENSITIVE).matcher(character).find()) {
                    cleanString.append("_");
                } else {
                    cleanString.append(character);
                }
            }
        }
        return cleanString.toString();
    }

    /**
     * Returns text after replaces its regular expressions which included in this
     * set []^$.|?*+(){}
     *
     * @param text the string that will have its special characters escaped
     * @return updated text after escaping its regular expressions
     */

    public static String replaceRegex(String text) {
        String[] specialCharactersArray = {"[", "]", "^", "$", ".", "|", "?", "*", "+", "(", ")", "{", "}"};
        return replaceRegex(specialCharactersArray, text);
    }

    public static String encodeToBase64String(String text) {
        return Base64.getUrlEncoder().encodeToString(text.getBytes());
    }

    /**
     * Compares two objects (that can be cast to a string value) based on the
     * selected comparisonType and ValidationType, then returns the result in an
     * integer value
     *
     * @param expectedValue  the expected value (test data) of this assertion
     * @param actualValue    the actual value (calculated data) of this assertion
     * @param comparisonType 1 is literalComparison, 2 is regexComparison, 3 is
     *                       containsComparison, 4 is caseInsensitiveComparison
     * @param validationType either 'true' for a positive assertion that the objects
     *                       are equal, or 'false' for a negative assertion that the
     *                       objects are not equal
     * @return integer value; 1 in case of match, 0 in case of no match, -1 in case
     * of invalid comparison operator, -2 in case of another unhandled
     * exception
     */
    public static int compareTwoObjects(Object expectedValue, Object actualValue, Object comparisonType,
                                        Boolean validationType) {
        ReportManagerHelper.logDiscrete("Expected \"" + expectedValue + "\", and actual \"" + actualValue + "\"", Level.DEBUG);
        if ("null".equals(expectedValue)) {
            expectedValue = null;
        }

        if ("null".equals(actualValue)) {
            actualValue = null;
        }

        if (comparisonType instanceof Integer comparisonInteger) {
            // comparison integer is used for all string-based, null, boolean, and Object comparisons
            if (Boolean.TRUE.equals(validationType)) {
                try {
                    return compareTwoObjectsPositively(expectedValue, actualValue, comparisonInteger);
                } catch (AssertionError e) {
                    return 0;
                } catch (Exception e) {
                    ReportManagerHelper.logDiscrete(e);
                    return -2;
                }
            } else {
                try {
                    return compareTwoObjectsNegatively(expectedValue, actualValue, comparisonInteger);
                } catch (AssertionError e) {
                    return 0;
                } catch (Exception e) {
                    ReportManagerHelper.logDiscrete(e);
                    return -2;
                }
            }
        } else if (comparisonType instanceof ValidationEnums.NumbersComparativeRelation numbersComparativeRelation) {
            // this means that it is a number-based comparison
            Boolean comparisonState = getNumberComparisonState(expectedValue, actualValue, numbersComparativeRelation);
            return (comparisonState && validationType) || (!comparisonState && !validationType) ? 1 : 0;
        }
        return -2;
    }

    private static Boolean getNumberComparisonState(Object expectedValue, Object actualValue, ValidationEnums.NumbersComparativeRelation numbersComparativeRelation) {
        Float expected = Float.parseFloat(String.valueOf(expectedValue));
        Float actual = Float.parseFloat(String.valueOf(actualValue));
        return switch (numbersComparativeRelation.getValue()) {
            case ">" -> actual > expected;
            case ">=" -> actual >= expected;
            case "<" -> actual < expected;
            case "<=" -> actual <= expected;
            case "==" -> actual.equals(expected);
            default -> false;
        };
    }

    private static int compareTwoObjectsPositively(Object expectedValue, Object actualValue, int comparisonType) {
        switch (comparisonType) {
            case 1 -> {
                // case sensitive literal equivalence
                switch (expectedValue) {
                    case null -> Assert.assertNull(actualValue);
                    case String expectedString when actualValue instanceof String actualString ->
                            Assert.assertEquals(actualString, expectedString);
                    case Number expectedNumber when actualValue instanceof Number actualNumber ->
                            Assert.assertEquals(actualNumber, expectedNumber);
                    default -> Assert.assertEquals(actualValue, expectedValue);
                }
            }
            case 2 ->
                // regex comparison
                    Assert.assertTrue((String.valueOf(actualValue)).matches(String.valueOf(expectedValue)));
            case 3 ->
                // contains
                    Assert.assertTrue((String.valueOf(actualValue)).contains(String.valueOf(expectedValue)));
            case 4 ->
                // case insensitive equivalence
                    Assert.assertTrue((String.valueOf(actualValue)).equalsIgnoreCase(String.valueOf(expectedValue)));
            default -> {
                // unhandled case
                return -1;
            }
        }
        return 1;
    }

    private static int compareTwoObjectsNegatively(Object expectedValue, Object actualValue, int comparisonType) {
        switch (comparisonType) {
            case 1 -> {
                // case sensitive literal equivalence
                switch (expectedValue) {
                    case null -> Assert.assertNotNull(actualValue);
                    case String expectedString when actualValue instanceof String actualString ->
                            Assert.assertNotEquals(actualString, expectedString);
                    case Number expectedNumber when actualValue instanceof Number actualNumber ->
                            Assert.assertNotEquals(actualNumber, expectedNumber);
                    default -> Assert.assertNotEquals(actualValue, expectedValue);
                }
            }
            case 2 ->
                // regex comparison
                    Assert.assertFalse((String.valueOf(actualValue)).matches(String.valueOf(expectedValue)));
            case 3 ->
                // contains
                    Assert.assertFalse((String.valueOf(actualValue)).contains(String.valueOf(expectedValue)));
            case 4 ->
                // case insensitive equivalence
                    Assert.assertFalse((String.valueOf(actualValue)).equalsIgnoreCase(String.valueOf(expectedValue)));
            default -> {
                // unhandled case
                return -1;
            }
        }
        return 1;
    }

    public static String formatLocatorToString(By locator) {
        if (locator instanceof RelativeLocator.RelativeBy relativeLocator) {
            return "Relative Locator: " + relativeLocator.getRemoteParameters().value().toString();
        } else {
            return locator.toString();
        }
    }

    public static String convertToSentenceCase(String text) {
        Pattern WORD_FINDER = Pattern.compile("(([A-Z]*[a-z]*)|([A-Z]))");
        Matcher matcher = WORD_FINDER.matcher(text);
        List<String> words = new ArrayList<>();
        while (matcher.find()) {
            words.add(matcher.group(0));
        }
        List<String> capitalized = new ArrayList<>();
        for (int i = 0; i < words.size(); i++) {
            String currentWord = words.get(i);
            if (i == 0) {
                capitalized.add(capitalizeFirst(currentWord));
            } else {
                capitalized.add(currentWord.toLowerCase());
            }
        }
        return String.join(" ", capitalized).trim();
    }

    private static String capitalizeFirst(String word) {
        return word.substring(0, 1).toUpperCase()
                + word.substring(1).toLowerCase();
    }

    public static String appendTestDataToRelativePath(String relativePath) {
        if (FileActions.getInstance(true).doesFileExist(relativePath)) {
            //file path is valid
            return relativePath;
        } else {
            if (relativePath.startsWith("/")) {
                //remove extra slash at the beginning if applicable
                relativePath = relativePath.substring(1);
            }
            var testDataFolderPath = SHAFT.Properties.paths.testData();
            if (relativePath.contains(testDataFolderPath)) {
                return relativePath;
            }
            return testDataFolderPath + relativePath;
        }

    }
}