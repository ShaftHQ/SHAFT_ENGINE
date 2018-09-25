package com.shaft.support;

import java.util.Base64;

public class JavaActions {

	private JavaActions() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Takes two parameters, one is the array of special characters that are needed
	 * to be replaced, and the text needed to be updated It converts text with @#$%&
	 * ..etc (special characters) to return it with \\@\\#\\$\\%\\&
	 * 
	 * @param specialCharactersArray
	 * @param text
	 * @return updated texts
	 */
	public static String replaceRegex(String[] specialCharactersArray, String text) {
		String oldChar;
		for (int i = 0; i < (specialCharactersArray.length); i++) {
			oldChar = specialCharactersArray[i];
			specialCharactersArray[i] = ("\\" + specialCharactersArray[i]);
			text = text.replace(oldChar, specialCharactersArray[i]);
		}
		return text;
	}

	/**
	 * Returns text after replaces its regular expressions which included in this
	 * set []^$.|?*+(){}
	 * 
	 * @param text
	 * @return updated text after escaping its regular expressions
	 */

	public static String replaceRegex(String text) {
		String[] specialCharactersArray = { "[", "]", "^", "$", ".", "|", "?", "*", "+", "(", ")", "{", "}" };
		return replaceRegex(specialCharactersArray, text);
	}

	public static String convertBase64(String text) {
		return Base64.getEncoder().encodeToString(text.getBytes());
	}
}
