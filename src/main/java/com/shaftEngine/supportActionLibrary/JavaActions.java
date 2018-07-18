package com.shaftEngine.supportActionLibrary;

public class JavaActions {
	
/**
 * replaceRegex method takes two parameters, one is the array of special characters that are needed to be replaced, and the text needed to be updated
 * It converts text with @#$%& ..etc (special characters) to return it with \\@\\#\\$\\%\\&
 * @param specialCharactersArray
 * @param text
 * @return updated texts
 */
	public String replaceRegex(String specialCharactersArray[], String text) {
		String oldChar;
		for(int i=0; i<(specialCharactersArray.length); i++)
		{
			oldChar = specialCharactersArray[i];
			specialCharactersArray[i] = ("\\" + specialCharactersArray[i]);
			text = text.replace(oldChar, specialCharactersArray[i]);
		}
		return text;
	}
}
