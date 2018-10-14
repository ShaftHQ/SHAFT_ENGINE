package com.shaft.io;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

public class JsonFileManager {


	/**
	 * Creates a new instance of the json file reader using the expected json
	 * file path
	 * 
	 * @param jsFilePath
	 *            the expected path for the target json file
	 */
	public JsonFileManager(String jsFilePath) {
		
		JSONParser parser = new JSONParser();
		
		try {
			 
	            JSONObject jsonObject = (JSONObject) parser.parse(new FileReader(jsFilePath));
	            JSONArray jsonArray = (JSONArray) jsonObject.get("$");
	            System.out.println(jsonArray);
	 
	        } catch (FileNotFoundException e) {
	            e.printStackTrace();
	        } catch (IOException e) {
	            e.printStackTrace();
	        } catch (ParseException e) {
	            e.printStackTrace();
	        }
	 
	    }
}
