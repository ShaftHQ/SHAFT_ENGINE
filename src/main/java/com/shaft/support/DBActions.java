package com.shaft.support;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import org.testng.annotations.Test;

public class DBActions {

	@Test
	public void connectMySQL() throws ClassNotFoundException, SQLException {

	Class.forName("com.mysql.jdbc.Driver");
	
	Connection con = DriverManager.getConnection("jdbc:mysql://72.55.136.25:3306/menna","incorta","incorta_123");
	
	Statement stmt = con.createStatement();	
	
	ResultSet QR= stmt.executeQuery("select * from DASHBOARD;");
	
	while(QR.next())
	{
		System.out.print(QR.getString(2)); //or rs.getString("column name");
	}
	
	}
	
}
