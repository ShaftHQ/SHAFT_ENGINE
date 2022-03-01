package com.shaft.db;


public class OracleDBConnection extends DBConnection{
    public OracleDBConnection(String dbServerIp, String dbPort, String dbName, String username, String password) {
        super(dbServerIp, dbPort, dbName, username, password);
    }

    String getString(String dbServerIP, String dbPort, String dbName) {
        return "jdbc:oracle:thin:@" + dbServerIP + ":" + dbPort + ":" + dbName;
    }

}
