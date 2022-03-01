package com.shaft.db;

public class Ibmdb2DBConnection extends DBConnection{

    protected Ibmdb2DBConnection(String dbServerIp, String dbPort, String dbName, String username, String password) {
        super(dbServerIp, dbPort, dbName, username, password);
    }

    @Override
     String getString(String dbServerIP, String dbPort, String dbName) {
        return "jdbc:db2://" + dbServerIP + ":" + dbPort + "/" + dbName;
    }
}
