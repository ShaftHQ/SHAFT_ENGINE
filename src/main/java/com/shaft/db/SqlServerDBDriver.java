package com.shaft.db;

public class SqlServerDBDriver  extends DBConnection{

    protected SqlServerDBDriver(String dbServerIp, String dbPort, String dbName, String username, String password) {
        super(dbServerIp, dbPort, dbName, username, password);
    }

    @Override
    String getString(String dbServerIP, String dbPort, String dbName) {
        return "jdbc:sqlserver://" + dbServerIP + ":" + dbPort + ";databaseName=" + dbName;
    }
}
