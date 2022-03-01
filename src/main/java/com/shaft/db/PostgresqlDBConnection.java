package com.shaft.db;

import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManagerHelper;

import java.sql.DriverManager;
import java.sql.SQLException;

public class PostgresqlDBConnection extends DBConnection{

    public PostgresqlDBConnection(String dbServerIp, String dbPort, String dbName, String username, String password) {
        super(dbServerIp, dbPort, dbName, username, password);
    }

    @Override
    public void open() {
        try {
            if (System.getProperty("databaseLoginTimeout") == null) {
                PropertyFileManager.readPropertyFiles();
            }
            DriverManager.setLoginTimeout(Integer.parseInt(System.getProperty("databaseLoginTimeout")));
            connection = DriverManager.getConnection(connectionString, username, password);
        } catch (SQLException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(connectionString, rootCauseException);
        }
        DBLogger.logConnectionStatus(this);
    }

    @Override
    String getString(String dbServerIP, String dbPort, String dbName) {
        return "jdbc:postgresql://" + dbServerIP + ":" + dbPort + ":" + dbName;
    }
}
