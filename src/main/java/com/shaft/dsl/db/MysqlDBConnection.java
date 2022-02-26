package com.shaft.dsl.db;

import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManagerHelper;

import java.sql.DriverManager;
import java.sql.SQLException;

public class MysqlDBConnection extends DBConnection{
    public MysqlDBConnection(String dbServerIp, String dbPort, String dbName, String username, String password) {
        super(dbServerIp, dbPort, dbName, username, password);
    }

    @Override
     String getString(String dbServerIP, String dbPort, String dbName) {
        return  "jdbc:mysql://" + dbServerIP + ":" + dbPort + "/" + dbName;
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
}
