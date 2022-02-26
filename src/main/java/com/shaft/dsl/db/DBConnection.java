package com.shaft.dsl.db;

import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManagerHelper;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.sql.*;
import java.util.concurrent.Executors;

public abstract class DBConnection {

    String username;
    String password;
    Connection connection = null;
    String connectionString ;

    protected DBConnection(String dbServerIp, String dbPort, String dbName, String username, String password) {
        this.username = username;
        this.password = password;
        this.connectionString = getString(dbServerIp, dbPort, dbName);
    }

    public void open() {
        try {
            if (System.getProperty("databaseLoginTimeout") == null) {
                PropertyFileManager.readPropertyFiles();
            }
            DriverManager.setLoginTimeout(Integer.parseInt(System.getProperty("databaseLoginTimeout")));
            connection = DriverManager.getConnection(connectionString, username, password);
            connection.setNetworkTimeout(Executors.newFixedThreadPool(1),
                    Integer.parseInt(System.getProperty("databaseNetworkTimeout")) * 60000);
        } catch (SQLException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(connectionString, rootCauseException);
        }
        DBLogger.logConnectionStatus(this);
    }

    protected void close() {
        if (connection == null) return;
        try {
            connection.close();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    abstract String getString(String dbServerIP, String dbPort, String dbName);

    public Statement getStatement() {
        Statement statement = null;
        try {
            statement = connection.createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY);
            // https://www.tutorialspoint.com/jdbc/jdbc-result-sets.htm
            statement.setQueryTimeout(Integer.parseInt(System.getProperty("databaseQueryTimeout")));
        } catch (SQLFeatureNotSupportedException rootCauseException) {
            if (!rootCauseException.getMessage().contains("org.postgresql.jdbc4.Jdbc4Statement.setQueryTimeout")) {
                ReportManagerHelper.log(rootCauseException);
                DBReporter.failAction(connection.toString(), rootCauseException);
            }
        } catch (SQLException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(toString(), rootCauseException);
        }
        DBLogger.logResultStatus(connection, statement);
        return statement;
    }

    @Override
    public String toString() {
        return ToStringBuilder.reflectionToString(this, ToStringStyle.SHORT_PREFIX_STYLE);
    }

}
