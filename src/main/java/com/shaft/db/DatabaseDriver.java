package com.shaft.db;

import com.shaft.tools.io.ReportManagerHelper;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import java.sql.ResultSet;
import java.sql.SQLException;

public class DatabaseDriver {

    DBConnection dbConnection ;

    public DatabaseDriver(DBConnection dbConnection) {
       this.dbConnection =dbConnection;
       dbConnection.open();
       dbConnection.close();
    }
    public ResultObject executeQuery(String sql) {
        ResultSet resultSet = null;
        dbConnection.open();
        try {resultSet =dbConnection.getStatement().executeQuery(sql);}
        catch (SQLException | NullPointerException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(DBLogger.getReportMessage("SELECT", sql), rootCauseException);
        }
        dbConnection.close();
        DBLogger.logSelectStmtResultStatus(sql, resultSet);
        ResultObject resultObject= new ResultObject(resultSet);
        return resultObject;
    }

    private int executeCommand(String sql, String queryType) {
        var updatedRows = 0;
        dbConnection.open();
        try {updatedRows = dbConnection.getStatement().executeUpdate(sql);
            DBReporter.passAction(sql);
        } catch (SQLException | NullPointerException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(DBLogger.getReportMessage(queryType, sql), rootCauseException);
            //to be updated
        }
        dbConnection.close();
        return updatedRows;
    }

    public int executeUpdateQuery(String sql) {
        return executeCommand(sql,"UPDATE");
    }

    public int executeInsertQuery(String sql) {
        return executeCommand(sql,"INSERT");

    }

    public int executeDeleteQuery(String sql) {
        return executeCommand(sql,"DELETE");

    }

    @Override
    public String toString() {
        return ToStringBuilder.reflectionToString(this, ToStringStyle.SHORT_PREFIX_STYLE);
    }

}
