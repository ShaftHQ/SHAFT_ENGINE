package com.shaft.db;

import com.shaft.tools.io.ReportManagerHelper;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import java.sql.ResultSet;
import java.sql.SQLException;

public class  DBDriver {

    DBConnection dbConnection ;

    public DBDriver(DBConnection dbConnection) {
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

    public int executeCommand(String sql) {
        var updatedRows = 0;
        dbConnection.open();
        try {updatedRows = dbConnection.getStatement().executeUpdate(sql);
            DBReporter.passAction(sql);
        } catch (SQLException | NullPointerException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(DBLogger.getReportMessage("UPDATE", sql), rootCauseException);
        }
        dbConnection.close();
        return updatedRows;
    }

    @Override
    public String toString() {
        return ToStringBuilder.reflectionToString(this, ToStringStyle.SHORT_PREFIX_STYLE);
    }

}
