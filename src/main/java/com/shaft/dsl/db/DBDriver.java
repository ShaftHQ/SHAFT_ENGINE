package com.shaft.dsl.db;

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
    public ResultSet executeSelectQuery(String sql) {
        dbConnection.open();
        ResultSet resultSet = null;
        try {resultSet =dbConnection.getStatement().executeQuery(sql);}
        catch (SQLException | NullPointerException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(DBLogger.getReportMessage("SELECT", sql), rootCauseException);
        }
        DBLogger.logSelectStmtResultStatus(sql, resultSet);
        dbConnection.close();
        return resultSet;
    }

    public int executeUpdateQuery(String sql) {
        var updatedRows = 0;
        try {updatedRows = dbConnection.getStatement().executeUpdate(sql);
            DBReporter.passAction(sql);
        } catch (SQLException | NullPointerException rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            DBReporter.failAction(DBLogger.getReportMessage("UPDATE", sql), rootCauseException);
        }
        return updatedRows;
    }

    @Override
    public String toString() {
        return ToStringBuilder.reflectionToString(this, ToStringStyle.SHORT_PREFIX_STYLE);
    }

}
