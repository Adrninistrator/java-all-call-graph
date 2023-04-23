package com.adrninistrator.jacg.handler.dto.mybatis;

import com.adrninistrator.mybatis_mysql_table_parser.common.enums.MySqlStatementEnum;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/21
 * @description: MyBatis中操作的数据库语句及表信息（支持MySQL）
 */
public class MyBatisMySqlStatementAndTable {
    private final MySqlStatementEnum mySqlStatementEnum;

    private final List<String> tableList;

    public MyBatisMySqlStatementAndTable(MySqlStatementEnum mySqlStatementEnum, List<String> tableList) {
        this.mySqlStatementEnum = mySqlStatementEnum;
        this.tableList = tableList;
    }

    public MySqlStatementEnum getMySqlStatementEnum() {
        return mySqlStatementEnum;
    }

    public List<String> getTableList() {
        return tableList;
    }
}
