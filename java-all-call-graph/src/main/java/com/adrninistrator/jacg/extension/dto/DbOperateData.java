package com.adrninistrator.jacg.extension.dto;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/9/8
 * @description:
 */
public class DbOperateData {

    private String statement;

    private List<String> tableList;

    public String getStatement() {
        return statement;
    }

    public void setStatement(String statement) {
        this.statement = statement;
    }

    public List<String> getTableList() {
        return tableList;
    }

    public void setTableList(List<String> tableList) {
        this.tableList = tableList;
    }
}
