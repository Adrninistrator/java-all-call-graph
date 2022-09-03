package com.adrninistrator.jacg.extensions.dto.db_operate;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/9/8
 * @description:
 */
public class DbOperateData {

    private String statement;

    private List<String> tableList;

    private String simpleClassName;

    private String methodName;

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

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }
}
