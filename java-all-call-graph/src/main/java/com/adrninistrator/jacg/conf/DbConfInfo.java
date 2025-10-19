package com.adrninistrator.jacg.conf;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2024/3/16
 * @description: 数据库相关配置参数
 */

public class DbConfInfo {

    private boolean useH2Db;
    private String dbH2FilePath;
    private String driverClassName;
    private String dbUrl;
    private String username;
    private String password;
    private String appName;
    private String tableSuffix;
    private boolean slowQuerySwitch;
    private int slowQueryTime;
    private int slowQueryRowNum;
    private int maxActive;
    private int dbInsertBatchSize;
    private boolean h2DbReadOnly = false;

    // equals不比较 slowQuerySwitch、slowQueryTime、slowQueryRowNum、maxActive、dbInsertBatchSize
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DbConfInfo that = (DbConfInfo) o;
        return useH2Db == that.useH2Db
                && Objects.equals(dbH2FilePath, that.dbH2FilePath)
                && Objects.equals(driverClassName, that.driverClassName)
                && Objects.equals(dbUrl, that.dbUrl)
                && Objects.equals(username, that.username)
                && Objects.equals(appName, that.appName)
                && Objects.equals(tableSuffix, that.tableSuffix);
    }

    // 获取用于显示的数据库信息
    public String getShowDbInfo() {
        if (useH2Db) {
            return dbH2FilePath;
        }
        return dbUrl + " " + username;
    }

    @Override
    public String toString() {
        return "DbConfInfo{" +
                "useH2Db=" + useH2Db +
                ", dbH2FilePath='" + dbH2FilePath + '\'' +
                ", driverClassName='" + driverClassName + '\'' +
                ", dbUrl='" + dbUrl + '\'' +
                ", username='" + username + '\'' +
                ", appName='" + appName + '\'' +
                ", tableSuffix='" + tableSuffix + '\'' +
                ", maxActive=" + maxActive +
                ", dbInsertBatchSize=" + dbInsertBatchSize +
                ", h2DbReadOnly=" + h2DbReadOnly +
                '}';
    }

    public boolean isUseH2Db() {
        return useH2Db;
    }

    public void setUseH2Db(boolean useH2Db) {
        this.useH2Db = useH2Db;
    }

    public String getDbH2FilePath() {
        return dbH2FilePath;
    }

    public void setDbH2FilePath(String dbH2FilePath) {
        this.dbH2FilePath = dbH2FilePath;
    }

    public String getDriverClassName() {
        return driverClassName;
    }

    public void setDriverClassName(String driverClassName) {
        this.driverClassName = driverClassName;
    }

    public String getDbUrl() {
        return dbUrl;
    }

    public void setDbUrl(String dbUrl) {
        this.dbUrl = dbUrl;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getAppName() {
        return appName;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public String getTableSuffix() {
        return tableSuffix;
    }

    public void setTableSuffix(String tableSuffix) {
        this.tableSuffix = tableSuffix;
    }

    public boolean isSlowQuerySwitch() {
        return slowQuerySwitch;
    }

    public void setSlowQuerySwitch(boolean slowQuerySwitch) {
        this.slowQuerySwitch = slowQuerySwitch;
    }

    public int getSlowQueryTime() {
        return slowQueryTime;
    }

    public void setSlowQueryTime(int slowQueryTime) {
        this.slowQueryTime = slowQueryTime;
    }

    public int getSlowQueryRowNum() {
        return slowQueryRowNum;
    }

    public void setSlowQueryRowNum(int slowQueryRowNum) {
        this.slowQueryRowNum = slowQueryRowNum;
    }

    public int getMaxActive() {
        return maxActive;
    }

    public void setMaxActive(int maxActive) {
        this.maxActive = maxActive;
    }

    public int getDbInsertBatchSize() {
        return dbInsertBatchSize;
    }

    public void setDbInsertBatchSize(int dbInsertBatchSize) {
        this.dbInsertBatchSize = dbInsertBatchSize;
    }

    public boolean isH2DbReadOnly() {
        return h2DbReadOnly;
    }

    public void setH2DbReadOnly(boolean h2DbReadOnly) {
        this.h2DbReadOnly = h2DbReadOnly;
    }
}
