package com.adrninistrator.jacg.conf;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description: 配置信息
 */

public class ConfInfo {

    private String appName;

    private String callGraphOutputDetail;

    private int threadNum;

    private int originalThreadNum;

    private boolean ignoreDupCalleeInOneCaller;

    private String outputRootPath;

    private int dbInsertBatchSize;

    private boolean checkJarFileUpdated;

    // 以下为数据库相关配置
    private boolean dbUseH2;

    private String dbH2FilePath;

    private String dbDriverName;

    private String dbUrl;

    private String dbUsername;

    private String dbPassword;

    //
    public String getAppName() {
        return appName;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public String getCallGraphOutputDetail() {
        return callGraphOutputDetail;
    }

    public void setCallGraphOutputDetail(String callGraphOutputDetail) {
        this.callGraphOutputDetail = callGraphOutputDetail;
    }

    public int getThreadNum() {
        return threadNum;
    }

    public void setThreadNum(int threadNum) {
        this.threadNum = threadNum;
    }

    public int getOriginalThreadNum() {
        return originalThreadNum;
    }

    public void setOriginalThreadNum(int originalThreadNum) {
        this.originalThreadNum = originalThreadNum;
    }

    public boolean isIgnoreDupCalleeInOneCaller() {
        return ignoreDupCalleeInOneCaller;
    }

    public void setIgnoreDupCalleeInOneCaller(boolean ignoreDupCalleeInOneCaller) {
        this.ignoreDupCalleeInOneCaller = ignoreDupCalleeInOneCaller;
    }

    public String getOutputRootPath() {
        return outputRootPath;
    }

    public void setOutputRootPath(String outputRootPath) {
        this.outputRootPath = outputRootPath;
    }

    public int getDbInsertBatchSize() {
        return dbInsertBatchSize;
    }

    public void setDbInsertBatchSize(int dbInsertBatchSize) {
        this.dbInsertBatchSize = dbInsertBatchSize;
    }

    public boolean isCheckJarFileUpdated() {
        return checkJarFileUpdated;
    }

    public void setCheckJarFileUpdated(boolean checkJarFileUpdated) {
        this.checkJarFileUpdated = checkJarFileUpdated;
    }

    public boolean isDbUseH2() {
        return dbUseH2;
    }

    public void setDbUseH2(boolean dbUseH2) {
        this.dbUseH2 = dbUseH2;
    }

    public String getDbH2FilePath() {
        return dbH2FilePath;
    }

    public void setDbH2FilePath(String dbH2FilePath) {
        this.dbH2FilePath = dbH2FilePath;
    }

    public String getDbDriverName() {
        return dbDriverName;
    }

    public void setDbDriverName(String dbDriverName) {
        this.dbDriverName = dbDriverName;
    }

    public String getDbUrl() {
        return dbUrl;
    }

    public void setDbUrl(String dbUrl) {
        this.dbUrl = dbUrl;
    }

    public String getDbUsername() {
        return dbUsername;
    }

    public void setDbUsername(String dbUsername) {
        this.dbUsername = dbUsername;
    }

    public String getDbPassword() {
        return dbPassword;
    }

    public void setDbPassword(String dbPassword) {
        this.dbPassword = dbPassword;
    }
}
