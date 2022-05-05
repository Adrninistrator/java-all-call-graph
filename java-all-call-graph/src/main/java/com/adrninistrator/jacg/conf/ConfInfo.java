package com.adrninistrator.jacg.conf;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class ConfInfo {

    private String appName;

    private String callGraphJarList;

    private boolean inputIgnoreOtherPackage;

    private String callGraphOutputDetail;

    private int threadNum;

    private int originalThreadNum;

    private boolean showMethodAnnotation;

    private boolean genCombinedOutput;

    private boolean showCallerLineNum;

    private boolean ignoreDupCalleeInOneCaller;

    private boolean dbUseH2;

    private String dbH2FilePath;

    private String dbDriverName;

    private String dbUrl;

    private String dbUsername;

    private String dbPassword;

    private boolean writeConf;

    @Override
    public String toString() {
        return "ConfInfo{" +
                "appName='" + appName + '\'' +
                ", callGraphJarList='" + callGraphJarList + '\'' +
                ", inputIgnoreOtherPackage=" + inputIgnoreOtherPackage +
                ", callGraphOutputDetail='" + callGraphOutputDetail + '\'' +
                ", threadNum=" + threadNum +
                ", originalThreadNum=" + originalThreadNum +
                ", showMethodAnnotation=" + showMethodAnnotation +
                ", genCombinedOutput=" + genCombinedOutput +
                ", showCallerLineNum=" + showCallerLineNum +
                ", ignoreDupCalleeInOneCaller=" + ignoreDupCalleeInOneCaller +
                ", dbUseH2=" + dbUseH2 +
                ", dbH2FilePath='" + dbH2FilePath + '\'' +
                ", dbDriverName='" + dbDriverName + '\'' +
                ", dbUrl='" + dbUrl + '\'' +
                ", dbUsername='" + dbUsername + '\'' +
                ", dbPassword='" + dbPassword + '\'' +
                ", writeConf=" + writeConf +
                '}';
    }

    //
    public String getAppName() {
        return appName;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public String getCallGraphJarList() {
        return callGraphJarList;
    }

    public void setCallGraphJarList(String callGraphJarList) {
        this.callGraphJarList = callGraphJarList;
    }

    public boolean isInputIgnoreOtherPackage() {
        return inputIgnoreOtherPackage;
    }

    public void setInputIgnoreOtherPackage(boolean inputIgnoreOtherPackage) {
        this.inputIgnoreOtherPackage = inputIgnoreOtherPackage;
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

    public boolean isShowMethodAnnotation() {
        return showMethodAnnotation;
    }

    public void setShowMethodAnnotation(boolean showMethodAnnotation) {
        this.showMethodAnnotation = showMethodAnnotation;
    }

    public boolean isGenCombinedOutput() {
        return genCombinedOutput;
    }

    public void setGenCombinedOutput(boolean genCombinedOutput) {
        this.genCombinedOutput = genCombinedOutput;
    }

    public boolean isShowCallerLineNum() {
        return showCallerLineNum;
    }

    public void setShowCallerLineNum(boolean showCallerLineNum) {
        this.showCallerLineNum = showCallerLineNum;
    }

    public boolean isIgnoreDupCalleeInOneCaller() {
        return ignoreDupCalleeInOneCaller;
    }

    public void setIgnoreDupCalleeInOneCaller(boolean ignoreDupCalleeInOneCaller) {
        this.ignoreDupCalleeInOneCaller = ignoreDupCalleeInOneCaller;
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

    public boolean isWriteConf() {
        return writeConf;
    }

    public void setWriteConf(boolean writeConf) {
        this.writeConf = writeConf;
    }
}
