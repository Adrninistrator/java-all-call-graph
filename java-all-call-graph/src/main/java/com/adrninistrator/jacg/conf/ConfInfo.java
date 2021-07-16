package com.adrninistrator.jacg.conf;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class ConfInfo {

    private String appName;

    private String callGraphJarList;

    private String callGraphInputFile;

    private boolean inputIgnoreOtherPackage;

    private String callGraphOutputDetail;

    private int threadNum;

    private boolean showMethodAnnotation;

    private boolean genCombinedOutput;

    private boolean genUpwardsMethodsFile;

    private String dbDriverName;

    private String dbUrl;

    private String dbUsername;

    private String dbPassword;

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

    public String getCallGraphInputFile() {
        return callGraphInputFile;
    }

    public void setCallGraphInputFile(String callGraphInputFile) {
        this.callGraphInputFile = callGraphInputFile;
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

    public boolean isGenUpwardsMethodsFile() {
        return genUpwardsMethodsFile;
    }

    public void setGenUpwardsMethodsFile(boolean genUpwardsMethodsFile) {
        this.genUpwardsMethodsFile = genUpwardsMethodsFile;
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
