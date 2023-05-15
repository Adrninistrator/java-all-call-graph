package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 用于写入数据库的数据，方法行号
 */
public class WriteDbData4MethodLineNumber extends AbstractWriteDbData {
    private String methodHash;
    private String simpleClassName;
    private int minLineNumber;
    private int maxLineNumber;
    private String fullMethod;

    public WriteDbData4MethodLineNumber() {
    }

    public WriteDbData4MethodLineNumber(String methodHash, String simpleClassName, int minLineNumber, int maxLineNumber, String fullMethod) {
        this.methodHash = methodHash;
        this.simpleClassName = simpleClassName;
        this.minLineNumber = minLineNumber;
        this.maxLineNumber = maxLineNumber;
        this.fullMethod = fullMethod;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public int getMinLineNumber() {
        return minLineNumber;
    }

    public void setMinLineNumber(int minLineNumber) {
        this.minLineNumber = minLineNumber;
    }

    public int getMaxLineNumber() {
        return maxLineNumber;
    }

    public void setMaxLineNumber(int maxLineNumber) {
        this.maxLineNumber = maxLineNumber;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }
}
