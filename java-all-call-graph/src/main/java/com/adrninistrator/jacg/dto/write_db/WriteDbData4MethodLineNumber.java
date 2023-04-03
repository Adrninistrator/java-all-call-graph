package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 用于写入数据库的数据，方法行号
 */
public class WriteDbData4MethodLineNumber extends AbstractWriteDbData {
    private final String methodHash;
    private final String simpleClassName;
    private final int minLineNumber;
    private final int maxLineNumber;
    private final String fullMethod;

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

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public int getMinLineNumber() {
        return minLineNumber;
    }

    public int getMaxLineNumber() {
        return maxLineNumber;
    }

    public String getFullMethod() {
        return fullMethod;
    }
}
