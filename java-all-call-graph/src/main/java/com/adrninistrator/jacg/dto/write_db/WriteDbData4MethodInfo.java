package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，方法的信息
 */
public class WriteDbData4MethodInfo extends AbstractWriteDbData {
    private final String methodHash;
    private final String simpleClassName;
    private final int accessFlags;
    private final String methodName;
    private final String fullMethod;
    private final String simpleReturnType;
    private final String returnType;

    public WriteDbData4MethodInfo(String methodHash, String simpleClassName, int accessFlags, String methodName, String fullMethod, String simpleReturnType, String returnType) {
        this.methodHash = methodHash;
        this.simpleClassName = simpleClassName;
        this.accessFlags = accessFlags;
        this.methodName = methodName;
        this.fullMethod = fullMethod;
        this.simpleReturnType = simpleReturnType;
        this.returnType = returnType;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public int getAccessFlags() {
        return accessFlags;
    }

    public String getMethodName() {
        return methodName;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public String getSimpleReturnType() {
        return simpleReturnType;
    }

    public String getReturnType() {
        return returnType;
    }
}
