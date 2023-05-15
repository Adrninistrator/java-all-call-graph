package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，方法的信息
 */
public class WriteDbData4MethodInfo extends AbstractWriteDbData {
    private String methodHash;
    private String simpleClassName;
    private int accessFlags;
    private String methodName;
    private String fullMethod;
    private String simpleReturnType;
    private String returnType;

    public WriteDbData4MethodInfo() {
    }

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

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public int getAccessFlags() {
        return accessFlags;
    }

    public void setAccessFlags(int accessFlags) {
        this.accessFlags = accessFlags;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getSimpleReturnType() {
        return simpleReturnType;
    }

    public void setSimpleReturnType(String simpleReturnType) {
        this.simpleReturnType = simpleReturnType;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }
}
