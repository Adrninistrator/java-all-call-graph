package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description: 用于写入数据库的数据，方法的参数类型
 */
public class WriteDbData4MethodArgType extends AbstractWriteDbData {
    private String methodHash;
    private Integer argSeq;
    private String simpleArgType;
    private String argType;
    private String simpleClassName;
    private String fullMethod;

    public WriteDbData4MethodArgType() {
    }

    public WriteDbData4MethodArgType(String methodHash, Integer argSeq, String simpleArgType, String argType, String simpleClassName, String fullMethod) {
        this.methodHash = methodHash;
        this.argSeq = argSeq;
        this.simpleArgType = simpleArgType;
        this.argType = argType;
        this.simpleClassName = simpleClassName;
        this.fullMethod = fullMethod;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public Integer getArgSeq() {
        return argSeq;
    }

    public void setArgSeq(Integer argSeq) {
        this.argSeq = argSeq;
    }

    public String getSimpleArgType() {
        return simpleArgType;
    }

    public void setSimpleArgType(String simpleArgType) {
        this.simpleArgType = simpleArgType;
    }

    public String getArgType() {
        return argType;
    }

    public void setArgType(String argType) {
        this.argType = argType;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }
}
