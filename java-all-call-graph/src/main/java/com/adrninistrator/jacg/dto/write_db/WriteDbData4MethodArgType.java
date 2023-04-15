package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description: 用于写入数据库的数据，方法的参数类型
 */
public class WriteDbData4MethodArgType extends AbstractWriteDbData {
    private final String methodHash;
    private final Integer argSeq;
    private final String simpleArgType;
    private final String argType;
    private final String simpleClassName;
    private final String fullMethod;

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

    public Integer getArgSeq() {
        return argSeq;
    }

    public String getSimpleArgType() {
        return simpleArgType;
    }

    public String getArgType() {
        return argType;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public String getFullMethod() {
        return fullMethod;
    }
}
