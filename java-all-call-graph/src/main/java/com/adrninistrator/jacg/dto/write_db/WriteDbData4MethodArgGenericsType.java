package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/3/20
 * @description: 用于写入数据库的数据，方法参数泛型类型
 */
public class WriteDbData4MethodArgGenericsType extends AbstractWriteDbData {
    private final String methodHash;
    private final String simpleClassName;
    private final int argSeq;
    private final String type;
    private final int typeSeq;
    private final String simpleGenericsType;
    private final String genericsType;
    private final String fullMethod;

    public WriteDbData4MethodArgGenericsType(String methodHash, String simpleClassName, int argSeq, String type, int typeSeq, String simpleGenericsType, String genericsType,
                                             String fullMethod) {
        this.methodHash = methodHash;
        this.simpleClassName = simpleClassName;
        this.argSeq = argSeq;
        this.type = type;
        this.typeSeq = typeSeq;
        this.simpleGenericsType = simpleGenericsType;
        this.genericsType = genericsType;
        this.fullMethod = fullMethod;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public int getArgSeq() {
        return argSeq;
    }

    public String getType() {
        return type;
    }

    public int getTypeSeq() {
        return typeSeq;
    }

    public String getSimpleGenericsType() {
        return simpleGenericsType;
    }

    public String getGenericsType() {
        return genericsType;
    }

    public String getFullMethod() {
        return fullMethod;
    }
}
