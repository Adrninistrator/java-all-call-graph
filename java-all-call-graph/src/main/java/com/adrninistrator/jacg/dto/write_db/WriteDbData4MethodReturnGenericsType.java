package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/4/13
 * @description: 用于写入数据库的数据，方法返回泛型类型
 */
public class WriteDbData4MethodReturnGenericsType extends AbstractWriteDbData {
    private final String methodHash;
    private final String simpleClassName;
    private final String type;
    private final int typeSeq;
    private final String simpleGenericsType;
    private final String genericsType;
    private final String fullMethod;

    public WriteDbData4MethodReturnGenericsType(String methodHash, String simpleClassName, String type, int typeSeq, String simpleGenericsType, String genericsType,
                                                String fullMethod) {
        this.methodHash = methodHash;
        this.simpleClassName = simpleClassName;
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
