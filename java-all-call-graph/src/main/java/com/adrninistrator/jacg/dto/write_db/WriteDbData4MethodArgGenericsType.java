package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/3/20
 * @description: 用于写入数据库的数据，方法参数泛型类型
 */
public class WriteDbData4MethodArgGenericsType extends AbstractWriteDbData {
    private String methodHash;
    private String simpleClassName;
    private int argSeq;
    private String type;
    private int typeSeq;
    private String simpleGenericsType;
    private String genericsType;
    private String fullMethod;

    public WriteDbData4MethodArgGenericsType() {
    }

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

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public int getArgSeq() {
        return argSeq;
    }

    public void setArgSeq(int argSeq) {
        this.argSeq = argSeq;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getTypeSeq() {
        return typeSeq;
    }

    public void setTypeSeq(int typeSeq) {
        this.typeSeq = typeSeq;
    }

    public String getSimpleGenericsType() {
        return simpleGenericsType;
    }

    public void setSimpleGenericsType(String simpleGenericsType) {
        this.simpleGenericsType = simpleGenericsType;
    }

    public String getGenericsType() {
        return genericsType;
    }

    public void setGenericsType(String genericsType) {
        this.genericsType = genericsType;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }
}
