package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/3/20
 * @description: 用于写入数据库的数据，方法参数泛型类型
 */
public class WriteDbData4MethodArgGenericsType implements BaseWriteDbData {
    private int recordId;
    private String methodHash;
    private String simpleClassName;
    private int seq;
    private String type;
    private int typeSeq;
    private String simpleGenericsTypeNad;
    private int genericsArrayDimensions;
    private String typeVariablesName;
    private String wildcard;
    private String referenceType;
    private String genericsCategory;
    private String genericsTypeNad;
    private String fullMethod;
    private String returnType;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
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

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
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

    public String getSimpleGenericsTypeNad() {
        return simpleGenericsTypeNad;
    }

    public void setSimpleGenericsTypeNad(String simpleGenericsTypeNad) {
        this.simpleGenericsTypeNad = simpleGenericsTypeNad;
    }

    public int getGenericsArrayDimensions() {
        return genericsArrayDimensions;
    }

    public void setGenericsArrayDimensions(int genericsArrayDimensions) {
        this.genericsArrayDimensions = genericsArrayDimensions;
    }

    public String getTypeVariablesName() {
        return typeVariablesName;
    }

    public void setTypeVariablesName(String typeVariablesName) {
        this.typeVariablesName = typeVariablesName;
    }

    public String getWildcard() {
        return wildcard;
    }

    public void setWildcard(String wildcard) {
        this.wildcard = wildcard;
    }

    public String getReferenceType() {
        return referenceType;
    }

    public void setReferenceType(String referenceType) {
        this.referenceType = referenceType;
    }

    public String getGenericsCategory() {
        return genericsCategory;
    }

    public void setGenericsCategory(String genericsCategory) {
        this.genericsCategory = genericsCategory;
    }

    public String getGenericsTypeNad() {
        return genericsTypeNad;
    }

    public void setGenericsTypeNad(String genericsTypeNad) {
        this.genericsTypeNad = genericsTypeNad;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }
}
