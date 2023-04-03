package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，继承与实现相关信息
 */
public class WriteDbData4ExtendsImpl extends AbstractWriteDbData {
    private String simpleClassName;
    private String className;
    private int accessFlags;
    private String type;
    private int seq;
    private int existsDownwardClasses;
    private String upwardSimpleClassName;
    private String upwardClassName;

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public int getAccessFlags() {
        return accessFlags;
    }

    public void setAccessFlags(int accessFlags) {
        this.accessFlags = accessFlags;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public int getExistsDownwardClasses() {
        return existsDownwardClasses;
    }

    public void setExistsDownwardClasses(int existsDownwardClasses) {
        this.existsDownwardClasses = existsDownwardClasses;
    }

    public String getUpwardSimpleClassName() {
        return upwardSimpleClassName;
    }

    public void setUpwardSimpleClassName(String upwardSimpleClassName) {
        this.upwardSimpleClassName = upwardSimpleClassName;
    }

    public String getUpwardClassName() {
        return upwardClassName;
    }

    public void setUpwardClassName(String upwardClassName) {
        this.upwardClassName = upwardClassName;
    }
}
