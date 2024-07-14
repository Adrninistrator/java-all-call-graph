package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description: 用于写入数据库的数据，类的签名中涉及继承与实现的信息1
 */
public class WriteDbData4ClassSignatureEi1 implements BaseWriteDbData {
    private String simpleClassName;
    private String type;
    private String superItfClassName;
    private int seq;
    private int useClassName;
    private String signClassGenericsName;
    private String className;

    public WriteDbData4ClassSignatureEi1() {
    }

    public WriteDbData4ClassSignatureEi1(String simpleClassName, String type, String superItfClassName, int seq, int useClassName, String signClassGenericsName,
                                         String className) {
        this.simpleClassName = simpleClassName;
        this.type = type;
        this.superItfClassName = superItfClassName;
        this.seq = seq;
        this.useClassName = useClassName;
        this.signClassGenericsName = signClassGenericsName;
        this.className = className;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getSuperItfClassName() {
        return superItfClassName;
    }

    public void setSuperItfClassName(String superItfClassName) {
        this.superItfClassName = superItfClassName;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public int getUseClassName() {
        return useClassName;
    }

    public void setUseClassName(int useClassName) {
        this.useClassName = useClassName;
    }

    public String getSignClassGenericsName() {
        return signClassGenericsName;
    }

    public void setSignClassGenericsName(String signClassGenericsName) {
        this.signClassGenericsName = signClassGenericsName;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }
}
