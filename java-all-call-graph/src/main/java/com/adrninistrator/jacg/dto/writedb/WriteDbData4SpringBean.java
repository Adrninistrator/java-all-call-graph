package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，Spring Bean信息
 */
public class WriteDbData4SpringBean implements BaseWriteDbData {
    private String springBeanName;
    private int seq;
    private String className;
    private String beanType;

    public WriteDbData4SpringBean() {
    }

    public WriteDbData4SpringBean(String springBeanName, int seq, String className, String beanType) {
        this.springBeanName = springBeanName;
        this.seq = seq;
        this.className = className;
        this.beanType = beanType;
    }

    public String getSpringBeanName() {
        return springBeanName;
    }

    public void setSpringBeanName(String springBeanName) {
        this.springBeanName = springBeanName;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getBeanType() {
        return beanType;
    }

    public void setBeanType(String beanType) {
        this.beanType = beanType;
    }
}
