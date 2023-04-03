package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，Spring Bean信息
 */
public class WriteDbData4SpringBean extends AbstractWriteDbData {    private final String springBeanName;
    private final int seq;
    private final String className;

    public WriteDbData4SpringBean(String springBeanName, int seq, String className) {
        this.springBeanName = springBeanName;
        this.seq = seq;
        this.className = className;
    }

    public String getSpringBeanName() {
        return springBeanName;
    }

    public int getSeq() {
        return seq;
    }

    public String getClassName() {
        return className;
    }
}
