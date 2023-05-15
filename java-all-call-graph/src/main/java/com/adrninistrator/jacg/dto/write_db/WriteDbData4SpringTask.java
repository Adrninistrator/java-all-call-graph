package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 用于写入数据库的数据，Spring定时任务信息
 */
public class WriteDbData4SpringTask extends AbstractWriteDbData {
    private String springBeanName;
    private String className;
    private String methodName;

    public WriteDbData4SpringTask() {
    }

    public WriteDbData4SpringTask(String springBeanName, String className, String methodName) {
        this.springBeanName = springBeanName;
        this.className = className;
        this.methodName = methodName;
    }

    public String getSpringBeanName() {
        return springBeanName;
    }

    public void setSpringBeanName(String springBeanName) {
        this.springBeanName = springBeanName;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }
}
