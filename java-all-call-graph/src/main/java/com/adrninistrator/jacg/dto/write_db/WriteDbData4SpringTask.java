package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 用于写入数据库的数据，Spring定时任务信息
 */
public class WriteDbData4SpringTask extends AbstractWriteDbData {
    private final String springBeanName;
    private final String className;
    private final String methodName;

    public WriteDbData4SpringTask(String springBeanName, String className, String methodName) {
        this.springBeanName = springBeanName;
        this.className = className;
        this.methodName = methodName;
    }

    public String getSpringBeanName() {
        return springBeanName;
    }

    public String getClassName() {
        return className;
    }

    public String getMethodName() {
        return methodName;
    }
}
