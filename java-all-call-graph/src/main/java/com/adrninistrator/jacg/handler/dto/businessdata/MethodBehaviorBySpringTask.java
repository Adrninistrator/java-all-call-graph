package com.adrninistrator.jacg.handler.dto.businessdata;

/**
 * @author adrninistrator
 * @date 2023/9/10
 * @description: Spring Task 对应的类名
 */
public class MethodBehaviorBySpringTask extends MethodBehaviorByBusinessData {

    public static final String TYPE = "SpringTask";

    // 类名
    private final String className;

    public MethodBehaviorBySpringTask(String className) {
        super(TYPE);
        this.className = className;
    }

    public String getClassName() {
        return className;
    }
}
