package com.adrninistrator.jacg.handler.dto.spring;

/**
 * @author adrninistrator
 * @date 2023/3/6
 * @description: Spring非法的事务注解方法
 */
public class SpringInvalidTxAnnotationMethod {

    // 完整方法（@Transactional注解所在方法）
    private final String fullMethod;

    // 方法返回类型（@Transactional注解所在方法）
    private final String returnType;

    // 方法标志
    private final String methodFlagsDesc;

    public SpringInvalidTxAnnotationMethod(String fullMethod, String returnType, String methodFlagsDesc) {
        this.fullMethod = fullMethod;
        this.returnType = returnType;
        this.methodFlagsDesc = methodFlagsDesc;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public String getReturnType() {
        return returnType;
    }

    public String getMethodFlagsDesc() {
        return methodFlagsDesc;
    }
}
