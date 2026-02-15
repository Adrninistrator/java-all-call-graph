package com.adrninistrator.jacg.extensions.methodcall;

import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2026/02/13
 * @description: java-callgraph2 组件方法调用处理扩展类 - 反射调用
 * 识别 java.lang.reflect.Method.invoke 调用，写入占位方法调用
 */
public class JavaCG2ReflectionMethodCallExtension extends JavaCG2ObjInstanceMethodNameMCE {

    public static final String TYPE = "Reflection_invoke";

    // Method.invoke 方法的参数类型
    private static final String METHOD_INVOKE_ARG_TYPES = "(java.lang.Object,java.lang.Object[])";

    @Override
    protected boolean checkHandleCalleeMethod(String calleeClassName, String calleeMethodName, String calleeMethodArgTypes) {
        // 判断是否为 Method.invoke 方法调用
        return "java.lang.reflect.Method".equals(calleeClassName) &&
                "invoke".equals(calleeMethodName) &&
                StringUtils.equals(METHOD_INVOKE_ARG_TYPES, calleeMethodArgTypes);
    }

    @Override
    protected String chooseMethodCallType() {
        return TYPE;
    }
}
