package com.adrninistrator.jacg.extensions.methodcall;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.dto.call.MethodCall;
import com.adrninistrator.javacg2.dto.call.MethodCallList;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.extensions.methodcall.JavaCG2MethodCallExtensionInterface;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description: java-callgraph2 组件方法调用处理扩展类
 * 通过方法调用参数添加方法调用，使用被调用对象，及被调用方法名称
 */
public abstract class JavaCG2ObjInstanceMethodNameMCE implements JavaCG2MethodCallExtensionInterface {

    private static final Logger logger = LoggerFactory.getLogger(JavaCG2ObjInstanceMethodNameMCE.class);

    public JavaCG2ObjInstanceMethodNameMCE() {
        String methodCallType = chooseMethodCallType();
        JavaCG2CallTypeEnum callTypeEnum = JavaCG2CallTypeEnum.getFromType(methodCallType);
        if (callTypeEnum != null) {
            logger.error("不允许使用已存在的方法调用类型 {} {}", this.getClass().getSimpleName(), methodCallType);
            throw new JavaCG2RuntimeException("不允许使用已存在的方法调用类型");
        }
    }

    @Override
    public void handle(MethodCall methodCall, JavaCG2Counter javaCG2Counter, MethodCallList methodCallList) {
        if (!checkHandleCalleeMethod(methodCall.getCalleeClassName(), methodCall.getCalleeMethodName(), methodCall.getCalleeMethodArgTypes())) {
            // 被调用方法不需要处理
            return;
        }

        MethodCall newMethodCall = new MethodCall();
        newMethodCall.setEnabled(true);
        newMethodCall.setCallerClassName(methodCall.getCallerClassName());
        newMethodCall.setCallerMethodName(methodCall.getCallerMethodName());
        newMethodCall.setCallerMethodArgTypes(methodCall.getCallerMethodArgTypes());
        newMethodCall.setCallerSourceLine(methodCall.getCallerSourceLine());
        newMethodCall.setCallerReturnType(methodCall.getCallerReturnType());
        newMethodCall.setMethodCallType(chooseMethodCallType());
        newMethodCall.setCalleeClassName(JACGConstants.CLASS_PLACE_HOLDER);
        newMethodCall.setCalleeMethodName(JACGConstants.METHOD_PLACE_HOLDER);
        newMethodCall.setCalleeMethodArgTypes(JavaCG2Constants.EMPTY_METHOD_ARGS);
        newMethodCall.setRawReturnType("");
        newMethodCall.setActualReturnType("");
        newMethodCall.setDescription(String.valueOf(methodCall.getCallId()));
        methodCallList.addMethodCallAutoCallId(newMethodCall);
    }

    /**
     * 判断当前被调用方法是否需要处理
     *
     * @param calleeClassName      被调用类名
     * @param calleeMethodName     被调用方法名
     * @param calleeMethodArgTypes 被调用方法参数类型，含括号，参数类型以半角逗号分隔，不含空格，如：()、(int)、(java.lang.String,int)
     * @return
     */
    protected abstract boolean checkHandleCalleeMethod(String calleeClassName, String calleeMethodName, String calleeMethodArgTypes);

    /**
     * 选择需要替换的方法调用类型，需要唯一
     * @return
     */
    protected abstract String chooseMethodCallType();
}
