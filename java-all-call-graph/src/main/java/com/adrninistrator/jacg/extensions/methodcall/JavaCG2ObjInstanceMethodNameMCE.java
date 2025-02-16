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
        if (!methodCall.getCalleeClassName().equals(chooseCalleeClassName()) ||
                !methodCall.getCalleeMethodName().equals(chooseCalleeMethodName())) {
            // 被调用方法不需要处理
            return;
        }

        MethodCall newmethodCall = new MethodCall();
        newmethodCall.setEnabled(true);
        newmethodCall.setCallerClassName(methodCall.getCallerClassName());
        newmethodCall.setCallerMethodName(methodCall.getCallerMethodName());
        newmethodCall.setCallerMethodArgTypes(methodCall.getCallerMethodArgTypes());
        newmethodCall.setCallerSourceLine(methodCall.getCallerSourceLine());
        newmethodCall.setCallerReturnType(methodCall.getCallerReturnType());
        newmethodCall.setMethodCallType(chooseMethodCallType());
        newmethodCall.setCalleeClassName(JACGConstants.CLASS_PLACE_HOLDER);
        newmethodCall.setCalleeMethodName(JACGConstants.METHOD_PLACE_HOLDER);
        newmethodCall.setCalleeMethodArgTypes(JavaCG2Constants.EMPTY_METHOD_ARGS);
        newmethodCall.setRawReturnType("");
        newmethodCall.setActualReturnType("");
        newmethodCall.setDescription(String.valueOf(methodCall.getCallId()));
        methodCallList.addMethodCallAutoCallId(newmethodCall);
    }

    // 选择需要处理的被调用类名
    protected abstract String chooseCalleeClassName();

    // 选择需要处理的被调用方法名
    protected abstract String chooseCalleeMethodName();

    // 选择需要替换的方法调用类型
    protected abstract String chooseMethodCallType();
}
