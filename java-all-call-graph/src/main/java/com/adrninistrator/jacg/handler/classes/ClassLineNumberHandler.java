package com.adrninistrator.jacg.handler.classes;

import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.common.enums.FieldRelationshipIdTypeEnum;
import com.adrninistrator.jacg.handler.dto.field.FieldBehavior;
import com.adrninistrator.jacg.handler.fieldrelationship.GetSetMethodHandler;
import com.adrninistrator.jacg.handler.fieldrelationship.filler.FieldBehaviorFillerInterface;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/12/8
 * @description: 根据类及代码行号进行处理的类
 */
public class ClassLineNumberHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(ClassLineNumberHandler.class);

    private final MethodCallHandler methodCallHandler;
    private final GetSetMethodHandler getSetMethodHandler;

    public ClassLineNumberHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
    }

    public ClassLineNumberHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
    }

    /**
     * 查询指定类的指定代码行号中包含的dto的get/set方法
     *
     * @param callerClassName
     * @param callerLineNumber
     * @return
     */
    public ListWithResult<BaseWriteDbData4GetSetMethod> queryGetSetMethodInClassLine(String callerClassName, int callerLineNumber) {
        // 查询指定类的指定代码行号对应的方法调用信息
        List<WriteDbData4MethodCall> methodCallList = methodCallHandler.queryMethodCallByCallerClassLineNumber(callerClassName, callerLineNumber);
        if (methodCallList == null) {
            return ListWithResult.genFail();
        }

        List<BaseWriteDbData4GetSetMethod> getSetMethodList = new ArrayList<>();
        for (WriteDbData4MethodCall methodCall : methodCallList) {
            String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCalleeFullMethod());
            // 判断被调用方法是否为get方法
            BaseWriteDbData4GetSetMethod getMethod = getSetMethodHandler.queryGetSetMethodByClassMethodSuper(true, calleeClassName, methodCall.getCalleeMethodName());
            if (getMethod != null) {
                getMethod.setMethodCallId(methodCall.getCallId());
                getSetMethodList.add(getMethod);
            }
            // 判断被调用方法是否为set方法
            BaseWriteDbData4GetSetMethod setMethod = getSetMethodHandler.queryGetSetMethodByClassMethodSuper(false, calleeClassName, methodCall.getCalleeMethodName());
            if (setMethod != null) {
                setMethod.setMethodCallId(methodCall.getCallId());
                getSetMethodList.add(setMethod);
            }
        }
        return new ListWithResult<>(getSetMethodList);
    }

    /**
     * 查询指定类的指定代码行号中包含的dto的get/set方法，并使用指定的类填充字段行为
     *
     * @param callerClassName
     * @param callerLineNumber
     * @param fieldBehaviorFillers
     * @return
     */
    public ListWithResult<FieldBehavior> queryFieldBehaviorInClassLine(String callerClassName, int callerLineNumber, FieldBehaviorFillerInterface... fieldBehaviorFillers) {
        if (ArrayUtils.isEmpty(fieldBehaviorFillers)) {
            logger.error("未指定为字段行为填充信息类的实例");
            return ListWithResult.genFail();
        }

        ListWithResult<BaseWriteDbData4GetSetMethod> getSetMethodList = queryGetSetMethodInClassLine(callerClassName, callerLineNumber);
        if (!getSetMethodList.isSuccess()) {
            return ListWithResult.genFail();
        }
        List<FieldBehavior> allFieldBehaviorList = new ArrayList<>();
        for (BaseWriteDbData4GetSetMethod getSetMethod : getSetMethodList.getList()) {
            String getMethodName = getSetMethod.isGetOrSet() ? getSetMethod.getMethodName() : "";
            String setMethodName = getSetMethod.isGetOrSet() ? "" : getSetMethod.getMethodName();
            FieldBehavior fieldBehavior = new FieldBehavior(getSetMethod.getClassName(), getSetMethod.getFieldName(), getSetMethod.getFieldType(), getMethodName, setMethodName,
                    getSetMethod.isGetOrSet(), 0);
            for (FieldBehaviorFillerInterface fieldBehaviorFiller : fieldBehaviorFillers) {
                FieldRelationshipIdTypeEnum fieldRelationshipIdTypeEnum = getSetMethod.isGetOrSet() ? FieldRelationshipIdTypeEnum.FRITE_GET_METHOD_CALL_ID :
                        FieldRelationshipIdTypeEnum.FRITE_SET_METHOD_CALL_ID;
                List<FieldBehavior> fieldBehaviorList = fieldBehaviorFiller.fillIn(fieldBehavior, fieldRelationshipIdTypeEnum, getSetMethod.getMethodCallId());
                if (!JavaCG2Util.isCollectionEmpty(fieldBehaviorList)) {
                    allFieldBehaviorList.addAll(fieldBehaviorList);
                } else {
                    allFieldBehaviorList.add(fieldBehavior);
                }
            }
        }
        return new ListWithResult<>(allFieldBehaviorList);
    }
}
