package com.adrninistrator.jacg.handler.protocol;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.methodreturn.MethodReturnTypeWithGenerics;
import com.adrninistrator.jacg.handler.field.FieldInfoHandler;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/12/3
 * @description: 查询返回协议中的字段信息的处理类
 */
public class QueryRspProtocolFieldHandler extends BaseHandler {

    protected final MethodInfoHandler methodInfoHandler;
    protected final MethodArgReturnHandler methodArgReturnHandler;
    protected final FieldInfoHandler fieldInfoHandler;

    public QueryRspProtocolFieldHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        fieldInfoHandler = new FieldInfoHandler(dbOperWrapper);
    }

    public QueryRspProtocolFieldHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        fieldInfoHandler = new FieldInfoHandler(dbOperWrapper);
    }

    /**
     * 查询指定方法的返回类型，包含泛型类型
     *
     * @param fullMethod
     * @return
     */
    public MethodReturnTypeWithGenerics queryMethodReturnTypeWithGenerics(String fullMethod) {
        MethodReturnTypeWithGenerics methodReturnTypeWithGenerics = new MethodReturnTypeWithGenerics();
        // 查询方法信息
        WriteDbData4MethodInfo methodInfo = methodInfoHandler.queryMethodInfoByFullMethod(fullMethod);
        methodReturnTypeWithGenerics.setMethodReturnType(methodInfo.getReturnType());

        List<String> objectFieldNameList = new ArrayList<>();
        methodReturnTypeWithGenerics.setObjectFieldNameList(objectFieldNameList);
        // 查询类的字段信息，包含超类中的字段，根据类名查询
        List<WriteDbData4FieldInfo> fieldInfoList = fieldInfoHandler.queryFieldInfoByClassNameIncludeSuper(methodInfo.getReturnType());
        for (WriteDbData4FieldInfo fieldInfo : fieldInfoList) {
            if (JavaCG2CommonNameConstants.CLASS_NAME_OBJECT.equals(fieldInfo.getFieldType())) {
                objectFieldNameList.add(fieldInfo.getFieldName());
            }
        }

        if (JavaCG2YesNoEnum.isYes(methodInfo.getReturnExistsGenericsType())) {
            // 查询返回类型中泛型类型
            List<String> genericsTypeInMethodReturn = methodArgReturnHandler.queryGenericsTypeInMethodReturn(fullMethod, false);
            methodReturnTypeWithGenerics.setMethodReturnGenericsTypeList(genericsTypeInMethodReturn);
        }
        return methodReturnTypeWithGenerics;
    }
}
