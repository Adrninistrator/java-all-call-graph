package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/11/14
 * @description: 写入数据库，get/set方法的抽象父类
 */
public abstract class AbstractWriteDbHandler4GetSetMethod<T extends BaseWriteDbData> extends AbstractWriteDbHandler<T> {

    public AbstractWriteDbHandler4GetSetMethod(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    /**
     * 读取文件内容并填充对象
     *
     * @param baseWriteDbData4GetSetMethod
     * @param getSetMethodSimpleClassMap
     */
    protected void fillInBaseWriteDbData4GetSetMethod(BaseWriteDbData4GetSetMethod baseWriteDbData4GetSetMethod, Map<String, Set<String>> getSetMethodSimpleClassMap) {
        String className = readLineData();
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String methodName = readLineData();
        String fieldName = readLineData();
        String fieldCategory = readLineData();
        String fieldTypeNad = readLineData();
        int arrayDimensions = Integer.parseInt(readLineData());
        String fullMethod = readLineData();
        String returnType = readLineData();

        // 记录get方法对应的信息
        Set<String> getMethodSet = getSetMethodSimpleClassMap.computeIfAbsent(simpleClassName, k -> new HashSet<>());
        getMethodSet.add(methodName);

        baseWriteDbData4GetSetMethod.setRecordId(genNextRecordId());
        baseWriteDbData4GetSetMethod.setSimpleClassName(simpleClassName);
        baseWriteDbData4GetSetMethod.setMethodName(methodName);
        baseWriteDbData4GetSetMethod.setFieldName(fieldName);
        baseWriteDbData4GetSetMethod.setFieldCategory(fieldCategory);
        baseWriteDbData4GetSetMethod.setSimpleFieldTypeNad(dbOperWrapper.querySimpleClassName(fieldTypeNad));
        baseWriteDbData4GetSetMethod.setFieldTypeNad(fieldTypeNad);
        baseWriteDbData4GetSetMethod.setArrayDimensions(arrayDimensions);
        baseWriteDbData4GetSetMethod.setClassName(className);
        baseWriteDbData4GetSetMethod.setMethodHash(JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType));
        baseWriteDbData4GetSetMethod.setFullMethod(fullMethod);
        baseWriteDbData4GetSetMethod.setReturnType(returnType);
    }

    protected Object[] genObjectArrayBase(BaseWriteDbData4GetSetMethod data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getMethodName(),
                data.getFieldName(),
                data.getFieldCategory(),
                data.getSimpleFieldTypeNad(),
                data.getFieldTypeNad(),
                data.getArrayDimensions(),
                data.getClassName(),
                data.getMethodHash(),
                data.getFullMethod(),
                data.getReturnType()
        };
    }

    protected String[] chooseFileColumnDescBase() {
        return new String[]{
                "完整类名",
                "方法名",
                "字段名",
                "字段分类，J:JDK中的类型，C:自定义类型，GJ:泛型类型，只涉及JDK中的类型，GC:泛型类型，涉及自定义类型",
                "字段类型（不包含数组标志）",
                "字段数组类型的维度，为0代表不是数组类型",
                "完整方法（类名+方法名+参数）",
                "方法返回类型，包含数组标志"
        };
    }

    protected void copyWriteDbData(BaseWriteDbData4GetSetMethod src, BaseWriteDbData4GetSetMethod dst) {
        dst.setRecordId(src.getRecordId());
        dst.setSimpleClassName(src.getSimpleClassName());
        dst.setMethodName(src.getMethodName());
        dst.setFieldName(src.getFieldName());
        dst.setFieldCategory(src.getFieldCategory());
        dst.setSimpleFieldTypeNad(src.getSimpleFieldTypeNad());
        dst.setFieldTypeNad(src.getFieldTypeNad());
        dst.setArrayDimensions(src.getArrayDimensions());
        dst.setClassName(src.getClassName());
        dst.setMethodHash(src.getMethodHash());
        dst.setFullMethod(src.getFullMethod());
        dst.setReturnType(src.getReturnType());
    }
}
