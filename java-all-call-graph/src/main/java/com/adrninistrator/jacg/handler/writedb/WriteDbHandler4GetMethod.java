package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4GetMethod;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/7/19
 * @description: 写入数据库，get方法
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_GET_METHOD,
        minColumnNum = 6,
        maxColumnNum = 6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_GET_METHOD
)
public class WriteDbHandler4GetMethod extends AbstractWriteDbHandler<WriteDbData4GetMethod> {
    /*
        get方法对应的信息
        key
            唯一类名
        value
            get方法名称Set
    */
    private Map<String, Set<String>> getMethodSimpleClassMap;

    public WriteDbHandler4GetMethod(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4GetMethod genData(String[] array) {
        String className = array[0];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }

        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        String methodName = array[1];
        String fieldName = array[2];
        String fieldCategory = array[3];
        String fieldType = array[4];
        String fullMethod = array[5];

        // 记录get方法对应的信息
        Set<String> getMethodSet = getMethodSimpleClassMap.computeIfAbsent(simpleClassName, k -> new HashSet<>());
        getMethodSet.add(methodName);

        WriteDbData4GetMethod writeDbData4GetMethod = new WriteDbData4GetMethod();
        writeDbData4GetMethod.setSimpleClassName(simpleClassName);
        writeDbData4GetMethod.setMethodName(methodName);
        writeDbData4GetMethod.setFieldName(fieldName);
        writeDbData4GetMethod.setFieldCategory(fieldCategory);
        writeDbData4GetMethod.setSimpleFieldType(dbOperWrapper.getSimpleClassName(fieldType));
        writeDbData4GetMethod.setFieldType(fieldType);
        writeDbData4GetMethod.setClassName(className);
        writeDbData4GetMethod.setMethodHash(JACGUtil.genHashWithLen(fullMethod));
        writeDbData4GetMethod.setFullMethod(fullMethod);
        return writeDbData4GetMethod;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4GetMethod data) {
        return new Object[]{
                genNextRecordId(),
                data.getSimpleClassName(),
                data.getMethodName(),
                data.getFieldName(),
                data.getFieldCategory(),
                data.getSimpleFieldType(),
                data.getFieldType(),
                data.getClassName(),
                data.getMethodHash(),
                data.getFullMethod()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "方法名",
                "字段名",
                "字段分类，J:JDK中的类型，C:自定义类型，GJ:集合的泛型类型，只涉及JDK中的类型，GC:集合的泛型类型，涉及自定义类型",
                "字段类型",
                "完整方法（类名+方法名+参数）"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "dto的get方法的信息，包含对应的字段信息"
        };
    }

    public void setGetMethodSimpleClassMap(Map<String, Set<String>> getMethodSimpleClassMap) {
        this.getMethodSimpleClassMap = getMethodSimpleClassMap;
    }
}

