package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SetMethod;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/7/19
 * @description: 写入数据库，set方法
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_SET_METHOD,
        minColumnNum = 6,
        maxColumnNum = 6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SET_METHOD
)
public class WriteDbHandler4SetMethod extends AbstractWriteDbHandler<WriteDbData4SetMethod> {
    /*
        set方法对应的信息
        key
            唯一类名
        value
            set方法名称Set
    */
    private Map<String, Set<String>> setMethodSimpleClassMap;

    public WriteDbHandler4SetMethod(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4SetMethod genData(String[] array) {
        String className = array[0];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }

        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String methodName = array[1];
        String fieldName = array[2];
        String fieldCategory = array[3];
        String fieldType = array[4];
        String fullMethod = array[5];

        // 记录set方法对应的信息
        Set<String> setMethodSet = setMethodSimpleClassMap.computeIfAbsent(simpleClassName, k -> new HashSet<>());
        setMethodSet.add(methodName);

        WriteDbData4SetMethod writeDbData4SetMethod = new WriteDbData4SetMethod();
        writeDbData4SetMethod.setRecordId(genNextRecordId());
        writeDbData4SetMethod.setSimpleClassName(simpleClassName);
        writeDbData4SetMethod.setMethodName(methodName);
        writeDbData4SetMethod.setFieldName(fieldName);
        writeDbData4SetMethod.setFieldCategory(fieldCategory);
        writeDbData4SetMethod.setSimpleFieldType(dbOperWrapper.querySimpleClassName(fieldType));
        writeDbData4SetMethod.setFieldType(fieldType);
        writeDbData4SetMethod.setClassName(className);
        writeDbData4SetMethod.setMethodHash(JACGUtil.genHashWithLen(fullMethod));
        writeDbData4SetMethod.setFullMethod(fullMethod);
        return writeDbData4SetMethod;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SetMethod data) {
        return new Object[]{
                data.getRecordId(),
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
                "dto的set方法的信息，包含对应的字段信息"
        };
    }

    public void setSetMethodSimpleClassMap(Map<String, Set<String>> setMethodSimpleClassMap) {
        this.setMethodSimpleClassMap = setMethodSimpleClassMap;
    }
}

