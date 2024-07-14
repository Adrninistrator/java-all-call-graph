package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldRelationship;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/7/19
 * @description: 写入数据库，通过get/set方法关联的字段关系表
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_FIELD_RELATIONSHIP,
        minColumnNum = 11,
        maxColumnNum = 11,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP
)
public class WriteDbHandler4FieldRelationship extends AbstractWriteDbHandler<WriteDbData4FieldRelationship> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4FieldRelationship.class);

    /*
        get方法对应的信息
        key
            唯一类名
        value
            get方法名称Set
    */
    private Map<String, Set<String>> getMethodSimpleClassMap;

    /*
        set方法对应的信息
        key
            唯一类名
        value
            set方法名称Set
    */
    private Map<String, Set<String>> setMethodSimpleClassMap;

    /*
        涉及继承的唯一类名
        key     子类唯一类名
        value   对应的父类唯一类名
     */
    private Map<String, String> extendsSimpleClassNameMap;

    // 枚举唯一类名集合
    private Set<String> enumSimpleClassNameSet;

    public WriteDbHandler4FieldRelationship(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4FieldRelationship genData(String[] array) {
        int fldRelationshipId = Integer.parseInt(array[0]);
        int getMethodCallId = Integer.parseInt(array[1]);
        int setMethodCallId = Integer.parseInt(array[2]);
        String callerFullMethod = array[3];
        String callerLineNumber = array[4];
        String getClassName = array[5];
        String getMethodName = array[6];
        String setClassName = array[7];
        String setMethodName = array[8];
        int valid = Integer.parseInt(array[9]);
        String type = array[10];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(callerFullMethod) || !isAllowedClassPrefix(getClassName) || !isAllowedClassPrefix(setClassName)) {
            return null;
        }

        String getSimpleClassName = dbOperWrapper.getSimpleClassName(getClassName);
        String setSimpleClassName = dbOperWrapper.getSimpleClassName(setClassName);
        if (enumSimpleClassNameSet.contains(setSimpleClassName) || enumSimpleClassNameSet.contains(getSimpleClassName)) {
            // 存在关联关系的get/set方法对应的类为枚举，不写入数据库，不打印提示日志
            return null;
        }

        // 检查存在关联关系的get方法是否是dto的get方法
        if (!checkDtoGetSetMethod(true, getSimpleClassName, getMethodName, getMethodSimpleClassMap, extendsSimpleClassNameMap)) {
            logger.warn("与set方法存在关联关系的get方法不是dto的get方法，当前关联关系设置为无效，假如确实是dto的get方法，请检查对应类及超类所在的jar包是否有指定需要解析 {} {} {} {}",
                    callerFullMethod, callerLineNumber, getClassName, getMethodName);
            valid = JavaCGYesNoEnum.NO.getIntValue();
        }

        // 检查存在关联关系的set方法是否是dto的set方法
        if (!checkDtoGetSetMethod(false, setSimpleClassName, setMethodName, setMethodSimpleClassMap, extendsSimpleClassNameMap)) {
            logger.warn("与get方法存在关联关系的set方法不是dto的set方法，当前关联关系设置为无效，假如确实是dto的set方法，请检查对应类及超类所在的jar包是否有指定需要解析 {} {} {} {}",
                    callerFullMethod, callerLineNumber, setClassName, setMethodName);
            valid = JavaCGYesNoEnum.NO.getIntValue();
        }

        WriteDbData4FieldRelationship fieldRelationship = new WriteDbData4FieldRelationship();
        fieldRelationship.setFldRelationshipId(fldRelationshipId);
        fieldRelationship.setGetMethodCallId(getMethodCallId);
        fieldRelationship.setSetMethodCallId(setMethodCallId);
        fieldRelationship.setCallerFullMethod(callerFullMethod);
        fieldRelationship.setCallerLineNumber(Integer.parseInt(callerLineNumber));
        fieldRelationship.setGetSimpleClassName(getSimpleClassName);
        fieldRelationship.setGetMethodName(getMethodName);
        fieldRelationship.setGetClassName(getClassName);
        fieldRelationship.setSetSimpleClassName(setSimpleClassName);
        fieldRelationship.setSetMethodName(setMethodName);
        fieldRelationship.setSetClassName(setClassName);
        fieldRelationship.setValid(valid);
        fieldRelationship.setType(type);
        fieldRelationship.setRelationshipFlags(0);
        fieldRelationship.setBeanUtilCallId(JavaCGConstants.RECORD_ID_MIN_BEFORE);
        fieldRelationship.setBeanUtilMethod(null);
        return fieldRelationship;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4FieldRelationship data) {
        return JACGSqlUtil.genFieldRelationshipObjectArray(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "关联关系id，从1开始",
                "get方法调用序号，从1开始",
                "set方法调用序号，从1开始",
                "调用方，完整方法",
                "调用方，源代码行号",
                "get方法完整类名",
                "get方法方法名",
                "set方法完整类名",
                "set方法方法名",
                "关联关系是否有效，1:是，0:否",
                "关联关系类型，参考 java-callgraph2 项目 JavaCGFieldRelationshipTypeEnum 类"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "通过get/set方法关联的字段关系，包含了set方法及用于赋值的get方法，以及set方法所在的方法"
        };
    }

    public void setGetMethodSimpleClassMap(Map<String, Set<String>> getMethodSimpleClassMap) {
        this.getMethodSimpleClassMap = getMethodSimpleClassMap;
    }

    public void setSetMethodSimpleClassMap(Map<String, Set<String>> setMethodSimpleClassMap) {
        this.setMethodSimpleClassMap = setMethodSimpleClassMap;
    }

    public void setExtendsSimpleClassNameMap(Map<String, String> extendsSimpleClassNameMap) {
        this.extendsSimpleClassNameMap = extendsSimpleClassNameMap;
    }

    public void setEnumSimpleClassNameSet(Set<String> enumSimpleClassNameSet) {
        this.enumSimpleClassNameSet = enumSimpleClassNameSet;
    }
}