package com.adrninistrator.jacg.handler.fieldrelationship;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldRelationship;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.common.enums.FieldRelationshipFlagsEnum;
import com.adrninistrator.jacg.handler.common.enums.FieldRelationshipIdTypeEnum;
import com.adrninistrator.jacg.handler.dto.field.FieldBehavior;
import com.adrninistrator.jacg.handler.dto.field.JACGFieldMethodInfo;
import com.adrninistrator.jacg.handler.fieldrelationship.filler.FieldBehaviorFillerInterface;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2FieldRelationshipTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/7/20
 * @description: 字段关联关系处理类（get/set方法关联关系）
 */
public class FieldRelationshipHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(FieldRelationshipHandler.class);

    private final GetSetMethodHandler getSetMethodHandler;

    private final AnnotationHandler annotationHandler;

    public FieldRelationshipHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
    }

    public FieldRelationshipHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
    }

    /**
     * 使用指定的类对字段行为填充
     *
     * @param fieldBehavior        字段行为
     * @param fldRelationshipId    通过get/set方法的字段关联关系id
     * @param fieldBehaviorFillers 为字段行为填充信息的对象数组
     * @return null: 没有填充字段行为信息 非null: 填充后的字段行为信息子类对象
     */
    public List<FieldBehavior> fillInInfo4RelatedField(FieldBehavior fieldBehavior, int fldRelationshipId, FieldBehaviorFillerInterface... fieldBehaviorFillers) {
        // 使用为字段行为填充信息的对象进行处理
        for (FieldBehaviorFillerInterface fieldBehaviorFiller : fieldBehaviorFillers) {
            List<FieldBehavior> newFieldBehaviorList = fieldBehaviorFiller.fillIn(fieldBehavior, FieldRelationshipIdTypeEnum.FRITE_FIELD_RELATIONSHIP_ID,
                    fldRelationshipId);
            if (!JavaCG2Util.isCollectionEmpty(newFieldBehaviorList)) {
                return newFieldBehaviorList;
            }
        }
        return null;
    }

    /**
     * 生成已填充或不需要填充的字段行为
     *
     * @param fieldRelationship    通过get/set方法的字段关联关系
     * @param fieldName            字段名
     * @param getOrSet             true: 对应get方法 false: 对应set方法
     * @param fieldBehaviorFillers 为字段行为填充信息的对象数组
     * @return
     */
    public List<FieldBehavior> genFilledFieldBehavior(WriteDbData4FieldRelationship fieldRelationship, String fieldName, boolean getOrSet,
                                                      FieldBehaviorFillerInterface... fieldBehaviorFillers) {
        String className = getOrSet ? fieldRelationship.getGetClassName() : fieldRelationship.getSetClassName();
        BaseWriteDbData4GetSetMethod getMethod = getSetMethodHandler.queryGetSetMethodByFieldNameSuper(true, className, fieldName);
        BaseWriteDbData4GetSetMethod setMethod = getSetMethodHandler.queryGetSetMethodByFieldNameSuper(false, className, fieldName);
        String getMethodName = null;
        String fieldType = null;
        if (getMethod != null) {
            getMethodName = getMethod.getMethodName();
            fieldType = getMethod.getFieldType();
        }
        String setMethodName = null;
        if (setMethod != null) {
            setMethodName = setMethod.getMethodName();
            if (fieldType == null) {
                fieldType = setMethod.getFieldType();
            }
        }
        FieldBehavior fieldBehavior = new FieldBehavior(className, fieldName, fieldType, getMethodName, setMethodName, getOrSet, fieldRelationship.getRelationshipFlags());
        List<FieldBehavior> newFieldBehaviorList = fillInInfo4RelatedField(fieldBehavior, fieldRelationship.getFldRelationshipId(), fieldBehaviorFillers);
        return !JavaCG2Util.isCollectionEmpty(newFieldBehaviorList) ? newFieldBehaviorList : Collections.singletonList(fieldBehavior);
    }

    /**
     * 根据get/set方法对应的类名、方法名，查询通过这些方法进行赋值的调用方方法
     *
     * @param getClassName  get方法类名
     * @param getMethodName get方法名
     * @param setClassName  set方法类名
     * @param setMethodName set方法名
     * @return
     */
    public List<WriteDbData4FieldRelationship> queryCallerMethodByRelationship(String getClassName, String getMethodName, String setClassName, String setMethodName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FR_QUERY_CALLER_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + JACGSqlUtil.joinColumns(DC.FR_CALLER_FULL_METHOD, DC.FR_CALLER_LINE_NUMBER) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP.getTableName() +
                    " where " + DC.FR_GET_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FR_GET_METHOD_NAME + " = ?" +
                    " and " + DC.FR_SET_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FR_SET_METHOD_NAME + " = ?" +
                    " and " + DC.FR_VALID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String simpleGetClassName = dbOperWrapper.querySimpleClassName(getClassName);
        String simpleSetClassName = dbOperWrapper.querySimpleClassName(setClassName);
        return dbOperator.queryList(sql, WriteDbData4FieldRelationship.class, simpleGetClassName, getMethodName, simpleSetClassName, setMethodName,
                JavaCG2YesNoEnum.YES.getIntValue());
    }

    /**
     * 查询指定类指定get方法存在关联关系的set方法列表（去重）
     *
     * @param className  get方法完整类名
     * @param methodName get方法名
     * @return
     */
    public List<WriteDbData4FieldRelationship> queryRelatedSetMethod4Get(String className, String methodName) {
        // 获取当前get方法对应的set方法，用于忽略当前类的get方法与set方法的关联关系
        String setMethodName = JACGClassMethodUtil.genSetMethodName4GetMethod(methodName);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FR_QUERY_SET_METHOD_4GET;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + JACGSqlUtil.joinColumns(DC.FR_SET_CLASS_NAME, DC.FR_SET_METHOD_NAME) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP.getTableName() +
                    " where " + DC.FR_GET_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FR_GET_METHOD_NAME + " = ?" +
                    " and not (" +
                    DC.FR_SET_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FR_SET_METHOD_NAME + " = ?" +
                    ")" +
                    " and " + DC.FR_VALID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        return dbOperator.queryList(sql, WriteDbData4FieldRelationship.class, simpleClassName, methodName, simpleClassName, setMethodName, JavaCG2YesNoEnum.YES.getIntValue());
    }

    /**
     * 查询指定类指定set方法存在关联关系的get方法列表（去重）
     *
     * @param className  set方法完整类名
     * @param methodName set方法名
     * @return
     */
    public List<WriteDbData4FieldRelationship> queryRelatedGetMethod4Set(String className, String methodName) {
        // 获取当前set方法对应的get方法，用于忽略当前类的get方法与set方法的关联关系
        String getMethodName = JACGClassMethodUtil.genSetMethodName4GetMethod(methodName);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FR_QUERY_GET_METHOD_4SET;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + JACGSqlUtil.joinColumns(DC.FR_GET_CLASS_NAME, DC.FR_GET_SIMPLE_CLASS_NAME, DC.FR_GET_METHOD_NAME) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP.getTableName() +
                    " where " + DC.FR_SET_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FR_SET_METHOD_NAME + " = ?" +
                    " and not (" +
                    DC.FR_GET_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FR_GET_METHOD_NAME + " = ?" +
                    ")" +
                    " and " + DC.FR_VALID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        return dbOperator.queryList(sql, WriteDbData4FieldRelationship.class, simpleClassName, methodName, simpleClassName, getMethodName, JavaCG2YesNoEnum.YES.getIntValue());
    }

    /**
     * 通过get方法调用序号查找匹配的字段关联关系，可能查询到多条
     *
     * @param setMethodCallId set方法调用序号
     * @return
     */
    public List<WriteDbData4FieldRelationship> queryRelationshipByGetMethodCallId(int setMethodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FR_QUERY_BY_GET_METHOD_CALL_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP.getTableName() +
                    " where " + DC.FR_GET_METHOD_CALL_ID + " = ?" +
                    " and " + DC.FR_VALID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4FieldRelationship.class, setMethodCallId, JavaCG2YesNoEnum.YES.getIntValue());
    }

    /**
     * 通过set方法调用序号查找匹配的直接赋值字段关联关系，可能查询到多条
     *
     * @param setMethodCallId set方法调用序号
     * @return
     */
    public List<WriteDbData4FieldRelationship> queryDirectlyRelationshipBySetMethodCallId(int setMethodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FR_QUERY_BY_SET_METHOD_CALL_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP.getTableName() +
                    " where " + DC.FR_SET_METHOD_CALL_ID + " = ?" +
                    " and " + DC.FR_TYPE + " in (?, ?)" +
                    " and " + DC.FR_VALID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4FieldRelationship.class, setMethodCallId, JavaCG2FieldRelationshipTypeEnum.FRTE_DIRECTLY.getType(),
                JavaCG2FieldRelationshipTypeEnum.FRTE_DIRECTLY_EQUIVALENT_CONVERSION.getType(), JavaCG2YesNoEnum.YES.getIntValue());
    }

    /**
     * 修改字段关联关系表的标志
     *
     * @param fldRelationshipId
     * @param relationshipFlagsBefore
     * @param addedFieldRelationFlagsEnum
     * @return
     */
    public boolean updateFieldRelationshipAddFlag(int fldRelationshipId, int relationshipFlagsBefore, FieldRelationshipFlagsEnum addedFieldRelationFlagsEnum) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FR_UPDATE_FLAGS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "update " + DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP.getTableName() +
                    " set " + DC.FR_RELATIONSHIP_FLAGS + " = ?" +
                    " where " + DC.FR_FLD_RELATIONSHIP_ID + " = ?" +
                    " and " + DC.FR_RELATIONSHIP_FLAGS + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        int relationshipFlagsAfter = addedFieldRelationFlagsEnum.setFlag(relationshipFlagsBefore);
        Integer row = dbOperator.update(sql, relationshipFlagsAfter, fldRelationshipId, relationshipFlagsBefore);
        if (row != null) {
            logger.debug("修改字段关联关系表中指定记录的标志 {} 返回行数: {}", fldRelationshipId, row);
            return true;
        }
        return false;
    }


    /**
     * 查询指定类及超类中的全部字段信息
     *
     * @param className       类名
     * @param queryGetMethod  true: 查询get方法对应的字段 false: 查询set方法对应的字段
     * @param useJsonProperty true: 使用@JsonProperty注解属性值作为字段名称 false: 不处理@JsonProperty注解
     * @return
     */
    public Map<String, JACGFieldMethodInfo> queryAllFieldMethodInfo(String className, boolean queryGetMethod, boolean useJsonProperty) {
        // 根据类名，查询对应的get/set方法，若指定的类中不存在则从超类中查询
        List<BaseWriteDbData4GetSetMethod> getSetMethodList = getSetMethodHandler.queryGetSetMethodByClassNameSuper(queryGetMethod, className);
        if (JavaCG2Util.isCollectionEmpty(getSetMethodList)) {
            return null;
        }
        Map<String, JACGFieldMethodInfo> fieldMethodInfoMap = new HashMap<>();
        for (BaseWriteDbData4GetSetMethod getSetMethod : getSetMethodList) {
            String fieldName = getSetMethod.getFieldName();
            String jsonPropertyValue = null;
            if (useJsonProperty) {
                // 查询@JsonProperty注解属性
                jsonPropertyValue = annotationHandler.queryFieldJsonPropertyValue(getSetMethod.getClassName(), fieldName);
            }
            // 若存在@JsonProperty注解属性，则使用其作为字段名
            String usedFieldName = jsonPropertyValue != null ? jsonPropertyValue : fieldName;
            boolean inCurrentClass = getSetMethod.getClassName().equals(className);
            JACGFieldMethodInfo fieldMethodInfo = new JACGFieldMethodInfo(usedFieldName, getSetMethod.getMethodName(), inCurrentClass, jsonPropertyValue != null);
            if (fieldMethodInfoMap.putIfAbsent(usedFieldName, fieldMethodInfo) != null) {
                logger.warn("子类与父类中存在同名字段 {} {} {}", className, getSetMethod.getClassName(), usedFieldName);
            }
        }
        return fieldMethodInfoMap;
    }
}
