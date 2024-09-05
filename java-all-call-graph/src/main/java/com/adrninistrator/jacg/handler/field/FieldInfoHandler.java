package com.adrninistrator.jacg.handler.field;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import org.apache.commons.lang3.ArrayUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/1/11
 * @description: 字段信息处理类
 */
public class FieldInfoHandler extends BaseHandler {
    public FieldInfoHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public FieldInfoHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 查询指定类中，字段类型属于指定的包的字段（支持排除特定的类型），且非public、非static、非final的字段信息
     *
     * @param className
     * @return
     */
    public List<WriteDbData4FieldInfo> queryClassFieldsByPackageExcludePSF(String className, String typePackage, String... excludedTypes) {
        int excludedTypeNum;
        if (ArrayUtils.isEmpty(excludedTypes)) {
            excludedTypeNum = 0;
        } else {
            excludedTypeNum = excludedTypes.length;
        }
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FI_QUERY_BY_CLASS_PACKAGE_EXCLUDE_PSF;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum, excludedTypeNum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_INFO.getTableName() +
                    " where " + DC.FI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FI_FIELD_TYPE + " like concat(?, '%')" +
                    " and " + DC.FI_MODIFIERS + " != ?" +
                    " and " + DC.FI_STATIC_FLAG + " = ?" +
                    " and " + DC.FI_FINAL_FLAG + " = ?";
            if (excludedTypeNum > 0) {
                sql = sql + " and " + DC.FI_FIELD_TYPE + " not in " + JACGSqlUtil.genQuestionString(excludedTypeNum);
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql, excludedTypeNum);
        }
        List<Object> argList = new ArrayList<>(excludedTypeNum + 2);
        argList.add(dbOperWrapper.querySimpleClassName(className));
        argList.add(typePackage);
        argList.add(JavaCG2CommonNameConstants.MODIFIERS_PUBLIC);
        argList.add(JavaCG2YesNoEnum.NO.getIntValue());
        argList.add(JavaCG2YesNoEnum.NO.getIntValue());
        if (excludedTypeNum > 0) {
            argList.addAll(Arrays.asList(excludedTypes));
        }
        return dbOperator.queryList(sql, WriteDbData4FieldInfo.class, argList.toArray());
    }

    /**
     * 查询指定类中，类型属于指定的包的字段，排除特定的类型
     *
     * @param className
     * @return
     */
    public List<WriteDbData4FieldInfo> queryClassCustomTypeFields(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FI_QUERY_BY_CLASS_CUSTOM_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_INFO.getTableName() +
                    " where " + DC.FI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FI_FIELD_TYPE + " not like concat(?, '%')" +
                    " and " + DC.FI_PRIMITIVE_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4FieldInfo.class, dbOperWrapper.querySimpleClassName(className), JavaCG2CommonNameConstants.PACKAGE_JAVA,
                JavaCG2YesNoEnum.NO.getIntValue());
    }

    /**
     * 查询指定类指定字段的集合中涉及的泛型类型
     *
     * @param className
     * @param fieldName
     * @return
     */
    public List<WriteDbData4FieldGenericsType> queryFieldGenericsTypeByClassFieldName(String className, String fieldName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FGT_QUERY_BY_CLASS_FIELD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_GENERICS_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_GENERICS_TYPE.getTableName() +
                    " where " + DC.FGT_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FGT_FIELD_NAME + " = ?" +
                    " order by " + DC.FGT_SEQ;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4FieldGenericsType.class, dbOperWrapper.querySimpleClassName(className), fieldName);
    }

    /**
     * 查询在集合的泛型类型中使用指定类型字段的类
     *
     * @param fieldType
     * @return
     */
    public List<String> queryFieldGenericsTypeByType(String fieldType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FGT_QUERY_BY_FIELD_GENERICS_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + DC.FGT_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_FIELD_GENERICS_TYPE.getTableName() +
                    " where " + DC.FGT_SIMPLE_FIELD_GENERICS_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, dbOperWrapper.querySimpleClassName(fieldType));
    }
}
