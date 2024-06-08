package com.adrninistrator.jacg.handler.classes;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassName;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.dto.accessflag.JavaCGAccessFlags;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/13
 * @description: 查询类的信息处理类
 */
public class ClassInfoHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(ClassInfoHandler.class);

    public ClassInfoHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public ClassInfoHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 查询以指定关键字结尾的类名信息
     *
     * @param classNameSuffix 类名后缀
     * @return
     */
    public List<WriteDbData4ClassName> getClassNameEndsWith(String classNameSuffix) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_QUERY_CLASS_LIKE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_CLASS_NAME) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " where " + DC.CN_SIMPLE_CLASS_NAME + " like concat('%', ?)";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4ClassName.class, classNameSuffix);
    }

    /**
     * 根据完整类名获取对应的类名
     * 假如对应的类不存在，则返回null
     *
     * @param className 完整类名信息
     * @return 完整类名或简单类名，或null
     */
    public String getExactlySimpleClassName(String className) {
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        WriteDbData4ClassName writeDbData4ClassName = getClassNameEquals(simpleClassName);
        if (writeDbData4ClassName == null || !className.equals(writeDbData4ClassName.getClassName())) {
            logger.warn("指定的类名不存在 {}", className);
            return null;
        }
        return simpleClassName;
    }

    /**
     * 查询指定类名的类名信息
     *
     * @param simpleClassName 完整类名或简单类名
     * @return
     */
    public WriteDbData4ClassName getClassNameEquals(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_QUERY_CLASS_EQUALS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_CLASS_NAME) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " where " + DC.CN_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4ClassName.class, simpleClassName);
    }

    /**
     * 根据完整类名查询类的JavaCGAccessFlags对象，可调用方法判断类的属性
     *
     * @param className 完整类名
     * @return 可能为null
     */
    public JavaCGAccessFlags queryClassJavaCGAccessFlags(String className) {
        Integer accessFlags = queryClassAccessFlag(className);
        if (accessFlags == null) {
            return null;
        }
        return new JavaCGAccessFlags(accessFlags);
    }

    /**
     * 根据完整类名查询类的access_flags
     *
     * @param className 完整类名
     * @return 可能为null
     */
    public Integer queryClassAccessFlag(String className) {
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CI_QUERY_ACCESS_FLAGS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CI_ACCESS_FLAGS +
                    " from " + DbTableInfoEnum.DTIE_CLASS_INFO.getTableName() +
                    " where " + DC.CSEI1_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, Integer.class, simpleClassName);
    }

    /**
     * 分页查询类信息
     *
     * @param lastQuery     是否最后一次查询
     * @param startRecordId 起始记录ID
     * @param endRecordId   结束记录ID
     * @return
     */
    public List<WriteDbData4ClassInfo> queryClassesInfoByPage(boolean lastQuery, int startRecordId, int endRecordId) {
        SqlKeyEnum sqlKeyEnum = lastQuery ? SqlKeyEnum.CI_QUERY_ALL_BY_ID : SqlKeyEnum.CI_QUERY_ALL_BY_ID_LAST;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_CLASS_INFO) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_INFO.getTableName() +
                    " where " + DC.CI_RECORD_ID + " > ?";
            if (!lastQuery) {
                sql += " and " + DC.CI_RECORD_ID + " <= ?";
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<Object> argList = new ArrayList<>();
        argList.add(startRecordId);
        if (!lastQuery) {
            argList.add(endRecordId);
        }
        return dbOperator.queryList(sql, WriteDbData4ClassInfo.class, argList.toArray());
    }

    /**
     * 根据包名前缀查询对应的类名
     *
     * @param packagePrefix
     * @return
     */
    public List<String> queryClassNameByPackagePrefix(String packagePrefix) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CI_QUERY_BY_PACKAGE_PREFIX;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CI_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_CLASS_INFO.getTableName() +
                    " where " + DC.CI_CLASS_NAME + " like concat(?, '%')";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, packagePrefix);
    }
}
