package com.adrninistrator.jacg.handler.classes;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassName;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.common.enums.ClassInterfaceEnum;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.dto.accessflag.JavaCG2AccessFlags;
import org.apache.commons.lang3.StringUtils;
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
     * 检查指定的类名是否存在
     *
     * @param className 类名
     * @return
     */
    public boolean checkClassNameExists(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CI_CHECK_CLASS_NAME_EXISTS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CI_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_CLASS_INFO.getTableName() +
                    " where " + DC.CI_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String existedClassName = dbOperator.queryObjectOneColumn(sql, String.class, className);
        return StringUtils.isNotBlank(existedClassName);
    }

    /**
     * 查询以指定关键字结尾的类名信息
     *
     * @param classNameSuffix 类名后缀
     * @return
     */
    public List<WriteDbData4ClassName> queryClassNameEndsWith(String classNameSuffix) {
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
    public String queryExactlySimpleClassName(String className) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        WriteDbData4ClassName writeDbData4ClassName = queryClassNameDataBySimple(simpleClassName);
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
    public WriteDbData4ClassName queryClassNameDataBySimple(String simpleClassName) {
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
     * 根据简单类名查询对应的完整类名
     *
     * @param simpleClassName
     * @return
     */
    public String queryClassNameBySimple(String simpleClassName) {
        WriteDbData4ClassName writeDbData4ClassName = queryClassNameDataBySimple(simpleClassName);
        if (writeDbData4ClassName == null) {
            return null;
        }
        return writeDbData4ClassName.getClassName();
    }

    /**
     * 根据完整类名查询类的JavaCG2AccessFlags对象，可调用方法判断类的属性
     *
     * @param className 完整类名
     * @return 可能为null
     */
    public JavaCG2AccessFlags queryClassJavaCG2AccessFlags(String className) {
        Integer accessFlags = queryClassAccessFlag(className);
        if (accessFlags == null) {
            return null;
        }
        return new JavaCG2AccessFlags(accessFlags);
    }

    /**
     * 根据完整类名查询类的access_flags
     *
     * @param className 完整类名
     * @return 可能为null
     */
    public Integer queryClassAccessFlag(String className) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        return queryClassAccessFlagBySCN(simpleClassName);
    }

    /**
     * 根据唯一类名查询类的access_flags
     *
     * @param simpleClassName 唯一类名
     * @return 可能为null
     */
    public Integer queryClassAccessFlagBySCN(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CI_QUERY_ACCESS_FLAGS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CI_ACCESS_FLAGS +
                    " from " + DbTableInfoEnum.DTIE_CLASS_INFO.getTableName() +
                    " where " + DC.CI_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, Integer.class, simpleClassName);
    }

    /**
     * 根据类名查询类的信息
     *
     * @param className
     * @return
     */
    public WriteDbData4ClassInfo queryClassInfoByClassName(String className) {
        return queryClassInfoBySCN(dbOperWrapper.querySimpleClassName(className));
    }

    /**
     * 根据唯一类名查询类的信息
     *
     * @param simpleClassName 唯一类名
     * @return 可能为null
     */
    public WriteDbData4ClassInfo queryClassInfoBySCN(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CI_QUERY_BY_SIMPLE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_CLASS_INFO) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_INFO.getTableName() +
                    " where " + DC.CI_SIMPLE_CLASS_NAME + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4ClassInfo.class, simpleClassName);
    }

    /**
     * 根据唯一类名查询重复类的信息
     *
     * @param simpleClassName 唯一类名
     * @return 可能为null
     */
    public List<WriteDbData4ClassInfo> queryDupClassInfoBySCN(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.DCI_QUERY_BY_SIMPLE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_DUP_CLASS_INFO) +
                    " from " + DbTableInfoEnum.DTIE_DUP_CLASS_INFO.getTableName() +
                    " where " + DC.CI_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4ClassInfo.class, simpleClassName);
    }

    /**
     * 根据唯一类名查询类的类型
     *
     * @param simpleClassName 唯一类名
     * @return
     */
    public ClassInterfaceEnum queryClassInterfaceEnumBySimple(String simpleClassName) {
        Integer superClassAccessFlags = queryClassAccessFlagBySCN(simpleClassName);
        return JACGClassMethodUtil.getClassInterfaceEnum(superClassAccessFlags);
    }

    /**
     * 根据完整类名查询类的类型
     *
     * @param className 唯一类名
     * @return
     */
    public ClassInterfaceEnum queryClassInterfaceEnum(String className) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        return queryClassInterfaceEnumBySimple(simpleClassName);
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
