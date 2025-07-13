package com.adrninistrator.jacg.handler.mybatis;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSFormatedSql;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;

/**
 * @author adrninistrator
 * @date 2025/6/8
 * @description:
 */
public class MybatisMsFormatedSqlHandler extends BaseHandler {
    public MybatisMsFormatedSqlHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public MybatisMsFormatedSqlHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 检查某个类是否为MyBatis Mapper
     *
     * @param className
     * @return
     */
    public boolean checkClassMyBatisMapper(String className) {
        return checkClassMyBatisMapper(className, "");
    }

    /**
     * 检查某个类是否为MyBatis Mapper
     *
     * @param className
     * @return
     */
    public boolean checkClassMyBatisMapper(String className, String tableSuffix) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMFS_QUERY_MAPPER_SCN;
        String sql = dbOperWrapper.getCachedSqlWithSuffix(sqlKeyEnum, tableSuffix);
        if (sql == null) {
            sql = "select " + DC.MMFS_MAPPER_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_FORMATED_SQL.getTableName(appName, tableSuffix) +
                    " where " + DC.MMFS_MAPPER_SIMPLE_CLASS_NAME + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSqlWithSuffix(sqlKeyEnum, sql, tableSuffix);
        }

        String mapperSimpleClassName = dbOperator.queryObjectOneColumn(sql, String.class, dbOperWrapper.querySimpleClassName(className, tableSuffix));
        return mapperSimpleClassName != null;
    }

    /**
     * 查询MyBatis Mapper对应的SQL语句、Mapper相关信息
     *
     * @param mapperClassName  MyBatis Mapper类名
     * @param mapperMethodName MyBatis Mapper方法名
     * @return
     */
    public WriteDbData4MyBatisMSFormatedSql queryMapperSqlHash(String mapperClassName, String mapperMethodName) {
        return queryMapperSqlHash(mapperClassName, mapperMethodName, "");
    }

    /**
     * 查询MyBatis Mapper对应的SQL语句、Mapper相关信息
     *
     * @param mapperClassName  MyBatis Mapper类名
     * @param mapperMethodName MyBatis Mapper方法名
     * @param tableSuffix      当前解析jar文件结果写入的数据库表名后缀
     * @return
     */
    public WriteDbData4MyBatisMSFormatedSql queryMapperSqlHash(String mapperClassName, String mapperMethodName, String tableSuffix) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMFS_QUERY_SQL_HASH;
        String sql = dbOperWrapper.getCachedSqlWithSuffix(sqlKeyEnum, tableSuffix);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_MYBATIS_MS_FORMATED_SQL) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_FORMATED_SQL.getTableName(appName, tableSuffix) +
                    " where " + DC.MMFS_MAPPER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MMFS_SQL_ID + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSqlWithSuffix(sqlKeyEnum, sql, tableSuffix);
        }

        return dbOperator.queryObject(sql, WriteDbData4MyBatisMSFormatedSql.class, dbOperWrapper.querySimpleClassName(mapperClassName, tableSuffix), mapperMethodName);
    }

    /**
     * 查询MyBatis Mapper对应的XML文件路径
     *
     * @param mapperClassName  MyBatis Mapper类名
     * @param mapperMethodName MyBatis Mapper方法名
     * @param tableSuffix      当前解析jar文件结果写入的数据库表名后缀
     * @return
     */
    public String queryMapperXmlFilePath(String mapperClassName, String mapperMethodName, String tableSuffix) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMFS_QUERY_XML_FILE_PATH;
        String sql = dbOperWrapper.getCachedSqlWithSuffix(sqlKeyEnum, tableSuffix);
        if (sql == null) {
            sql = "select " + DC.MMFS_XML_FILE_PATH +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_FORMATED_SQL.getTableName(appName, tableSuffix) +
                    " where " + DC.MMFS_MAPPER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MMFS_SQL_ID + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSqlWithSuffix(sqlKeyEnum, sql, tableSuffix);
        }

        return dbOperator.queryObjectOneColumn(sql, String.class, dbOperWrapper.querySimpleClassName(mapperClassName, tableSuffix), mapperMethodName);
    }
}
