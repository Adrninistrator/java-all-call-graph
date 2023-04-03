package com.adrninistrator.jacg.handler.mybatis;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.mybatis.MyBatisMySqlTableInfo;
import com.adrninistrator.jacg.handler.dto.mybatis.MyBatisMySqlWriteTableInfo;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import com.adrninistrator.mybatis_mysql_table_parser.common.enums.MySqlStatementEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/15
 * @description: 对MyBatis Mapper的处理类
 */
public class MyBatisMapperHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisMapperHandler.class);

    public MyBatisMapperHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public MyBatisMapperHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 获取MyBatis Mapper对应的数据库表信息
     *
     * @param mapperClassName
     * @param mapperMethodName
     * @return
     */
    public MyBatisMySqlTableInfo queryMyBatisMySqlTableInfo(String mapperClassName, String mapperMethodName) {
        String mapperSimpleClassName = dbOperWrapper.getSimpleClassName(mapperClassName);

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMT_QUERY_TABLE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MMT_SQL_STATEMENT, DC.MMT_TABLE_NAME) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_TABLE.getTableName() +
                    " where " + DC.MMT_MAPPER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MMT_MAPPER_METHOD_NAME + " = ?" +
                    " order by " + JACGSqlUtil.joinColumns(DC.MMT_SQL_STATEMENT, DC.MMT_TABLE_SEQ);
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{mapperSimpleClassName, mapperMethodName});
        if (JavaCGUtil.isCollectionEmpty(list)) {
            logger.error("未查询到MyBatis Mapper对应的数据库表信息 {} {}", mapperClassName, mapperMethodName);
            return null;
        }

        MyBatisMySqlTableInfo myBatisMySqlTableInfo = new MyBatisMySqlTableInfo();
        Map<String, List<String>> tableNameMap = new HashMap<>();
        for (Map<String, Object> map : list) {
            String sqlStatement = (String) map.get(DC.MMT_SQL_STATEMENT);
            String tableName = (String) map.get(DC.MMT_TABLE_NAME);
            List<String> tableNameList = tableNameMap.computeIfAbsent(sqlStatement, k -> new ArrayList<>());
            tableNameList.add(tableName);
        }

        for (Map.Entry<String, List<String>> entry : tableNameMap.entrySet()) {
            String sqlStatement = entry.getKey();
            List<String> tableNameList = entry.getValue();
            if (MySqlStatementEnum.DSSE_SELECT.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setSelectTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_SELECT_4_UPDATE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setSelect4UpdateTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_INSERT.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setInsertTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_INSERT_IGNORE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setInsertIgnoreTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_INSERT_OR_UPDATE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setInsertOrUpdateTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_REPLACE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setReplaceTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_UPDATE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setUpdateTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_DELETE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setDeleteTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_ALTER.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setAlterTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_TRUNCATE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setTruncateTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_CREATE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setCreateTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_DROP.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setDropTableList(tableNameList);
            }
        }

        return myBatisMySqlTableInfo;
    }

    /**
     * 获取MyBatis Mapper对应的写数据库表信息
     *
     * @param mapperClassName
     * @param mapperMethodName
     * @return
     */
    public MyBatisMySqlWriteTableInfo queryMyBatisMySqlWriteTableInfo(String mapperClassName, String mapperMethodName) {
        String mapperSimpleClassName = dbOperWrapper.getSimpleClassName(mapperClassName);

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMWT_QUERY_TABLE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MMWT_SQL_STATEMENT, DC.MMWT_TABLE_NAME) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_WRITE_TABLE.getTableName() +
                    " where " + DC.MMWT_MAPPER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MMWT_MAPPER_METHOD_NAME + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Map<String, Object> map = dbOperator.queryOneRow(sql, new Object[]{mapperSimpleClassName, mapperMethodName});
        if (JACGUtil.isMapEmpty(map)) {
            logger.error("未查询到MyBatis Mapper对应的写数据库表信息 {} {}", mapperClassName, mapperMethodName);
            return null;
        }

        String sqlStatement = (String) map.get(DC.MMT_SQL_STATEMENT);
        String tableName = (String) map.get(DC.MMT_TABLE_NAME);
        return new MyBatisMySqlWriteTableInfo(sqlStatement, tableName);
    }
}
