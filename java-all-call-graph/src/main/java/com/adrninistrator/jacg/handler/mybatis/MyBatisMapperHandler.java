package com.adrninistrator.jacg.handler.mybatis;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MyBatisMSTable;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MyBatisMSWriteTable;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.mybatis.MyBatisMySqlTableInfo;
import com.adrninistrator.jacg.util.JACGSqlUtil;
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

        List<WriteDbData4MyBatisMSTable> list = dbOperator.queryList(sql, WriteDbData4MyBatisMSTable.class, mapperSimpleClassName, mapperMethodName);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            logger.error("未查询到MyBatis Mapper对应的数据库表信息 {} {}", mapperClassName, mapperMethodName);
            return null;
        }

        MyBatisMySqlTableInfo myBatisMySqlTableInfo = new MyBatisMySqlTableInfo();
        Map<String, List<String>> tableNameMap = new HashMap<>();
        for (WriteDbData4MyBatisMSTable writeDbData4MyBatisMSTable : list) {
            List<String> tableNameList = tableNameMap.computeIfAbsent(writeDbData4MyBatisMSTable.getSqlStatement(), k -> new ArrayList<>());
            tableNameList.add(writeDbData4MyBatisMSTable.getTableName());
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
    public WriteDbData4MyBatisMSWriteTable queryMyBatisMySqlWriteTableInfo(String mapperClassName, String mapperMethodName) {
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
        WriteDbData4MyBatisMSWriteTable writeDbData4MyBatisMSWriteTable = dbOperator.queryObject(sql, WriteDbData4MyBatisMSWriteTable.class, mapperSimpleClassName,
                mapperMethodName);
        if (writeDbData4MyBatisMSWriteTable == null) {
            logger.error("未查询到MyBatis Mapper对应的写数据库表信息 {} {}", mapperClassName, mapperMethodName);
        }
        return writeDbData4MyBatisMSWriteTable;
    }
}
