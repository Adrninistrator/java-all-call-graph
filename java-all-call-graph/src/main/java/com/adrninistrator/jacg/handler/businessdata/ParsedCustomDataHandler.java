package com.adrninistrator.jacg.handler.businessdata;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ParsedCustomData;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/6/6
 * @description: 解析jar文件时获取的自定义数据处理类
 */
public class ParsedCustomDataHandler extends BaseHandler {

    private static final Logger logger = LoggerFactory.getLogger(ParsedCustomDataHandler.class);

    public ParsedCustomDataHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public ParsedCustomDataHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 通过数据类型查询解析jar文件时获取的自定义数据
     *
     * @param dataType
     * @return
     */
    public List<WriteDbData4ParsedCustomData> queryParsedCustomDataByType(String dataType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.PCD_QUERY_BY_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_PARSED_CUSTOM_DATA) +
                    " from " + DbTableInfoEnum.DTIE_PARSED_CUSTOM_DATA.getTableName() +
                    " where " + DC.PCD_DATA_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4ParsedCustomData.class, dataType);
    }

    /**
     * 通过数据类型与数据key查询解析jar文件时获取的自定义数据
     *
     * @param dataType
     * @param dataKey
     * @return
     */
    public List<WriteDbData4ParsedCustomData> queryParsedCustomDataByTypeKey(String dataType, String dataKey) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.PCD_QUERY_BY_TYPE_KEY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_PARSED_CUSTOM_DATA) +
                    " from " + DbTableInfoEnum.DTIE_PARSED_CUSTOM_DATA.getTableName() +
                    " where " + DC.PCD_DATA_TYPE + " = ?" +
                    " and " + DC.PCD_DATA_KEY + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4ParsedCustomData.class, dataType, dataKey);
    }

    /**
     * 通过数据类型与数据key查询解析jar文件时获取的自定义数据，仅查询1条
     *
     * @param dataType
     * @param dataKey
     * @return
     */
    public WriteDbData4ParsedCustomData querySingleParsedCustomDataByTypeKey(String dataType, String dataKey) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.PCD_QUERY_SINGLE_BY_TYPE_KEY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_PARSED_CUSTOM_DATA) +
                    " from " + DbTableInfoEnum.DTIE_PARSED_CUSTOM_DATA.getTableName() +
                    " where " + DC.PCD_DATA_TYPE + " = ?" +
                    " and " + DC.PCD_DATA_KEY + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4ParsedCustomData.class, dataType, dataKey);
    }
}
