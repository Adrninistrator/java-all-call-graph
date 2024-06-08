package com.adrninistrator.jacg.handler.conf;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4PropertiesConf;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/9/18
 * @description: 查询properties配置文件的类
 */
public class PropertiesConfHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(PropertiesConfHandler.class);

    public PropertiesConfHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public PropertiesConfHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 查询properties文件中指定名称的配置值
     *
     * @param propKey properties文件配置的名称
     * @return
     */
    public List<String> queryPropConfValueList(String propKey) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.PC_QUERY_VALUE_BY_KEY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.PC_PROPERTIES_VALUE +
                    " from " + DbTableInfoEnum.DTIE_PROPERTIES_CONF.getTableName() +
                    " where " + DC.PC_PROPERTIES_KEY + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<String> list = dbOperator.queryListOneColumn(sql, String.class, propKey);
        // 查询出来的结果会有为空的数据，需要去掉
        List<String> returnList = new ArrayList<>();
        for (String value : list) {
            if (StringUtils.isNotBlank(value)) {
                returnList.add(value);
            }
        }
        return returnList;
    }

    /**
     * 查询properties文件中指定名称的配置信息
     *
     * @param propKey   properties文件配置的名称
     * @param usePrefix true: 配置名称使用前缀匹配 false: 配置名称完整匹配
     * @return
     */
    public List<WriteDbData4PropertiesConf> queryPropConfList(String propKey, boolean usePrefix) {
        SqlKeyEnum sqlKeyEnum = usePrefix ? SqlKeyEnum.PC_QUERY_ALL_BY_KEY_LIKE : SqlKeyEnum.PC_QUERY_ALL_BY_KEY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_PROPERTIES_CONF) +
                    " from " + DbTableInfoEnum.DTIE_PROPERTIES_CONF.getTableName();
            if (usePrefix) {
                sql = sql + " where " + DC.PC_PROPERTIES_KEY + " like concat(?, '%')";
            } else {
                sql = sql + " where " + DC.PC_PROPERTIES_KEY + " = ?";
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4PropertiesConf.class, propKey);
    }

    /**
     * 查询指定properties文件的配置信息
     *
     * @param propFileName properties文件名称，不包含目录
     * @return
     */
    public List<WriteDbData4PropertiesConf> queryPropConfListByFileName(String propFileName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.PC_QUERY_ALL_BY_FILE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_PROPERTIES_CONF) +
                    " from " + DbTableInfoEnum.DTIE_PROPERTIES_CONF.getTableName() +
                    " where " + DC.PC_PROPERTIES_FILE_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4PropertiesConf.class, propFileName);
    }

    /**
     * 查询数据库表properties文件配置的最大id
     *
     * @return
     */
    public int queryMaxRecordId() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.PC_QUERY_MAX_RECORD_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select ifnull(max(" + DC.PC_RECORD_ID + "),?) from " + DbTableInfoEnum.DTIE_PROPERTIES_CONF.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        Integer maxId = dbOperator.queryObjectOneColumn(sql, Integer.class, JavaCGConstants.RECORD_ID_MIN_BEFORE);
        if (maxId == null) {
            logger.error("查询数据库表properties文件配置的最大id为null");
            return JACGConstants.RECORD_ID_ILLEGAL;
        }
        return maxId;
    }

    /**
     * 向数据库表插入properties文件配置信息
     * 当前方法不允许并发调用
     *
     * @param propertiesConfList 需要插入的记录
     * @return
     */
    public synchronized boolean insertPropConfList(List<WriteDbData4PropertiesConf> propertiesConfList) {
        // 以下需要查询方法调用表最大序号，加1并使用，因此不能并发调用
        // 查询数据库表properties文件配置的最大id
        int maxRecordId = queryMaxRecordId();
        if (maxRecordId == JACGConstants.RECORD_ID_ILLEGAL) {
            return false;
        }
        logger.info("插入数据，当前的最大记录ID: {} 插入数量: {}", maxRecordId, propertiesConfList.size());
        String sql = dbOperWrapper.genAndCacheInsertSql(DbTableInfoEnum.DTIE_PROPERTIES_CONF, DbInsertMode.DIME_INSERT);
        List<Object[]> objectList = new ArrayList<>(propertiesConfList.size());
        for (WriteDbData4PropertiesConf propertiesConf : propertiesConfList) {
            objectList.add(new Object[]{
                    ++maxRecordId,
                    propertiesConf.getPropertiesKey(),
                    propertiesConf.getPropertiesFilePath(),
                    propertiesConf.getPropertiesFileName(),
                    propertiesConf.getPropertiesValue(),
            });
        }
        return dbOperator.batchInsert(sql, objectList);
    }
}
