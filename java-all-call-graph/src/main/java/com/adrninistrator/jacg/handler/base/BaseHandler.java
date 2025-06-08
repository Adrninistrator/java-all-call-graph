package com.adrninistrator.jacg.handler.base;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.spring.context.SpringContextManager;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;

import java.io.Closeable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/5
 * @description: 处理类的基类，实现了Closeable接口，支持通过try-with-resource方法确保执行完毕时数据库被关闭
 */
public abstract class BaseHandler implements Closeable {
    private static final Logger logger = LoggerFactory.getLogger(BaseHandler.class);

    public static final String SQL_KEY_QUERY_END_ID_BY_PAGE = "@queryEndIdByPage@";

    protected final boolean useH2Db;

    protected DbOperator dbOperator;

    protected DbOperWrapper dbOperWrapper;

    protected ConfigureWrapper configureWrapper;

    /*
        数据库表名后缀
        null或空字符串: 不使用数据库表名后缀 非null且非空字符串: 使用指定的数据库表名后缀
     */
    private String tableSuffix;

    // 记录是否需要关闭数据库操作对象
    private boolean needCloseDb = false;

    protected String appName;

    protected ApplicationContext applicationContext;

    /**
     * 调用该构造函数时，[会]创建新的数据源，结束前[需要]手动关闭数据库操作对象
     * 建议使用try-with-resource方式创建当前类的子类实例，保证操作结束时关闭数据源
     *
     * @param configureWrapper 配置包装类对象，不允许为null，可以为new出来的ConfigureWrapper对象
     */
    public BaseHandler(ConfigureWrapper configureWrapper) {
        this(configureWrapper, null);
    }

    /**
     * 调用该构造函数时，【会】创建新的数据源，结束前【需要】手动关闭数据库操作对象
     * 建议使用try-with-resource方式创建当前类的子类实例，保证操作结束时关闭数据源
     *
     * @param configureWrapper 配置包装类对象，不允许为null，可以为new出来的ConfigureWrapper对象
     * @param tableSuffix      数据库表名后缀
     */
    protected BaseHandler(ConfigureWrapper configureWrapper, String tableSuffix) {
        if (configureWrapper == null) {
            throw new JavaCG2RuntimeException("传入配置不允许为null");
        }

        if (useNeo4j()) {
            applicationContext = SpringContextManager.getApplicationContext();
            this.tableSuffix = null;
            // 完成需要使用的基础配置的初始化
            dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, true, this);
            // 获取appName不能使用 dbOperWrapper.getDbOperator().getAppName(); 因为 dbOperWrapper.getDbOperator() 为null
            appName = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_APP_NAME);
            useH2Db = false;
            return;
        }

        // 处理数据库表名后缀
        ConfigureWrapper usedConfigureWrapper;
        if (StringUtils.isNotBlank(tableSuffix)) {
            usedConfigureWrapper = configureWrapper.copy();
            usedConfigureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, tableSuffix);
        } else {
            usedConfigureWrapper = configureWrapper;
        }

        this.tableSuffix = tableSuffix;
        // 完成需要使用的基础配置的初始化
        dbOperWrapper = DbInitializer.genDbOperWrapper(usedConfigureWrapper, false, this);
        dbOperator = dbOperWrapper.getDbOperator();
        appName = dbOperator.getAppName();
        this.configureWrapper = usedConfigureWrapper;
        useH2Db = dbOperator.isUseH2Db();
        logger.warn("调用该构造函数时，结束前[需要]手动关闭数据库操作对象");
        needCloseDb = true;
    }

    /**
     * 调用该构造函数时，【不会】创建新的数据源，结束前【不需要】手动关闭数据库操作对象
     * 在BaseHandler子类构造函数中创建其他BaseHandler子类时，建议使用该方法
     *
     * @param dbOperWrapper 已完成初始化的数据库操作包装对象
     */
    public BaseHandler(DbOperWrapper dbOperWrapper) {
        if (dbOperWrapper == null || (!useNeo4j() && dbOperWrapper.getDbOperator() == null)) {
            logger.error("{} 参数不允许为空", this.getClass().getName());
            throw new JavaCG2RuntimeException("参数不允许为空");
        }

        this.dbOperator = dbOperWrapper.getDbOperator();
        appName = dbOperator.getAppName();
        this.dbOperWrapper = dbOperWrapper;
        this.configureWrapper = dbOperWrapper.getConfigureWrapper();
        useH2Db = dbOperator.isUseH2Db();
    }

    protected boolean useNeo4j() {
        return false;
    }

    /**
     * 从数据库表分页查询当前处理的结束ID
     *
     * @param startRecordId   起始记录ID，大于指定的值
     * @param dbTableInfoEnum 对应数据库信息枚举
     * @param idColumnName    ID字符名称
     * @return
     */
    protected int queryEndIdByPage(int startRecordId, DbTableInfoEnum dbTableInfoEnum, String idColumnName) {
        logger.debug("从数据库表分页查询当前处理的结束ID {} {} {}", dbTableInfoEnum.getTableName(), idColumnName, startRecordId);
        String sqlKey = dbTableInfoEnum.getTableNameKeyword() + SQL_KEY_QUERY_END_ID_BY_PAGE + idColumnName;
        String sql = dbOperWrapper.getCachedSql(sqlKey);
        if (sql == null) {
            sql = "select " + idColumnName +
                    " from " + dbTableInfoEnum.getTableName() +
                    " where " + idColumnName + " > ?" +
                    " order by " + idColumnName +
                    " limit ?, 1";
            sql = dbOperWrapper.cacheSql(sqlKey, sql, sqlKey);
        }

        Integer endRecordId = dbOperator.queryObjectOneColumn(sql, Integer.class, startRecordId, JACGConstants.DB_PAGE_HANDLE_SIZE_MINUS_1);
        return chooseEndIdByPage(endRecordId);
    }

    /**
     * 处理分页查询本次查询到的结束ID
     *
     * @param endId 查询到的结束ID
     * @return 若为null则返回代表最后一次分页查询；若非null则原样返回
     */
    protected int chooseEndIdByPage(Integer endId) {
        if (endId == null) {
            // 最后一次分页查询
            logger.debug("最后一次分页查询");
            return JACGConstants.PAGE_QUERY_LAST;
        }

        // 不是最后一次分页查询
        logger.debug("查询到结束ID {}", endId);
        return endId;
    }

    /**
     * 通过一个或多个字段结合ID字段分页查询结束ID的公共方法
     *
     * @param startRecordId   起始记录ID，大于指定的值
     * @param dbTableInfoEnum 对应数据库信息枚举
     * @param idColumnName    ID字符名称
     * @param queryColumnName 用于查询的字段名称数组
     * @param queryValue      用于查询的字段对应的值数组
     * @return
     */
    protected int queryEndIdOneColumn(int startRecordId, DbTableInfoEnum dbTableInfoEnum, String idColumnName, String queryColumnName, Object queryValue) {
        return queryEndIdMultiColumns(startRecordId, dbTableInfoEnum, idColumnName, new String[]{queryColumnName}, new Object[]{queryValue});
    }

    /**
     * 通过一个或多个字段结合ID字段分页查询结束ID的公共方法
     *
     * @param startRecordId    起始记录ID，大于指定的值
     * @param dbTableInfoEnum  对应数据库信息枚举
     * @param idColumnName     ID字符名称
     * @param queryColumnNames 用于查询的字段名称数组
     * @param queryValues      用于查询的字段对应的值数组
     * @return
     */
    protected int queryEndIdMultiColumns(int startRecordId, DbTableInfoEnum dbTableInfoEnum, String idColumnName, String[] queryColumnNames, Object[] queryValues) {
        if (ArrayUtils.isEmpty(queryColumnNames)) {
            logger.error("用于查询的字段名称数组为空");
            throw new JavaCG2RuntimeException("用于查询的字段名称数组为空");
        }
        if (ArrayUtils.isEmpty(queryValues)) {
            logger.error("用于查询的字段对应的值数组为空");
            throw new JavaCG2RuntimeException("用于查询的字段对应的值数组为空");
        }
        if (queryColumnNames.length != queryValues.length) {
            logger.error("用于查询的字段名称数组与用于查询的字段对应的值数组数量不同 {} {}", queryColumnNames.length, queryValues.length);
            throw new JavaCG2RuntimeException("用于查询的字段名称数组与用于查询的字段对应的值数组数量不同");
        }
        String combinedColumnName = StringUtils.join(queryColumnNames, ",");
        logger.debug("从数据库表分页查询当前处理的结束ID，多字段 {} {} {} {}", dbTableInfoEnum.getTableName(), idColumnName, startRecordId, combinedColumnName);
        String sqlKey = dbTableInfoEnum.getTableNameKeyword() + SQL_KEY_QUERY_END_ID_BY_PAGE + idColumnName + JACGConstants.FLAG_AT + combinedColumnName;
        String sql = dbOperWrapper.getCachedSql(sqlKey);
        if (sql == null) {
            StringBuilder stringBuilder = new StringBuilder("select ").append(idColumnName)
                    .append(" from ").append(dbTableInfoEnum.getTableName())
                    .append(" where ");
            for (int i = 0; i < queryColumnNames.length; i++) {
                if (i > 0) {
                    stringBuilder.append(" and ");
                }
                stringBuilder.append(queryColumnNames[i]).append(" = ?");
            }
            stringBuilder.append(" and ").append(idColumnName).append(" > ?")
                    .append(" limit ?, 1");
            sql = dbOperWrapper.cacheSql(sqlKey, stringBuilder.toString(), sqlKey);
        }
        List<Object> argList = new ArrayList<>(queryValues.length + 2);
        argList.addAll(Arrays.asList(queryValues));
        argList.add(startRecordId);
        argList.add(JACGConstants.DB_PAGE_HANDLE_SIZE_MINUS_1);
        Integer endCallId = dbOperator.queryObjectOneColumn(sql, Integer.class, argList.toArray());
        return chooseEndIdByPage(endCallId);
    }

    /**
     * 分页查询本次从方法调用表根据被调用类唯一类名查询的结束call_id
     *
     * @param calleeSimpleClassName 被调用类唯一类名
     * @param startCallId           起始调用ID
     * @return
     */
    protected int queryEndCallIdEESCNByPage(String calleeSimpleClassName, int startCallId) {
        return queryEndIdOneColumn(startCallId, DbTableInfoEnum.DTIE_METHOD_CALL, DC.MC_CALL_ID, DC.MC_CALLEE_SIMPLE_CLASS_NAME, calleeSimpleClassName);
    }

    /**
     * 从方法调用表分页查询指定调用方法本次结束的call_id
     *
     * @param callerMethodHash 调用方法HASH+长度
     * @param startCallId      起始方法调用ID
     * @return
     */
    protected int queryEndCallIdERMHByPage(String callerMethodHash, int startCallId) {
        return queryEndIdOneColumn(startCallId, DbTableInfoEnum.DTIE_METHOD_CALL, DC.MC_CALL_ID, DC.MC_CALLER_METHOD_HASH, callerMethodHash);
    }

    @Override
    public void close() {
        if (needCloseDb && dbOperator != null) {
            // 关闭数据源
            dbOperator.closeDs(this);
        }
    }

    public String getTableSuffix() {
        return tableSuffix;
    }
}
