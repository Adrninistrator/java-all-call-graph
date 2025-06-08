package com.adrninistrator.jacg.dboper;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.exceptions.JACGSQLException;
import com.adrninistrator.jacg.conf.DbConfInfo;
import com.adrninistrator.jacg.druidfilter.DruidMonitorFilter;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import com.alibaba.druid.pool.DataSourceClosedException;
import com.alibaba.druid.pool.DruidDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.BeanPropertyRowMapper;

import java.sql.Connection;
import java.sql.SQLSyntaxErrorException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description: 数据库操作对象
 */

public class DbOperator implements AutoCloseable {
    private static final Logger logger = LoggerFactory.getLogger(DbOperator.class);

    private static final AtomicInteger ATOMIC_INTEGER = new AtomicInteger(0);

    private final Map<String, BeanPropertyRowMapper<?>> beanPropertyRowMapperMap = new ConcurrentHashMap<>();

    private final DbConfInfo dbConfInfo;

    private final JdbcTemplateQuiet jdbcTemplate;

    private final String objSeq;

    private final DruidDataSource dataSource;

    // 记录当前对象被引用的次数
    private final JavaCG2Counter referenceCounter = new JavaCG2Counter(0);

    // 引用了当前类的对象类名及HASH列表
    private final List<String> referenceCLassNameAndHashList = new ArrayList<>();

    // 释放了当前类的对象类名及HASH列表
    private final List<String> releaseCLassNameAndHashList = new ArrayList<>();

    private boolean closed = false;

    DbOperator(DbConfInfo dbConfInfo, String initDbOperSimpleCLassNameAndHash) {
        this.dbConfInfo = dbConfInfo;
        // 处理引用当前对象的信息
        referenceCounter.addAndGet();
        referenceCLassNameAndHashList.add(initDbOperSimpleCLassNameAndHash);

        objSeq = "dbo@" + ATOMIC_INTEGER.incrementAndGet();
        logger.info("[{}] 创建数据库操作对象 {} {}", objSeq, initDbOperSimpleCLassNameAndHash, dbConfInfo);

        dataSource = new DruidDataSource();
        dataSource.setMaxActive(dbConfInfo.getMaxActive());
        dataSource.setTestOnBorrow(false);
        dataSource.setTestOnReturn(false);
        dataSource.setTestWhileIdle(false);
        dataSource.setProxyFilters(Collections.singletonList(new DruidMonitorFilter(dbConfInfo.isUseH2Db())));
        initDataSource();

        jdbcTemplate = new JdbcTemplateQuiet(dataSource);

        // 在JVM关闭时检查当前数据库操作对象是否有关闭
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            if (!closed) {
                logger.error("[{}] 数据库操作对象未关闭\n当前类目前被引用的次数为 {}\n引用过当前类的对象类名及HASH为\n{}\n释放过当前类的对象类名及HASH为\n{}", objSeq, referenceCounter.getCount(),
                        StringUtils.join(referenceCLassNameAndHashList, "\n"), StringUtils.join(releaseCLassNameAndHashList, "\n"));
                closeDs(this);
            }
        }));
    }

    private void initDataSource() {
        if (dbConfInfo.isUseH2Db()) {
            initH2Db();
            return;
        }
        initNonH2Db();
    }

    private void initH2Db() {
        dataSource.setDriverClassName("org.h2.Driver");
        String h2DbJdbcUrl = JACGConstants.H2_PROTOCOL + dbConfInfo.getDbH2FilePath() +
                ";MODE=MySQL;DATABASE_TO_LOWER=TRUE;CASE_INSENSITIVE_IDENTIFIERS=TRUE;INIT=CREATE SCHEMA IF NOT EXISTS " +
                JACGConstants.H2_SCHEMA + "\\;SET SCHEMA " + JACGConstants.H2_SCHEMA;
        logger.info("[{}] 初始化H2数据源 URL: {}", objSeq, h2DbJdbcUrl);

        dataSource.setUrl(h2DbJdbcUrl);
        dataSource.setUsername("");
        dataSource.setPassword("");
    }

    private void initNonH2Db() {
        dataSource.setDriverClassName(dbConfInfo.getDriverClassName());
        dataSource.setUrl(dbConfInfo.getDbUrl());
        dataSource.setUsername(dbConfInfo.getUsername());
        dataSource.setPassword(dbConfInfo.getPassword());
    }

    public Connection getConnection() {
        synchronized (DbOperator.class) {
            try {
//            return DriverManager.getConnection(confInfo.getDbUrl(), confInfo.getDbUsername(), confInfo.getDbPassword());
                return dataSource.getConnection();
            } /*
                数据库连接不会在AbstractRunner子类与其他类中复用，以下异常应该不会再出现（使用C3P0数据源）
                catch (SQLException e) {
                    if (StringUtils.contains(e.getMessage(), " has been closed() -- you can no longer use it.")) {
                        // 以上错误信息见com.mchange.v2.c3p0.impl.AbstractPoolBackedDataSource类，assertCpds()方法
                        logger.error("数据源已被关闭");
                    }
                    logger.error("getConnection SQLException error ", e);
                    return null;
                }
            */ catch (Exception e) {
                logger.error("[{}] getConnection error ", objSeq, e);
                return null;
            }
        }
    }

    /**
     * 关闭数据源
     *
     * @param callerObject 调用当前方法的对象
     */
    public void closeDs(Object callerObject) {
        if (!closed) {
            String closeDbOperSimpleCLassNameAndHash = JACGUtil.getObjSimpleClassNameAndHash(callerObject);
            if (DbOperator.class == callerObject.getClass()) {
                // 调用当前方法的类为当前类，说明使用了try-with-resource方式，获取调用当前方法的方法
                String callerMethod = JACGClassMethodUtil.getMethodInStackTrace(Thread.currentThread().getStackTrace(), 3);
                closeDbOperSimpleCLassNameAndHash += (JACGConstants.FLAG_AT + callerMethod);
            }
            // 处理释放当前对象的信息
            releaseCLassNameAndHashList.add(closeDbOperSimpleCLassNameAndHash);
            if (referenceCounter.minusAndGet() == 0) {
                logger.info("[{}] 关闭数据源\n引用过当前类的对象类名及HASH为\n{}\n释放过当前类的对象类名及HASH为\n{}", objSeq, StringUtils.join(referenceCLassNameAndHashList, "\n"),
                        StringUtils.join(releaseCLassNameAndHashList, "\n"));
                dataSource.close();
                closed = true;
                return;
            }
            logger.info("[{}] 暂不关闭数据源，当前类被引用次数为 {} ，调用当前方法的对象为: {}", objSeq, referenceCounter.getCount(), closeDbOperSimpleCLassNameAndHash);
            return;
        }
        logger.info("[{}] 数据源已被关闭，不需要再关闭，调用当前方法的对象为: {}", objSeq, JACGUtil.getObjSimpleClassNameAndHash(callerObject));
    }

    /**
     * 创建数据库表
     *
     * @param sql
     * @return
     */
    public boolean createTable(String sql) {
        if (!executeDDLSql(sql)) {
            return false;
        }

        String tableName = StringUtils.substringBetween(sql, JACGConstants.SQL_CREATE_TABLE_HEAD, JavaCG2Constants.FLAG_LEFT_BRACKET);
        if (StringUtils.isBlank(tableName)) {
            logger.error("建表SQL语句中未找到表名 {}", sql);
            return false;
        }
        tableName = tableName.trim();

        // 检查数据库表是否创建成功，可能出现上述建表语句执行失败但未抛出异常的情况
        if (!checkTableExists(tableName)) {
            return false;
        }
        logger.info("[{}] 数据库表创建成功 [{}]", objSeq, tableName);
        return true;
    }

    /**
     * 检查指定的数据库表是否存在
     *
     * @param tableName
     * @return
     */
    public boolean checkTableExists(String tableName) {
        if (dbConfInfo.isUseH2Db()) {
            return checkTableExistsH2(tableName);
        }
        return checkTableExistsNonH2(tableName);
    }

    /**
     * 检查指定的数据库表是否存在
     *
     * @param dbTableInfoEnum
     * @return
     */
    public boolean checkTableExists(DbTableInfoEnum dbTableInfoEnum) {
        String tableName = JACGSqlUtil.replaceFlagInSql(dbTableInfoEnum.getTableName(), getAppName(), getTableSuffix());
        return checkTableExists(tableName);
    }

    /**
     * 判断数据库表是否存在，H2数据库
     *
     * @param tableName
     * @return
     */
    private boolean checkTableExistsH2(String tableName) {
        List<String> list = queryListOneColumn("SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES where TABLE_SCHEMA = ? and TABLE_NAME = ?",
                String.class, JACGConstants.H2_SCHEMA, tableName);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            logger.error("数据库表不存在 [{}]", tableName);
            return false;
        }
        return true;
    }

    /**
     * 判断数据库表是否存在，非H2数据库
     *
     * @param tableName
     * @return
     */
    private boolean checkTableExistsNonH2(String tableName) {
        List<String> list = queryListOneColumn("show tables like ?", String.class, tableName);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            logger.error("数据库表不存在 [{}]", tableName);
            return false;
        }
        return true;
    }

    /**
     * 删除数据库表
     *
     * @param tableName
     * @return
     */
    public boolean dropTable(String tableName) {
        String sql = "drop table if exists " + tableName;
        sql = JACGSqlUtil.replaceFlagInSql(sql, getAppName(), getTableSuffix());
        logger.info("[{}] drop table sql: [{}]", objSeq, sql);
        return executeDDLSql(sql);
    }

    /**
     * 清空数据库表
     *
     * @param tableName
     * @return
     */
    public boolean truncateTable(String tableName) {
        String sql = "truncate table " + tableName;
        sql = JACGSqlUtil.replaceFlagInSql(sql, getAppName(), getTableSuffix());
        logger.info("[{}] truncate table sql: [{}]", objSeq, sql);
        return executeDDLSql(sql);
    }

    /**
     * 执行DDL语句
     *
     * @param sql
     * @return
     */
    public boolean executeDDLSql(String sql) {
        try {
            jdbcTemplate.execute(sql);
            return true;
        } catch (Exception e) {
            logger.error("error [{}] ", sql, e);
            return false;
        }
    }

    /**
     * 更新数据库
     *
     * @param sql
     * @param arguments
     * @return
     */
    public Integer update(String sql, Object... arguments) {
        try {
            return jdbcTemplate.update(sql, arguments);
        } catch (Exception e) {
            if (!handleSpecialException(e, sql)) {
                logger.error("更新失败 [{}] ", sql, e);
            }
            throw new JACGSQLException("更新失败");
        }
    }

    /**
     * 批量写入数据库
     *
     * @param sql
     * @param argumentList
     * @return
     */
    public boolean batchInsert(String sql, List<Object[]> argumentList) {
        try {
            jdbcTemplate.batchUpdate(sql, argumentList);
            return true;
        } catch (Exception e) {
            if (!handleSpecialException(e, sql)) {
                logger.error("插入失败 sql: [{}] ", sql, e);
            }
            // 打印插入失败的数据
            for (int i = 0; i < argumentList.size(); i++) {
                logger.error("插入失败的数据 序号: [{}] 数据: [{}]", i, StringUtils.join(argumentList.get(i), JACGConstants.FLAG_COMMA_WITH_SPACE));
            }
            throw new JACGSQLException("插入失败");
        }
    }

    /**
     * 写入数据库
     *
     * @param sql
     * @param arguments
     * @return
     */
    public boolean insert(String sql, Object... arguments) {
        try {
            jdbcTemplate.update(sql, arguments);
            return true;
        } catch (Exception e) {
            logger.error("error [{}] ", sql, e);
            return false;
        }
    }

    /**
     * 查询列表，仅包含一列，返回类型为基本类型
     *
     * @param sql
     * @param type
     * @param arguments
     * @return
     */
    public <T> List<T> queryListOneColumn(String sql, Class<T> type, Object... arguments) {
        if (!JACGUtil.checkJavaBasicWrapperType(type)) {
            throw new JavaCG2RuntimeException("查询返回类型需要使用Java基本类型 " + type.getName());
        }
        try {
            return jdbcTemplate.queryForList(sql, type, arguments);
        } catch (Exception e) {
            if (!handleSpecialException(e, sql)) {
                logger.error("查询失败 [{}] [{}] ", sql, StringUtils.join(arguments, " "), e);
            }
            throw new JACGSQLException("查询失败");
        }
    }

    /**
     * 查询列表，包含多列，返回类型非基本类型
     *
     * @param sql
     * @param type
     * @param arguments
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T> List<T> queryList(String sql, Class<T> type, Object... arguments) {
        if (JACGUtil.checkJavaBasicWrapperType(type)) {
            throw new JavaCG2RuntimeException("查询返回类型不允许使用Java基本类型 " + type.getName());
        }
        try {
            BeanPropertyRowMapper<?> beanPropertyRowMapper = beanPropertyRowMapperMap.computeIfAbsent(type.getName(),
                    k -> new BeanPropertyRowMapper<>(type));
            return jdbcTemplate.query(sql, (BeanPropertyRowMapper<T>) beanPropertyRowMapper, arguments);
        } catch (Exception e) {
            if (!handleSpecialException(e, sql)) {
                logger.error("查询失败 [{}] [{}] ", sql, StringUtils.join(arguments, " "), e);
            }
            throw new JACGSQLException("查询失败");
        }
    }

    /**
     * 查询一行且只有一列的记录，返回类型为基本类型
     *
     * @param sql
     * @param type
     * @param arguments
     * @return
     */
    public <T> T queryObjectOneColumn(String sql, Class<T> type, Object... arguments) {
        if (!JACGUtil.checkJavaBasicWrapperType(type)) {
            throw new JavaCG2RuntimeException("查询返回类型需要使用Java基本类型 " + type.getName());
        }
        try {
            return jdbcTemplate.queryForObject(sql, type, arguments);
        } catch (Exception e) {
            if (!handleSpecialException(e, sql)) {
                logger.error("查询失败 [{}] [{}] ", sql, StringUtils.join(arguments, " "), e);
            }
            throw new JACGSQLException("查询失败");
        }
    }

    /**
     * 查询一行记录，包含多列，返回类型非基本类型
     *
     * @param sql
     * @param type
     * @param arguments
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T> T queryObject(String sql, Class<T> type, Object... arguments) {
        if (JACGUtil.checkJavaBasicWrapperType(type)) {
            throw new JavaCG2RuntimeException("查询返回类型不允许使用Java基本类型 " + type.getName());
        }
        try {
            BeanPropertyRowMapper<?> beanPropertyRowMapper = beanPropertyRowMapperMap.computeIfAbsent(type.getName(),
                    k -> new BeanPropertyRowMapper<>(type));
            return jdbcTemplate.queryForObject(sql, (BeanPropertyRowMapper<T>) beanPropertyRowMapper, arguments);
        } catch (Exception e) {
            if (!handleSpecialException(e, sql)) {
                logger.error("查询失败 [{}] [{}] ", sql, StringUtils.join(arguments, " "), e);
            }
            throw new JACGSQLException("查询失败");
        }
    }

    /**
     * 处理特殊的异常
     *
     * @param e
     * @param sql
     * @return true: 不需要继续处理 false: 需要继续处理
     */
    private boolean handleSpecialException(Exception e, String sql) {
      /*
            使用H2数据库时，e的类型为org.h2.jdbc.JdbcSQLSyntaxErrorException
            使用MySQL数据库时，e.getCause()的类型为SQLSyntaxErrorException
         */
        if (ExceptionUtils.indexOfType(e, SQLSyntaxErrorException.class) != -1) {
            logger.error("[{}] 数据库操作失败" +
                    "\n1. 请检查数据库表是否有成功创建" +
                    "\n2. 再检查数据库表是否需要使用最新版本重新创建，可先drop对应的数据库表" +
                    "\n请重新执行 com.adrninistrator.jacg.unzip.UnzipFile 类释放最新的SQL语句（需要先删除现有的SQL语句）" +
                    "\n若使用H2数据库，需要删除对应的数据库文件 {}" +
                    "\n[{}] ", objSeq, dataSource.getUrl(), sql, e);
            return true;
        }
        if (ExceptionUtils.indexOfType(e, DataSourceClosedException.class) != -1) {
            logger.error("[{}] 当前数据源已被关闭，关闭数据源的对象为 {} [{}] ", objSeq, getLastReleaseCLassNameAndHash(), sql, e);
            return true;
        }
        return false;
    }

    /**
     * 使用try-with-resource方式时，使用以下方法关闭
     *
     * @throws Exception
     */
    @Override
    public void close() throws Exception {
        closeDs(this);
    }

    // 获得释放了当前类的对象类名及HASH列表中的最后一个
    private String getLastReleaseCLassNameAndHash() {
        int listSize = releaseCLassNameAndHashList.size();
        if (listSize <= 1) {
            return "";
        }
        return releaseCLassNameAndHashList.get(listSize - 1);
    }

    // 引用数据库操作类
    public void referenceDbOperator(String callerClassNameAndHash) {
        referenceCounter.addAndGet();
        referenceCLassNameAndHashList.add(callerClassNameAndHash);
        logger.info("[{}] 增加引用当前对象的信息 {} {}", objSeq, referenceCounter.getCount(), callerClassNameAndHash);
    }

    public String getAppName() {
        return dbConfInfo.getAppName();
    }

    public String getTableSuffix() {
        return dbConfInfo.getTableSuffix();
    }

    public int getDbInsertBatchSize() {
        return dbConfInfo.getDbInsertBatchSize();
    }

    public DbConfInfo getDbConfInfo() {
        return dbConfInfo;
    }

    public boolean isClosed() {
        return closed;
    }

    public boolean isUseH2Db() {
        return dbConfInfo.isUseH2Db();
    }
}
