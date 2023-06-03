package com.adrninistrator.jacg.dboper;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.util.JavaCGUtil;
import com.alibaba.druid.pool.DruidDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.BeanPropertyRowMapper;

import java.sql.Connection;
import java.sql.SQLSyntaxErrorException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description: 数据库操作对象
 */

public class DbOperator {
    private static final Logger logger = LoggerFactory.getLogger(DbOperator.class);

    private static final AtomicInteger ATOMIC_INTEGER = new AtomicInteger(0);

    private DruidDataSource dataSource;

    private final JdbcTemplateQuiet jdbcTemplate;

    private final Map<String, BeanPropertyRowMapper<?>> beanPropertyRowMapperMap = new ConcurrentHashMap<>();

    private final ConfigureWrapper configureWrapper;

    private final String appName;

    private final String objSeq;

    // 记录当前入口类的类名
    private final String entrySimpleClassName;

    private boolean useH2Db = false;

    private boolean closed = false;

    public static DbOperator genInstance(ConfigureWrapper configureWrapper, String entrySimpleClassName) {
        try {
            DbOperator instance = new DbOperator(configureWrapper, entrySimpleClassName);
//            Class.forName(confInfo.getDbDriverName());

            if (configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2)) {
                instance.initH2Db();
            } else {
                instance.initNonH2Db();
            }

            return instance;
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    private DbOperator(ConfigureWrapper configureWrapper, String entrySimpleClassName) {
        this.configureWrapper = configureWrapper;
        this.entrySimpleClassName = entrySimpleClassName;

        dataSource = new DruidDataSource();
        dataSource.setMaxActive(configureWrapper.getMainConfig(ConfigKeyEnum.CKE_THREAD_NUM));
        dataSource.setTestOnBorrow(false);
        dataSource.setTestOnReturn(false);
        dataSource.setTestWhileIdle(false);

        jdbcTemplate = new JdbcTemplateQuiet(dataSource);

        appName = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_APP_NAME);
        objSeq = String.valueOf(ATOMIC_INTEGER.incrementAndGet());
        logger.info("[{}] 创建数据库操作对象 {}", objSeq, entrySimpleClassName);

        // 在JVM关闭时检查当前数据库操作对象是否有关闭
        addShutdownHook();
    }

    // 在JVM关闭时检查当前数据库操作对象是否有关闭
    private void addShutdownHook() {
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            if (dataSource != null) {
                logger.error("[{}] 数据库操作对象未关闭 {}", objSeq, entrySimpleClassName);
                closeDs();
            }
        }));
    }

    private void initH2Db() {
        useH2Db = true;

        dataSource.setDriverClassName("org.h2.Driver");
        String h2DbJdbcUrl = JACGConstants.H2_PROTOCOL + configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH) +
                ";MODE=MySQL;DATABASE_TO_LOWER=TRUE;CASE_INSENSITIVE_IDENTIFIERS=TRUE;INIT=CREATE SCHEMA IF NOT EXISTS " +
                JACGConstants.H2_SCHEMA + "\\;SET SCHEMA " + JACGConstants.H2_SCHEMA;
        logger.info("[{}] 初始化H2数据源 URL: {}", objSeq, h2DbJdbcUrl);

        dataSource.setUrl(h2DbJdbcUrl);
        dataSource.setUsername("");
        dataSource.setPassword("");
    }

    private void initNonH2Db() {
        useH2Db = false;

        dataSource.setDriverClassName(configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME));
        dataSource.setUrl(configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_URL));
        dataSource.setUsername(configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME));
        dataSource.setPassword(configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD));

        logger.info("[{}] 初始化数据源", objSeq);
    }

    public void setMaxPoolSize(int maxPoolSize) {
        dataSource.setMaxActive(maxPoolSize);
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
     */
    public void closeDs() {
        if (dataSource != null) {
            logger.info("[{}] 关闭数据源", objSeq);
            dataSource.close();
            dataSource = null;
            closed = true;
        }
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

        String tableName = StringUtils.substringBetween(sql, JACGConstants.SQL_CREATE_TABLE_HEAD, JavaCGConstants.FLAG_LEFT_BRACKET);
        if (StringUtils.isBlank(tableName)) {
            logger.error("建表SQL语句中未找到表名 {}", sql);
            return false;
        }
        tableName = tableName.trim();

        // 检查数据库表是否创建成功，可能出现上述建表语句执行失败但未抛出异常的情况
        if (useH2Db) {
            if (!checkTableExistsH2(tableName)) {
                return false;
            }
        } else {
            if (!checkTableExistsNonH2(tableName)) {
                return false;
            }
        }

        logger.info("[{}] 数据库表创建成功 [{}]", objSeq, tableName);
        return true;
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
        if (JavaCGUtil.isCollectionEmpty(list)) {
            logger.error("数据库表创建失败 [{}]", tableName);
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
        if (JavaCGUtil.isCollectionEmpty(list)) {
            logger.error("数据库表创建失败 [{}]", tableName);
            return false;
        }
        return true;
    }

    /**
     * 清空数据库表
     *
     * @param tableName
     * @return
     */
    public boolean truncateTable(String tableName) {
        String sql = "truncate table " + tableName;
        sql = JACGSqlUtil.replaceAppNameInSql(sql, appName);
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
            if (!noticeDropTable(e, sql)) {
                logger.error("error [{}] ", sql, e);
            }
            return null;
        }
    }

    /**
     * 指量写入数据库
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
            if (!noticeDropTable(e, sql)) {
                if (argumentList.size() == 1) {
                    // 打印插入失败的数据
                    logger.error("error\nsql: [{}]\n数据: [{}]", sql, StringUtils.join(argumentList.get(0), JACGConstants.FLAG_COMMA_WITH_SPACE), e);
                } else {
                    logger.error("error 为了打印插入失败的数据，可将{} {}参数值设置为1\nsql: [{}]", ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE.getFileName(),
                            ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE.getKey(), sql, e);
                }
            }
            return false;
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
        try {
            return jdbcTemplate.queryForList(sql, type, arguments);
        } catch (Exception e) {
            if (!noticeDropTable(e, sql)) {
                logger.error("error [{}] [{}] ", sql, StringUtils.join(arguments, " "), e);
            }
            return null;
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
        try {
            BeanPropertyRowMapper<?> beanPropertyRowMapper = beanPropertyRowMapperMap.computeIfAbsent(type.getName(),
                    k -> new BeanPropertyRowMapper<>(type));
            return jdbcTemplate.query(sql, (BeanPropertyRowMapper<T>) beanPropertyRowMapper, arguments);
        } catch (Exception e) {
            if (!noticeDropTable(e, sql)) {
                logger.error("error [{}] [{}] ", sql, StringUtils.join(arguments, " "), e);
            }
            return null;
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
        try {
            return jdbcTemplate.queryForObject(sql, type, arguments);
        } catch (Exception e) {
            if (!noticeDropTable(e, sql)) {
                logger.error("error [{}] [{}] ", sql, StringUtils.join(arguments, " "), e);
            }
            return null;
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
        try {
            BeanPropertyRowMapper<?> beanPropertyRowMapper = beanPropertyRowMapperMap.computeIfAbsent(type.getName(),
                    k -> new BeanPropertyRowMapper<>(type));
            return jdbcTemplate.queryForObject(sql, (BeanPropertyRowMapper<T>) beanPropertyRowMapper, arguments);
        } catch (Exception e) {
            if (!noticeDropTable(e, sql)) {
                logger.error("error [{}] [{}] ", sql, StringUtils.join(arguments, " "), e);
            }
            return null;
        }
    }

    /**
     * 出现异常时，判断是否需要提示drop对应的数据库表
     *
     * @param e
     * @param sql
     * @return true: 需要 false: 不需要
     */
    private boolean noticeDropTable(Exception e, String sql) {
      /*
            使用H2数据库时，e的类型为org.h2.jdbc.JdbcSQLSyntaxErrorException
            使用MySQL数据库时，e.getCause()的类型为SQLSyntaxErrorException
         */
        if (ExceptionUtils.indexOfType(e, SQLSyntaxErrorException.class) != -1) {
            logger.error("\n请检查数据库表是否需要使用最新版本重新创建，可先drop对应的数据库表" +
                    "\n请重新执行 com.adrninistrator.jacg.unzip.UnzipFile 类释放最新的SQL语句（需要先删除现有的SQL语句）" +
                    "\n若使用H2数据库，需要删除对应的数据库文件 {}" +
                    "\n[{}] ", dataSource.getUrl(), sql, e);
            return true;
        }
        return false;
    }

    public String getAppName() {
        return appName;
    }

    public boolean isClosed() {
        return closed;
    }
}
