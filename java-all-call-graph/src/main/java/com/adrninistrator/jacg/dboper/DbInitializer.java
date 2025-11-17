package com.adrninistrator.jacg.dboper;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.DbConfInfo;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.neo4j.dboper.Neo4jDbOperWrapper;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2024/3/16
 * @description: 用于创建数据库相关对象的类
 */
public class DbInitializer {

    private static final Logger logger = LoggerFactory.getLogger(DbInitializer.class);

    /**
     * 创建数据库操作包装类DbOperWrapper实例
     *
     * @param configureWrapper 配置参数类
     * @param callerObject     调用当前方法的对象
     * @return
     */
    public static DbOperWrapper genDbOperWrapper(ConfigureWrapper configureWrapper, Object callerObject) {
        return genDbOperWrapper(configureWrapper, false, callerObject);
    }

    /**
     * 创建数据库操作包装类DbOperWrapper实例，使用H2数据库时不使用只读模式
     *
     * @param configureWrapper 配置参数类
     * @param useNeo4j         是否使用neo4j
     * @param callerObject     调用当前方法的对象
     * @return
     */
    public static DbOperWrapper genDbOperWrapper(ConfigureWrapper configureWrapper, boolean useNeo4j, Object callerObject) {
        return genDbOperWrapper(configureWrapper, useNeo4j, false, callerObject);
    }

    /**
     * 创建数据库操作包装类DbOperWrapper实例
     *
     * @param configureWrapper 配置参数类
     * @param useNeo4j         是否使用neo4j
     * @param useNeo4j         使用H2数据库时是否使用只读模式
     * @param callerObject     调用当前方法的对象
     * @return
     */
    public static DbOperWrapper genDbOperWrapper(ConfigureWrapper configureWrapper, boolean useNeo4j, boolean h2DbReadOnly, Object callerObject) {
        if (!useNeo4j) {
            return genDbOperWrapperDb(configureWrapper, h2DbReadOnly, callerObject);
        }
        return genDbOperWrapperNeo4j(configureWrapper);
    }

    private static DbOperWrapper genDbOperWrapperDb(ConfigureWrapper configureWrapper, boolean h2DbReadOnly, Object callerObject) {
        String callerSimpleClassName = callerObject.getClass().getSimpleName();
        DbConfInfo dbConfInfo = new DbConfInfo();
        String appName = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_APP_NAME);
        int maxActive = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_THREAD_NUM);
        int dbInsertBatchSize = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE);
        boolean useH2Db = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2);
        String tableSuffix = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX);
        boolean slowQuerySwitch = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_SWITCH);
        int slowQueryTime = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_TIME);
        int slowQueryRowNum = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_ROW_NUM);

        dbConfInfo.setAppName(appName);
        dbConfInfo.setMaxActive(maxActive);
        dbConfInfo.setDbInsertBatchSize(dbInsertBatchSize);
        dbConfInfo.setUseH2Db(useH2Db);
        dbConfInfo.setTableSuffix(tableSuffix);
        dbConfInfo.setSlowQuerySwitch(slowQuerySwitch);
        dbConfInfo.setSlowQueryTime(slowQueryTime);
        dbConfInfo.setSlowQueryRowNum(slowQueryRowNum);

        if (useH2Db) {
            String dbH2FilePath = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH);
            dbConfInfo.setDbH2FilePath(dbH2FilePath);
            dbConfInfo.setH2DbReadOnly(h2DbReadOnly);
        } else {
            String driverClassName = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME);
            String dbUrl = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_URL);
            String username = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME);
            String password = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD);
            dbConfInfo.setDriverClassName(driverClassName);
            dbConfInfo.setDbUrl(dbUrl);
            dbConfInfo.setUsername(username);
            dbConfInfo.setPassword(password);
            if (StringUtils.containsIgnoreCase(driverClassName, "postgresql")) {
                dbConfInfo.setUsePgDb(true);
            }
        }

        String callerClassNameAndHash = JACGUtil.getObjSimpleClassNameAndHash(callerObject);
        // 判断当前配置类是否已创建数据库操作包装对象
        DbOperWrapper existedDbOperWrapper = configureWrapper.getDbOperWrapper();
        if (existedDbOperWrapper != null) {
            // 当前配置类已创建数据库操作包装对象
            DbOperator existedDbOperator = existedDbOperWrapper.getDbOperator();
            if (!existedDbOperator.getDbConfInfo().equals(dbConfInfo)) {
                logger.error("{} 当前配置类已有的数据库信息与新指定的不同，需要创建新的 {} 对象 {} {}", callerSimpleClassName, ConfigureWrapper.class.getSimpleName(), existedDbOperator.getDbConfInfo(),
                        dbConfInfo);
                throw new JavaCG2RuntimeException("当前配置类已有的数据库信息与新指定的不同，需要创建新的 " + ConfigureWrapper.class.getSimpleName() + " 对象");
            }
            if (!existedDbOperator.isClosed()) {
                // 当前数据库操作类未关闭，引用数据库操作类
                existedDbOperator.referenceDbOperator(callerClassNameAndHash);
                return existedDbOperWrapper;
            }
            logger.info("当前数据库操作类已关闭");
        }

        // 当前配置类未创建数据库操作包装对象，进行创建
        DbOperator dbOperator = new DbOperator(dbConfInfo, callerClassNameAndHash);
        DbOperWrapper dbOperWrapper = new DbOperWrapper(configureWrapper, dbOperator);
        // 记录数据库操作对象
        configureWrapper.setDbOperWrapper(dbOperWrapper);
        return dbOperWrapper;
    }

    private static DbOperWrapper genDbOperWrapperNeo4j(ConfigureWrapper configureWrapper) {
        String appName = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_APP_NAME);
        int dbInsertBatchSize = 5000;
        logger.info("使用neo4j时批量插入数量使用 {}", dbInsertBatchSize);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, String.valueOf(dbInsertBatchSize));
        return new Neo4jDbOperWrapper(appName, dbInsertBatchSize);
    }

    private DbInitializer() {
        throw new IllegalStateException("illegal");
    }
}
