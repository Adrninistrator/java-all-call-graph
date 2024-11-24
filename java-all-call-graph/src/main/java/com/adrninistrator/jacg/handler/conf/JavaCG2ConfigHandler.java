package com.adrninistrator.jacg.handler.conf;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConfigKeyEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2024/11/17
 * @description: 查询java-callgraph2组件使用的配置参数的类
 */
public class JavaCG2ConfigHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(JavaCG2ConfigHandler.class);

    public JavaCG2ConfigHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public JavaCG2ConfigHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 根据配置文件名与配置参数名查询对应的配置参数值
     *
     * @param configFileName
     * @param configKey
     * @return
     */
    public String queryConfigValue(String configFileName, String configKey) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.JAVACG2C_QUERY_VALUE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.JAVACG2C_CONFIG_VALUE +
                    " from " + DbTableInfoEnum.DTIE_JAVACG2_CONFIG.getTableName() +
                    " where " + DC.JAVACG2C_CONFIG_FILE_NAME + " = ?"
                    + " and " + DC.JAVACG2C_CONFIG_KEY + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, configFileName, configKey);
    }

    /**
     * 判断使用 java-callgraph2 组件处理方法调用时是否解析被调用对象和参数可能的类型与值
     *
     * @return true: 解析 false: 不解析
     */
    public boolean checkParseMethodCallTypeValue() {
        String parseMethodCallTypeValue = queryConfigValue(JavaCG2Constants.FILE_PATH_CONFIG, JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getKey());
        if (Boolean.parseBoolean(parseMethodCallTypeValue)) {
            logger.info("使用 java-callgraph2 组件处理方法调用时有解析被调用对象和参数可能的类型与值");
            return true;
        }
        logger.warn("使用 java-callgraph2 组件处理方法调用时未解析被调用对象和参数可能的类型与值");
        return false;
    }
}
