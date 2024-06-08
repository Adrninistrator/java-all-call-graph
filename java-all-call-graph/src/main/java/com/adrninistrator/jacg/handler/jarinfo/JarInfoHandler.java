package com.adrninistrator.jacg.handler.jarinfo;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/9/20
 * @description: 对解析的jar包及目录信息处理的类
 */
public class JarInfoHandler extends BaseHandler {
    public JarInfoHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public JarInfoHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 查询所有的jar包和目录信息
     *
     * @return
     */
    public List<WriteDbData4JarInfo> queryAllJarInfo() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.JI_QUERY_JAR_INFO;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_JAR_INFO) +
                    " from " + DbTableInfoEnum.DTIE_JAR_INFO.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4JarInfo.class);
    }

    /**
     * 查询当前解析的jar包信息的解析文件输出目录
     *
     * @return
     */
    public String queryOutputDirPath() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.JI_QUERY_OUTPUT_DIR_PATH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.JI_JAR_FULL_PATH +
                    " from " + DbTableInfoEnum.DTIE_JAR_INFO.getTableName() +
                    " where " + DC.JI_JAR_TYPE + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, JavaCGConstants.FILE_KEY_RESULT_DIR_INFO_PREFIX);
    }
}
