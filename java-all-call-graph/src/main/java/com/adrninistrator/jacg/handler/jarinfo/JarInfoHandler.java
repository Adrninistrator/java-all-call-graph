package com.adrninistrator.jacg.handler.jarinfo;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/9/20
 * @description: 对解析的jar包及目录信息处理的类
 */
public class JarInfoHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(JarInfoHandler.class);

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
        return dbOperator.queryObjectOneColumn(sql, String.class, JavaCG2Constants.FILE_KEY_RESULT_DIR);
    }

    /**
     * 从jar文件信息表查询指定类型的数据
     *
     * @param type
     * @return
     */
    public List<WriteDbData4JarInfo> queryJarInfoByType(String type) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.JI_QUERY_JAR_LIST;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_JAR_INFO) +
                    " from " + DbTableInfoEnum.DTIE_JAR_INFO.getTableName() +
                    " where " + DC.JI_JAR_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4JarInfo.class, type);
    }

    /**
     * 查询最终解析的jar文件信息
     *
     * @return
     */
    public WriteDbData4JarInfo queryParseJarInfo() {
        List<WriteDbData4JarInfo> jarInfoList = queryJarInfoByType(JavaCG2Constants.FILE_KEY_PARSE_JAR_PATH);
        if (jarInfoList.size() != 1) {
            logger.error("查询最终解析的jar文件数量非法 {}", jarInfoList.size());
            return null;
        }
        return jarInfoList.get(0);
    }

    /**
     * 查询fat jar文件信息
     *
     * @return
     */
    public WriteDbData4JarInfo queryFatJarInfo() {
        List<WriteDbData4JarInfo> jarInfoList = queryJarInfoByType(JavaCG2Constants.FILE_KEY_FAT_JAR_PATH);
        if (jarInfoList.size() != 1) {
            logger.error("查询fat jar文件数量非法 {}", jarInfoList.size());
            return null;
        }
        return jarInfoList.get(0);
    }
}
