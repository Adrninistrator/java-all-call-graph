package com.adrninistrator.jacg.dboper;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.DbConfInfo;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.IncorrectResultSizeDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.RowMapperResultSetExtractor;
import org.springframework.lang.Nullable;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/4/25
 * @description: 继承JdbcTemplate，查询结果为空时不抛出异常
 */
public class JdbcTemplateQuiet extends JdbcTemplate {

    private static final Logger logger = LoggerFactory.getLogger("jacg_slow_query");

    private final boolean slowQuerySwitch;
    private final int slowQueryTime;
    private final int slowQueryRowNum;

    public JdbcTemplateQuiet(DataSource dataSource, DbConfInfo dbConfInfo) {
        super(dataSource);
        slowQuerySwitch = dbConfInfo.isSlowQuerySwitch();
        slowQueryTime = dbConfInfo.getSlowQueryTime();
        slowQueryRowNum = dbConfInfo.getSlowQueryRowNum();
    }

    @Override
    public <T> List<T> queryForList(String sql, Class<T> elementType, @Nullable Object... args) throws DataAccessException {
        long startTime = System.currentTimeMillis();
        List<T> results = super.queryForList(sql, elementType, args);
        printSlowQuery(startTime, results, sql, args);
        return results;
    }

    @Override
    public <T> List<T> query(String sql, RowMapper<T> rowMapper, @Nullable Object... args) throws DataAccessException {
        long startTime = System.currentTimeMillis();
        List<T> results = super.query(sql, rowMapper, args);
        printSlowQuery(startTime, results, sql, args);
        return results;
    }

    @Override
    public <T> T queryForObject(String sql, Class<T> requiredType, @Nullable Object... args) throws DataAccessException {
        long startTime = System.currentTimeMillis();
        List<T> results = super.query(sql, newArgPreparedStatementSetter(args), new RowMapperResultSetExtractor<>(getSingleColumnRowMapper(requiredType), 1));
        printSlowQuery(startTime, results, sql, args);
        return getSingleResult(results);
    }

    @Override
    @Nullable
    public <T> T queryForObject(String sql, RowMapper<T> rowMapper, @Nullable Object... args) throws DataAccessException {
        long startTime = System.currentTimeMillis();
        List<T> results = super.query(sql, newArgPreparedStatementSetter(args), new RowMapperResultSetExtractor<>(rowMapper, 1));
        printSlowQuery(startTime, results, sql, args);
        return getSingleResult(results);
    }

    /**
     * 查询列表，返回List中每个元素为Map，支持指定查询超时时间
     * 超时时间单位为秒，使用 JDBC 的 PreparedStatement.setQueryTimeout(int seconds) 实现
     * （JDBC 规范仅提供秒级的查询超时设置，不支持毫秒，详见详细方案文档“为什么只能用秒”一节）
     *
     * @param sql          SQL语句
     * @param queryTimeout 查询超时时间（秒），0表示不超时
     * @param args         参数
     * @return 查询结果
     * @throws DataAccessException 查询异常
     */
    public List<Map<String, Object>> queryForListWithTimeout(String sql, int queryTimeout, @Nullable Object... args) throws DataAccessException {
        long startTime = System.currentTimeMillis();
        List<Map<String, Object>> results = execute((Connection conn) -> {
            try (PreparedStatement ps = conn.prepareStatement(sql)) {
                if (queryTimeout > 0) {
                    // JDBC的setQueryTimeout仅支持秒，无法做到毫秒级精度，因此超时时间单位使用秒
                    ps.setQueryTimeout(queryTimeout);
                }
                if (args != null && args.length > 0) {
                    for (int i = 0; i < args.length; i++) {
                        ps.setObject(i + 1, args[i]);
                    }
                }
                try (ResultSet rs = ps.executeQuery()) {
                    List<Map<String, Object>> rows = new LinkedList<>();
                    ResultSetMetaData rsmd = rs.getMetaData();
                    int columnCount = rsmd.getColumnCount();
                    while (rs.next()) {
                        Map<String, Object> row = new LinkedHashMap<>(columnCount);
                        for (int i = 1; i <= columnCount; i++) {
                            row.put(rsmd.getColumnLabel(i), rs.getObject(i));
                        }
                        rows.add(row);
                    }
                    return rows;
                }
            }
        });
        printSlowQuery(startTime, results, sql, args);
        return results;
    }

    private <T> void printSlowQuery(long startTime, List<T> results, String sql, Object... args) {
        if (!slowQuerySwitch) {
            return;
        }
        long spendTime = System.currentTimeMillis() - startTime;
        if (spendTime < slowQueryTime && results.size() < slowQueryRowNum) {
            return;
        }
        String formattedSql = sql;
        for (Object arg : args) {
            formattedSql = StringUtils.replaceOnce(formattedSql, "?", genArgStr(arg));
        }

        logger.warn("出现慢查询 sql [{}] 参数 [{}] 耗时 [{}] ms 查询结果数量 [{}] 格式化后的sql [{}]", sql, StringUtils.join(args, JACGConstants.FLAG_COMMA_WITH_SPACE), spendTime, results.size(),
                formattedSql);
    }

    private String genArgStr(Object arg) {
        if (arg instanceof String) {
            return "'" + arg + "'";
        }
        return String.valueOf(arg);
    }

    private <T> T getSingleResult(List<T> results) {
        if (JavaCG2Util.isCollectionEmpty(results)) {
            return null;
        }
        if (results.size() > 1) {
            throw new IncorrectResultSizeDataAccessException(1, results.size());
        }
        return results.get(0);
    }
}
