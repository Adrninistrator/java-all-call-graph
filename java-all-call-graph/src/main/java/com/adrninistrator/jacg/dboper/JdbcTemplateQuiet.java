package com.adrninistrator.jacg.dboper;

import com.adrninistrator.javacg.util.JavaCGUtil;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.IncorrectResultSizeDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.RowMapperResultSetExtractor;
import org.springframework.lang.Nullable;

import javax.sql.DataSource;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/25
 * @description: 继承JdbcTemplate，查询结果为空时不抛出异常
 */
public class JdbcTemplateQuiet extends JdbcTemplate {
    public JdbcTemplateQuiet(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    public <T> T queryForObject(String sql, Class<T> requiredType, @Nullable Object... args) throws DataAccessException {
        List<T> results = query(sql, args, new RowMapperResultSetExtractor<>(getSingleColumnRowMapper(requiredType), 1));
        return getSingleResult(results);
    }

    @Override
    @Nullable
    public <T> T queryForObject(String sql, RowMapper<T> rowMapper, @Nullable Object... args) throws DataAccessException {
        List<T> results = query(sql, args, new RowMapperResultSetExtractor<>(rowMapper, 1));
        return getSingleResult(results);
    }

    private <T> T getSingleResult(List<T> results) {
        if (JavaCGUtil.isCollectionEmpty(results)) {
            return null;
        }
        if (results.size() > 1) {
            throw new IncorrectResultSizeDataAccessException(1, results.size());
        }
        return results.get(0);
    }
}
