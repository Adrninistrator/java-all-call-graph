package com.adrninistrator.jacg.dboper;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.util.JACGUtil;
import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.beans.PropertyVetoException;
import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class DbOperator {

    private static final Logger logger = LoggerFactory.getLogger(DbOperator.class);

    private static volatile DbOperator instance;

    private ComboPooledDataSource cpds;

    private boolean useH2Db = false;

//    private ConfInfo confInfo = null;

    public static DbOperator getInstance() {
        if (instance == null) {
            synchronized (DbOperator.class) {
                if (instance == null) {
                    instance = new DbOperator();
                }
            }
        }
        return instance;
    }

    public boolean init(ConfInfo confInfo) {
        try {
//            this.confInfo = confInfo;
//            Class.forName(confInfo.getDbDriverName());

            cpds = new ComboPooledDataSource();
            cpds.setMaxPoolSize(confInfo.getThreadNum());
            cpds.setTestConnectionOnCheckin(false);
            cpds.setTestConnectionOnCheckout(false);

            if (confInfo.isDbUseH2()) {
                initH2Db(confInfo);
            } else {
                initNonH2Db(confInfo);
            }

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    private void initH2Db(ConfInfo confInfo) throws PropertyVetoException {
        useH2Db = true;

        cpds.setDriverClass("org.h2.Driver");
        String h2DbJdbcUrl = JACGConstants.H2_PROTOCOL + confInfo.getDbH2FilePath() +
                ";MODE=MySQL;DATABASE_TO_LOWER=TRUE;CASE_INSENSITIVE_IDENTIFIERS=TRUE;INIT=CREATE SCHEMA IF NOT EXISTS " +
                JACGConstants.H2_SCHEMA + "\\;SET SCHEMA " + JACGConstants.H2_SCHEMA;
        logger.info("H2数据库的JDBC URL: {}", h2DbJdbcUrl);

        cpds.setJdbcUrl(h2DbJdbcUrl);
        cpds.setUser("");
        cpds.setPassword("");
    }

    private void initNonH2Db(ConfInfo confInfo) throws PropertyVetoException {
        useH2Db = false;

        cpds.setDriverClass(confInfo.getDbDriverName());
        cpds.setJdbcUrl(confInfo.getDbUrl());
        cpds.setUser(confInfo.getDbUsername());
        cpds.setPassword(confInfo.getDbPassword());
    }

    public void setMaxPoolSize(int maxPoolSize) {
        cpds.setMaxPoolSize(maxPoolSize);
    }

    public Connection getConnection() {
        synchronized (DbOperator.class) {
            try {
//            return DriverManager.getConnection(confInfo.getDbUrl(), confInfo.getDbUsername(), confInfo.getDbPassword());
                return cpds.getConnection();
            } catch (Exception e) {
                logger.error("getConnection error ", e);
                return null;
            }
        }
    }

    public void closeDs() {
        if (cpds != null) {
            cpds.close();
        }
    }

    private void close(Connection connection, PreparedStatement stmt, boolean closeConnection) {
        try {
            if (stmt != null) {
                stmt.close();
            }
            if (closeConnection && connection != null) {
                // 使用数据源，只是将连接释放回连接池，不会断开与数据库的连接
                connection.close();
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    private void close(Connection connection, PreparedStatement stmt) {
        close(connection, stmt, true);
    }

    public void closeConnection(Connection connection) {
        try {
            if (connection != null) {
                // 使用数据源，只是将连接释放回连接池，不会断开与数据库的连接
                connection.close();
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    private void closeResultSet(ResultSet rs) {
        try {
            if (rs != null) {
                rs.close();
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    public boolean createTable(String sql) {
        if (!executeSql(sql)) {
            return false;
        }

        int indexStart = sql.indexOf(JACGConstants.SQL_CREATE_TABLE_HEAD);
        if (indexStart == -1) {
            logger.error("建表SQL语句中未找到指定内容 {} {}", sql, JACGConstants.SQL_CREATE_TABLE_HEAD);
            return false;
        }

        int indexEnd = sql.indexOf('(');
        if (indexEnd == -1) {
            logger.error("建表SQL语句中未找到\")\" {}", sql);
            return false;
        }

        String tableName = sql.substring(indexStart + JACGConstants.SQL_CREATE_TABLE_HEAD_LENGTH, indexEnd).trim();

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

        logger.info("数据库表创建成功 [{}]", tableName);
        return true;
    }

    private boolean checkTableExistsH2(String tableName) {
        List<Object> list = queryListOneColumn("SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES where TABLE_SCHEMA = ? and TABLE_NAME = ?",
                new Object[]{JACGConstants.H2_SCHEMA, tableName});
        if (JACGUtil.isCollectionEmpty(list)) {
            logger.error("数据库表创建失败 [{}]", tableName);
            return false;
        }
        return true;
    }

    private boolean checkTableExistsNonH2(String tableName) {
        List<Object> list = queryListOneColumn("show tables like ?", new Object[]{tableName});
        if (JACGUtil.isCollectionEmpty(list)) {
            logger.error("数据库表创建失败 [{}]", tableName);
            return false;
        }
        return true;
    }

    public boolean truncateTable(String tableName) {
        String sql = "truncate table " + tableName;
        logger.info("truncate table sql: [{}]", sql);
        return executeSql(sql);
    }

    public boolean executeSql(String sql) {
        Connection connection = null;
        PreparedStatement stmt = null;

        try {
            connection = getConnection();
            if (connection == null) {
                return false;
            }

            stmt = connection.prepareStatement(sql);
            stmt.execute();
            return true;
        } catch (Exception e) {
            logger.error("error [{}] ", sql, e);
            return false;
        } finally {
            close(connection, stmt);
        }
    }

    public Integer update(Connection connection, boolean closeConnection, String sql, Object[] arguments) {
        if (connection == null) {
            return null;
        }

        PreparedStatement stmt = null;

        try {
            stmt = connection.prepareStatement(sql);
            setArguments(stmt, arguments);
            int row = stmt.executeUpdate();
            return Integer.valueOf(row);
        } catch (Exception e) {
            logger.error("error [{}] ", sql, e);
            return null;
        } finally {
            close(connection, stmt, closeConnection);
        }
    }

    public Integer update(String sql, Object[] arguments) {
        Connection connection = getConnection();
        if (connection == null) {
            return null;
        }
        return update(connection, true, sql, arguments);
    }

    public boolean batchInsert(String sql, List<Object[]> argumentList) {
        Connection connection = null;
        PreparedStatement stmt = null;

        try {
            connection = getConnection();
            if (connection == null) {
                return false;
            }

            connection.setAutoCommit(false);

            stmt = connection.prepareStatement(sql);

            int columnNum = argumentList.get(0).length;

            for (Object[] argument : argumentList) {
                for (int i = 0; i < columnNum; i++) {
                    stmt.setObject(i + 1, argument[i]);
                }
                stmt.addBatch();
            }

            stmt.executeBatch();
            stmt.clearBatch();
            connection.commit();
            return true;
        } catch (Exception e) {
            if (e.getCause() instanceof SQLSyntaxErrorException) {
                logger.error("请检查数据库表是否需要使用最新版本创建，是否需要执行 com.adrninistrator.jacg.unzip.UnzipFile 释放最新的SQL语句\n[{}] ", sql, e);
            } else {
                logger.error("error [{}] ", sql, e);
            }
            return false;
        } finally {
            close(connection, stmt);
        }
    }

    /**
     * 查询列表，仅包含一个字段
     *
     * @param connection
     * @param closeConnection
     * @param sql
     * @param arguments
     * @return
     */
    public List<Object> queryListOneColumn(Connection connection, boolean closeConnection, String sql, Object[] arguments) {
        if (connection == null) {
            return null;
        }

        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            stmt = connection.prepareStatement(sql);
            setArguments(stmt, arguments);

            rs = stmt.executeQuery();

            List<Object> list = new ArrayList<>();
            while (rs.next()) {
                list.add(rs.getObject(1));
            }
            return list;
        } catch (Exception e) {
            logger.error("error [{}] [{}] ", sql, StringUtils.join(arguments, " "), e);
            return null;
        } finally {
            close(connection, stmt, closeConnection);
            closeResultSet(rs);
        }
    }

    /**
     * 查询列表，仅包含一个字段
     *
     * @param sql
     * @param arguments
     * @return
     */
    public List<Object> queryListOneColumn(String sql, Object[] arguments) {
        Connection connection = getConnection();
        if (connection == null) {
            return null;
        }
        return queryListOneColumn(connection, true, sql, arguments);
    }

    /**
     * 查询列表，包含多个字段
     *
     * @param connection
     * @param closeConnection
     * @param sql
     * @param arguments
     * @return
     */
    public List<Map<String, Object>> queryList(Connection connection, boolean closeConnection, String sql, Object[] arguments) {
        if (connection == null) {
            return null;
        }
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            stmt = connection.prepareStatement(sql);
            setArguments(stmt, arguments);
            rs = stmt.executeQuery();

            ResultSetMetaData meta = rs.getMetaData();
            int columnCount = meta.getColumnCount();
            List<Map<String, Object>> list = new ArrayList<>();

            while (rs.next()) {
                Map<String, Object> map = new HashMap<>(columnCount);
                for (int i = 1; i <= columnCount; i++) {
                    /*
                        当查询SQL通过AS指定字段别名时，使用getColumnLabel可以获取到别名，未指定别名时，可获取到原始字段名
                        使用getColumnName只能获取到原始字段名
                     */
                    map.put(meta.getColumnLabel(i), rs.getObject(i));
                }
                list.add(map);
            }
            return list;
        } catch (Exception e) {
            logger.error("error [{}] [{}] ", sql, StringUtils.join(arguments, " "), e);
            return null;
        } finally {
            close(connection, stmt, closeConnection);
            closeResultSet(rs);
        }
    }

    public List<Map<String, Object>> queryList(String sql, Object[] arguments) {
        Connection connection = getConnection();
        if (connection == null) {
            return null;
        }
        return queryList(connection, true, sql, arguments);
    }

    /**
     * 查询一行记录
     *
     * @param sql
     * @param arguments
     * @return
     */
    public Map<String, Object> queryOneRow(String sql, Object[] arguments) {
        Connection connection = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            connection = getConnection();
            if (connection == null) {
                return null;
            }

            stmt = connection.prepareStatement(sql);
            setArguments(stmt, arguments);
            rs = stmt.executeQuery();

            ResultSetMetaData meta = rs.getMetaData();
            int columnCount = meta.getColumnCount();

            Map<String, Object> map = new HashMap<>(columnCount);
            if (rs.next()) {
                for (int i = 1; i <= columnCount; i++) {
                    /*
                        当查询SQL通过AS指定字段别名时，使用getColumnLabel可以获取到别名，未指定别名时，可获取到原始字段名
                        使用getColumnName只能获取到原始字段名
                     */
                    map.put(meta.getColumnLabel(i), rs.getObject(i));
                }
            }
            return map;
        } catch (Exception e) {
            logger.error("error [{}] [{}] ", sql, StringUtils.join(arguments, " "), e);
            return null;
        } finally {
            close(connection, stmt);
            closeResultSet(rs);
        }
    }

    private void setArguments(PreparedStatement stmt, Object[] arguments) throws SQLException {
        if (arguments != null) {
            int argumentNum = arguments.length;
            for (int i = 0; i < argumentNum; i++) {
                stmt.setObject(i + 1, arguments[i]);
            }
        }
    }
}
