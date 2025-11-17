package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldRelationship;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAdvice;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAspect;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopPointcut;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringTask;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.alibaba.druid.sql.ast.SQLStatement;
import com.alibaba.druid.sql.ast.statement.SQLColumnDefinition;
import com.alibaba.druid.sql.ast.statement.SQLSelectOrderByItem;
import com.alibaba.druid.sql.ast.statement.SQLTableElement;
import com.alibaba.druid.sql.dialect.mysql.ast.MySqlPrimaryKey;
import com.alibaba.druid.sql.dialect.mysql.ast.MySqlUnique;
import com.alibaba.druid.sql.dialect.mysql.ast.statement.MySqlCreateTableStatement;
import com.alibaba.druid.sql.dialect.mysql.ast.statement.MySqlTableIndex;
import com.alibaba.druid.sql.dialect.mysql.parser.MySqlStatementParser;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description:
 */

public class JACGSqlUtil {

    private static final Logger logger = LoggerFactory.getLogger(JACGSqlUtil.class);

    /**
     * 生成指定数量的问号，使用括号包含
     *
     * @param num 问号数量
     * @return (?, ...)
     */
    public static String genQuestionString(int num) {
        if (num < 1) {
            return null;
        }

        StringBuilder stringBuilder = new StringBuilder("(");
        for (int i = 0; i < num; i++) {
            if (i != 0) {
                stringBuilder.append(", ");
            }
            stringBuilder.append("?");
        }
        stringBuilder.append(")");
        return stringBuilder.toString();
    }

    /**
     * 拼接数据库字段，使用括号包含
     *
     * @param columns 字段名
     * @return
     */
    public static String genColumnString(String[] columns) {
        int num = columns.length;
        StringBuilder stringBuilder = new StringBuilder("(");
        for (int i = 0; i < num; i++) {
            if (i != 0) {
                stringBuilder.append(", ");
            }
            stringBuilder.append(columns[i]);
        }
        stringBuilder.append(")");
        return stringBuilder.toString();
    }

    /**
     * 通过数据库驱动类名判断是否使用MySQL
     *
     * @param driverClassName
     * @return
     */
    public static boolean isMySQLDb(String driverClassName) {
        return StringUtils.contains(driverClassName, JACGConstants.MYSQL_FLAG);
    }

    /**
     * 拼接查询结果中的字段
     *
     * @param columns
     * @return
     */
    public static String joinColumns(String... columns) {
        return StringUtils.join(columns, JACGConstants.FLAG_COMMA_WITH_SPACE);
    }

    /**
     * 获取指定数据库表的全部字段
     *
     * @param dbTableInfoEnum
     * @return
     */
    public static String getTableAllColumns(DbTableInfoEnum dbTableInfoEnum) {
        return joinColumns(dbTableInfoEnum.getColumns());
    }

    /**
     * 替换建表sql语句中的appName及表名后缀
     *
     * @param sql
     * @param appName
     * @return
     */
    public static String replaceFlagInSql(String sql, String appName, String tableSuffix) {
        String appNameAndTableSuffix = appName;
        if (StringUtils.isNotBlank(tableSuffix)) {
            // 指定的表名后缀非空，拼接到appName后面
            appNameAndTableSuffix += tableSuffix;
        }
        return sql.replace(JACGConstants.REPLACE_SQL_FLAG_APP_NAME, appNameAndTableSuffix);
    }

    /**
     * 生成方法调用关系对象对应的数组
     *
     * @param data
     * @return
     */
    public static Object[] genMethodCallObjectArray(WriteDbData4MethodCall data) {
        return new Object[]{
                data.getCallId(),
                data.getEnabled(),
                data.getCallType(),
                data.getCallerMethodHash(),
                data.getCallerSimpleClassName(),
                data.getCallerMethodName(),
                data.getCallerFullMethod(),
                data.getCallerLineNumber(),
                data.getCallerReturnType(),
                data.getCalleeMethodHash(),
                data.getCalleeSimpleClassName(),
                data.getCalleeMethodName(),
                data.getCalleeFullMethod(),
                data.getCalleeArrayDimensions(),
                data.getCalleeObjType(),
                data.getRawReturnType(),
                data.getActualReturnType(),
                data.getCallFlags(),
                data.getCallerJarNum(),
                data.getCalleeJarNum(),
                data.getDescription()
        };
    }

    /**
     * 生成Spring Task对象对应的数组
     *
     * @param data
     * @return
     */
    public static Object[] genWriteDbData4SpringTask(WriteDbData4SpringTask data) {
        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getSpringBeanName(),
                data.getClassName(),
                data.getMethodName(),
                data.getType(),
                data.getFullMethod(),
                data.getReturnType(),
                data.getDefineClassNameXmlPath()
        };
    }

    /**
     * 生成通过get/set方法关联的字段关系表对象对应的数组
     *
     * @param data
     * @return
     */
    public static Object[] genFieldRelationshipObjectArray(WriteDbData4FieldRelationship data) {
        return new Object[]{
                data.getFldRelationshipId(),
                data.getGetMethodCallId(),
                data.getSetMethodCallId(),
                data.getCallerFullMethod(),
                data.getCallerLineNumber(),
                data.getGetSimpleClassName(),
                data.getGetMethodName(),
                data.getGetClassName(),
                data.getSetSimpleClassName(),
                data.getSetMethodName(),
                data.getSetClassName(),
                data.getValid(),
                data.getType(),
                data.getRelationshipFlags(),
                data.getBeanUtilCallId(),
                data.getBeanUtilMethod()
        };
    }

    public static Object[] genSpringAopAspectArray(WriteDbData4SpringAopAspect data) {
        return new Object[]{
                data.getRecordId(),
                data.getType(),
                data.getXmlAspectId(),
                data.getXmlAspectRef(),
                data.getAspectOrder(),
                data.getClassName(),
                data.getDefineXmlPath()
        };
    }

    public static Object[] genSpringAopPointcutArray(WriteDbData4SpringAopPointcut data) {
        return new Object[]{
                data.getRecordId(),
                data.getType(),
                data.getXmlPointcutId(),
                data.getExpression(),
                data.getFullMethod(),
                data.getDefineXmlPath()
        };
    }

    public static Object[] genSpringAopAdviceArray(WriteDbData4SpringAopAdvice data) {
        return new Object[]{
                data.getRecordId(),
                data.getType(),
                data.getXmlAspectId(),
                data.getXmlAspectMethodName(),
                data.getAdviceType(),
                data.getXmlPointcutRef(),
                data.getExpression(),
                data.getAspectOrder(),
                data.getAdviceFullMethod(),
                data.getAdviceMethodReturnType(),
                data.getAdviceMethodHash(),
                data.getAspectClassName(),
                data.getDefineXmlPath()
        };
    }

    // 对建表sql语句进行转换
    public static String transformCreateTableSql(String sql, boolean delNumberInIndex, String appName, String tableSuffix) {
        String newSql = JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
        String trimSql = sql.trim();
        if (delNumberInIndex && StringUtils.startsWithAny(trimSql, "PRIMARY KEY", "INDEX", "UNIQUE INDEX")) {
            // 使用H2数据库，且以PRIMARY KEY、INDEX、UNIQUE INDEX开头，去掉索引中指定的字段长度
            return newSql.replaceAll("\\([0-9]+\\)", "");
        }

        // 其他情况，每行都替换appName
        return newSql;
    }

    // 转换为 postgresql 格式的建表语句
    public static String transformCreateTableSqlToPg(String sql) {
        MySqlStatementParser parser = new MySqlStatementParser(sql);
        SQLStatement statement = parser.parseStatement();
        if (!(statement instanceof MySqlCreateTableStatement)) {
            logger.error("语句类型不符合预期 {} {}", statement.getClass().getName(), sql);
            throw new JavaCG2RuntimeException("语句类型不符合预期");
        }
        MySqlCreateTableStatement mySqlCreateTableStatement = (MySqlCreateTableStatement) statement;
        String newSql = convertMysqlToPg(mySqlCreateTableStatement);
        logger.debug("postgresql 使用的建表 sql: {}", newSql);
        return newSql;
    }


    /**
     * 将MySQL建表语句转换为PostgreSQL格式
     */
    public static String convertMysqlToPg(MySqlCreateTableStatement mysqlStmt) {
        List<String> sqlParts = new ArrayList<>();

        // 1. 表名和基础CREATE TABLE语句
        String tableName = mysqlStmt.getName().getSimpleName();
        sqlParts.add(JACGConstants.SQL_CREATE_TABLE_HEAD + " " + tableName + " (");

        // 2. 处理字段定义
        List<SQLTableElement> tableElementList = mysqlStmt.getTableElementList();
        List<String> columnDefinitions = new ArrayList<>();
        List<String> primaryKeys = new ArrayList<>();
        List<String> uniqueConstraints = new ArrayList<>();
        List<String> indexes = new ArrayList<>();

        for (SQLTableElement element : tableElementList) {
            if (element instanceof SQLColumnDefinition) {
                // 处理字段定义
                SQLColumnDefinition column = (SQLColumnDefinition) element;
                String columnSql = buildColumnDefinition(column);
                columnDefinitions.add(columnSql);
            } else if (element instanceof MySqlPrimaryKey) {
                // 处理主键
                MySqlPrimaryKey primaryKey = (MySqlPrimaryKey) element;
                primaryKeys.addAll(getIndexColumns(primaryKey.getColumns()));
            } else if (element instanceof MySqlUnique) {
                // 处理唯一约束
                MySqlUnique unique = (MySqlUnique) element;
                String constraintName = unique.getName() != null ? unique.getName().getSimpleName() : "";
                List<String> columns = getIndexColumns(unique.getColumns());
                if (!columns.isEmpty()) {
                    if (!StringUtils.isEmpty(constraintName)) {
                        uniqueConstraints.add("CONSTRAINT " + constraintName + " UNIQUE (" + String.join(", ", columns) + ")");
                    } else {
                        uniqueConstraints.add("UNIQUE (" + String.join(", ", columns) + ")");
                    }
                }
            } else if (element instanceof MySqlTableIndex) {
                // 处理普通索引
                MySqlTableIndex index = (MySqlTableIndex) element;
                String indexName = index.getName() != null ? index.getName().getSimpleName() : "";
                List<String> columns = getIndexColumns(index.getColumns());
                if (!columns.isEmpty() && !"PRIMARY".equals(indexName)) {
                    indexes.add("INDEX " + indexName + " (" + String.join(", ", columns) + ")");
                }
            }
        }

        // 3. 合并所有表元素定义
        List<String> allElements = new ArrayList<>(columnDefinitions);

        // 添加主键约束
        if (!primaryKeys.isEmpty()) {
            allElements.add("PRIMARY KEY (" + String.join(", ", primaryKeys) + ")");
        }

        // 添加唯一约束
        allElements.addAll(uniqueConstraints);

        // 添加索引（PostgreSQL中索引是单独的语句，但为了兼容性可以放在这里）
        // allElements.addAll(indexes); // 注意：PostgreSQL的索引需要单独的CREATE INDEX语句

        // 4. 处理最后一个逗号
        if (!allElements.isEmpty()) {
            for (int i = 0; i < allElements.size(); i++) {
                if (i == allElements.size() - 1) {
                    // 最后一个元素去掉末尾的逗号
                    String lastElement = allElements.get(i);
                    if (lastElement.endsWith(",")) {
                        sqlParts.add("    " + lastElement.substring(0, lastElement.length() - 1));
                    } else {
                        sqlParts.add("    " + lastElement);
                    }
                } else {
                    sqlParts.add("    " + allElements.get(i) + ",");
                }
            }
        }

        sqlParts.add(");");

        // 5. 添加表注释
        if (mysqlStmt.getComment() != null) {
            String tableComment = formatComment(mysqlStmt.getComment().toString());
            sqlParts.add("COMMENT ON TABLE " + tableName + " IS " + tableComment + ";");
        }

        // 6. 添加字段注释
        for (SQLTableElement element : tableElementList) {
            if (element instanceof SQLColumnDefinition) {
                SQLColumnDefinition column = (SQLColumnDefinition) element;
                if (column.getComment() != null) {
                    String columnName = column.getColumnName();
                    String columnComment = formatComment(column.getComment().toString());
                    sqlParts.add("COMMENT ON COLUMN " + tableName + "." + columnName + " IS " + columnComment + ";");
                }
            }
        }

        // 7. 添加索引语句（PostgreSQL需要单独的CREATE INDEX语句）
        for (SQLTableElement element : tableElementList) {
            if (element instanceof MySqlTableIndex) {
                MySqlTableIndex index = (MySqlTableIndex) element;
                String indexName = index.getName() != null ? index.getName().getSimpleName() : "";
                List<String> columns = getIndexColumns(index.getColumns());
                if (!columns.isEmpty() && !"PRIMARY".equals(indexName)) {
                    String indexSql = "CREATE INDEX IF NOT EXISTS " + indexName + " ON " + tableName + " (" + String.join(", ", columns) + ");";
                    sqlParts.add(indexSql);
                }
            }
        }

        // 8. 拼接最终SQL
        return String.join("\n", sqlParts);
    }

    /**
     * 构建字段定义SQL
     */
    private static String buildColumnDefinition(SQLColumnDefinition column) {
        StringBuilder sb = new StringBuilder();

        // 字段名
        String columnName = column.getColumnName();
        sb.append(columnName).append(" ");

        // 字段类型转换
        String mysqlType = column.getDataType().getName().toLowerCase();
        String pgType = convertDataType(mysqlType);
        sb.append(pgType);

        // NOT NULL约束
        if (column.containsNotNullConstaint()) {
            sb.append(" NOT NULL");
        }

        // 默认值
        if (column.getDefaultExpr() != null) {
            sb.append(" DEFAULT ").append(column.getDefaultExpr());
        }

        // 字段注释（PostgreSQL中字段注释是单独的语句，这里只构建字段定义）
        // 注释会在后续单独处理

        return sb.toString();
    }

    /**
     * MySQL数据类型到PostgreSQL数据类型的转换
     */
    private static String convertDataType(String mysqlType) {
        switch (mysqlType.toLowerCase()) {
            case "boolean":
                return "BOOLEAN";
            case "smallint":
            case "smallint(5)":
            case "tinyint":
            case "tinyint(1)":
                return "SMALLINT";
            case "mediumint":
            case "int":
            case "integer":
                return "INTEGER";
            case "bigint":
                return "BIGINT";
            case "decimal":
            case "numeric":
                return "NUMERIC";
            case "float":
                return "REAL";
            case "double":
                return "DOUBLE PRECISION";
            case "char":
                return "CHAR";
            case "varchar":
                return "VARCHAR";
            case "text":
            case "tinytext":
            case "mediumtext":
            case "longtext":
                return "TEXT";
            case "date":
                return "DATE";
            case "time":
                return "TIME";
            case "datetime":
            case "timestamp":
                return "TIMESTAMP";
            case "blob":
            case "tinyblob":
            case "mediumblob":
            case "longblob":
                return "BYTEA";
            case "json":
                return "JSON";
            default:
                return mysqlType.toUpperCase();
        }
    }

    /**
     * 获取索引字段列表
     */
    private static List<String> getIndexColumns(List<SQLSelectOrderByItem> columns) {
        List<String> columnNames = new ArrayList<>();
        for (SQLSelectOrderByItem column : columns) {
            columnNames.add(column.getExpr().toString());
        }
        return columnNames;
    }

    /**
     * 格式化注释文本
     */
    private static String formatComment(String comment) {
        if (comment == null) {
            return "NULL";
        }

        String cleanedComment = comment.trim();

        // 如果注释已经被单引号包含，直接返回
        if (cleanedComment.startsWith("'") && cleanedComment.endsWith("'")) {
            return cleanedComment;
        }

        // 否则用单引号包含
        return "'" + cleanedComment.replace("'", "''") + "'";
    }

    private JACGSqlUtil() {
        throw new IllegalStateException("illegal");
    }
}
