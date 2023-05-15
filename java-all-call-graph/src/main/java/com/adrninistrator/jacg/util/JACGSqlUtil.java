package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description:
 */

public class JACGSqlUtil {

    /**
     * 生成指定数量的问号，使用括号包含
     *
     * @param num 问号数量
     * @return
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
     * 替换sql语句中的appName
     *
     * @param sql
     * @param appName
     * @return
     */
    public static String replaceAppNameInSql(String sql, String appName) {
        return sql.replace(JACGConstants.APP_NAME_IN_SQL, appName);
    }

    private JACGSqlUtil() {
        throw new IllegalStateException("illegal");
    }
}
