package com.adrninistrator.jacg.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description:
 */

public class SqlUtil {
    private static final Logger logger = LoggerFactory.getLogger(SqlUtil.class);

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
     * 生成字段名及对应别名，查询时使用
     *
     * @param columns 字段名数组
     * @param alias   别名数组
     * @return
     */
    public static String genColumnAlias(String[] columns, String[] alias) {
        if (columns == null || alias == null || columns.length == 0 || alias.length == 0 || columns.length != alias.length) {
            logger.error("指定的参数非法 {} {}", columns, alias);
            return null;
        }

        StringBuilder stringBuilder = new StringBuilder();

        int length = columns.length;
        for (int i = 0; i < length; i++) {
            if (i > 0) {
                stringBuilder.append(", ");
            }
            stringBuilder.append(columns[i]).append(" as ").append(alias[i]);
        }
        return stringBuilder.toString();
    }

    private SqlUtil() {
        throw new IllegalStateException("illegal");
    }
}
