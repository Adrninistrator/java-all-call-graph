package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldRelationship;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringTask;
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
                data.getCallType(),
                data.getCalleeObjType(),
                data.getEnabled(),
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
                data.getCallFlags(),
                data.getRawReturnType(),
                data.getActualReturnType(),
                data.getCallerJarNum(),
                data.getCalleeJarNum()
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
                data.getFullMethod()
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

    private JACGSqlUtil() {
        throw new IllegalStateException("illegal");
    }
}
