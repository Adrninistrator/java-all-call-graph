package com.adrninistrator.jacg.handler.method;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.javacg.util.JavaCGUtil;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/4/13
 * @description: 根据dto查询相关的完整方法，包括方法参数、方法参数泛型、方法返回类型、方法返回泛型类型中的dto
 */
public class FindMethodByDtoHandler extends BaseHandler {
    public FindMethodByDtoHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public FindMethodByDtoHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 查询参数中有使用指定类型的完整方法
     *
     * @param argType 指定的参数类型
     * @return
     */
    public Set<String> findMethodByArgType(String argType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MAT_QUERY_BY_ARG_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.MAT_FULL_METHOD + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ARG_TYPE.getTableName() +
                    " where " + DC.MAT_SIMPLE_ARG_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        String simpleArgType = dbOperWrapper.getSimpleClassName(argType);
        List<String> list = dbOperator.queryListOneColumn(sql, String.class, simpleArgType);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptySet();
        }
        return new HashSet<>(list);
    }

    /**
     * 查询参数泛型中有使用指定类型的完整方法
     *
     * @param argType 指定的参数类型
     * @return
     */
    public Set<String> findMethodByArgGenericsType(String argType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MAGT_QUERY_BY_ARG_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.MAGT_FULL_METHOD + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ARG_GENERICS_TYPE.getTableName() +
                    " where " + DC.MAGT_SIMPLE_GENERICS_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        String simpleArgType = dbOperWrapper.getSimpleClassName(argType);
        List<String> list = dbOperator.queryListOneColumn(sql, String.class, simpleArgType);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptySet();
        }
        return new HashSet<>(list);
    }

    /**
     * 查询返回指定类型的完整方法
     *
     * @param returnType 指定的返回类型
     * @return
     */
    public Set<String> findMethodByReturnType(String returnType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_BY_RETURN_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.MI_FULL_METHOD + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_SIMPLE_RETURN_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        String simpleArgType = dbOperWrapper.getSimpleClassName(returnType);
        List<String> list = dbOperator.queryListOneColumn(sql, String.class, simpleArgType);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptySet();
        }
        return new HashSet<>(list);
    }

    /**
     * 查询返回的泛型包含指定类型的完整方法
     *
     * @param returnType 指定的返回类型
     * @return
     */
    public Set<String> findMethodByReturnGenericsType(String returnType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MRGT_QUERY_BY_RETURN_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.MRGT_FULL_METHOD + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_RETURN_GENERICS_TYPE.getTableName() +
                    " where " + DC.MRGT_SIMPLE_GENERICS_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        String simpleArgType = dbOperWrapper.getSimpleClassName(returnType);
        List<String> list = dbOperator.queryListOneColumn(sql, String.class, simpleArgType);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptySet();
        }
        return new HashSet<>(list);
    }

    /**
     * 查询方法参数、方法参数泛型、方法返回类型、方法返回泛型类型中包含指定dto类型的完整方法
     *
     * @param dtoType 指定的dto类型
     * @return
     */
    public Set<String> findMethodByAllType(String dtoType) {
        Set<String> fullMethodSet = new HashSet<>();
        // 查询参数中有使用指定类型的完整方法
        fullMethodSet.addAll(findMethodByArgType(dtoType));
        // 查询参数泛型中有使用指定类型的完整方法
        fullMethodSet.addAll(findMethodByArgGenericsType(dtoType));
        // 查询返回指定类型的完整方法
        fullMethodSet.addAll(findMethodByReturnType(dtoType));
        // 查询返回的泛型包含指定类型的完整方法
        fullMethodSet.addAll(findMethodByReturnGenericsType(dtoType));
        return fullMethodSet;
    }
}
