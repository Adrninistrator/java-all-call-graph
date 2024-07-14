package com.adrninistrator.jacg.handler.method;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.dto.stack.ListAsStack;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description: 方法处理类
 */
public class MethodInfoHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(MethodInfoHandler.class);

    private final JACGExtendsImplHandler jacgExtendsImplHandler;

    public MethodInfoHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    public MethodInfoHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    /**
     * 根据完整类名与方法代码行号查询对应的完整方法
     *
     * @param className
     * @param lineNumber
     * @return null: 未查询到
     */
    public String queryFullMethodByClassLine(String className, int lineNumber) {
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MLN_QUERY_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MLN_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_LINE_NUMBER.getTableName() +
                    " where " + DC.MLN_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MLN_MIN_LINE_NUMBER + " <= ?" +
                    " and " + DC.MLN_MAX_LINE_NUMBER + " >= ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, simpleClassName, lineNumber, lineNumber);
    }

    /**
     * 根据完整类名与方法名查询方法信息
     *
     * @param className
     * @param methodName
     * @return
     */
    public List<WriteDbData4MethodInfo> queryMethodInfoByClassMethod(String className, String methodName) {
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        return queryMethodInfoBySimpleClassMethod(simpleClassName, methodName);
    }

    /**
     * 根据唯一类名与方法名查询方法信息
     *
     * @param simpleClassName
     * @param methodName
     * @return
     */
    public List<WriteDbData4MethodInfo> queryMethodInfoBySimpleClassMethod(String simpleClassName, String methodName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_ALL_BY_CLASS_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = " select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MI_METHOD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodInfo.class, simpleClassName, methodName);
    }

    /**
     * 根据类名与方法名，查找对应的完整方法，假如当前类没有查找到，则从实现的接口中查找
     *
     * @param className
     * @param methodName
     * @return 包含字段： fullMethod returnType
     */
    public List<WriteDbData4MethodInfo> queryMethodInfoByCMSuperInterface(String className, String methodName) {
        // 当前需要处理的类名列表
        ListAsStack<String> currentClassNameStack = new ListAsStack<>();
        // 已经处理过的类名集合
        Set<String> handledClassNameSet = new HashSet<>();
        List<WriteDbData4MethodInfo> allMethodInfoList = new ArrayList<>();
        List<String> allFullMethodList = new ArrayList<>();
        currentClassNameStack.push(className);
        while (!currentClassNameStack.isEmpty()) {
            String currentClassName = currentClassNameStack.pop();
            handledClassNameSet.add(currentClassName);

            // 根据类名与方法名，查找对应的完整方法，假如当前类没有查找到，则从实现的接口中查找，执行查询操作
            doQueryMethodInfoByCMSuperInterface(className, currentClassName, methodName, allMethodInfoList, allFullMethodList);

            // 查询当前处理的Mapper接口的父接口
            List<String> interfaceNameList = jacgExtendsImplHandler.queryImplInterfaceNameByClassName(currentClassName);
            if (JavaCGUtil.isCollectionEmpty(interfaceNameList)) {
                continue;
            }
            for (String interfaceName : interfaceNameList) {
                if (!handledClassNameSet.contains(interfaceName)) {
                    currentClassNameStack.push(interfaceName);
                }
            }
        }
        logger.info("根据类名与方法名，查找对应的完整方法 {} {} {}", className, methodName, StringUtils.join(allFullMethodList, "\n"));
        return allMethodInfoList;
    }

    /**
     * 根据完整方法查询方法信息
     *
     * @param fullMethod
     * @return
     */
    public WriteDbData4MethodInfo queryMethodInfoByFullMethod(String fullMethod) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_ALL_BY_FULL_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = " select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_METHOD_HASH + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4MethodInfo.class, JACGUtil.genHashWithLen(fullMethod));
    }

    // 根据类名与方法名，查找对应的完整方法，假如当前类没有查找到，则从实现的接口中查找，执行查询操作
    private void doQueryMethodInfoByCMSuperInterface(String className, String currentClassName, String methodName, List<WriteDbData4MethodInfo> allMethodInfoList,
                                                     List<String> allFullMethodList) {
        // 使用当前处理的Mapper接口类名查询对应的方法信息
        String currentSimpleClassName = dbOperWrapper.getSimpleClassName(currentClassName);
        List<WriteDbData4MethodInfo> methodInfoList = queryMethodInfoBySimpleClassMethod(currentSimpleClassName, methodName);
        if (JavaCGUtil.isCollectionEmpty(methodInfoList)) {
            return;
        }
        for (WriteDbData4MethodInfo methodInfo : methodInfoList) {
            WriteDbData4MethodInfo returnWriteDbData4MethodInfo = new WriteDbData4MethodInfo();
            if (className.equals(currentClassName)) {
                // 当前查询的类与参数指定的相同，直接使用查询到的完整方法
                returnWriteDbData4MethodInfo.setFullMethod(methodInfo.getFullMethod());
            } else {
                // 当前查询的类与参数指定的不同，使用替换了类名后的完整方法
                String methodNameWithArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(methodInfo.getFullMethod());
                returnWriteDbData4MethodInfo.setFullMethod(JavaCGClassMethodUtil.formatFullMethodWithArgTypes(className, methodNameWithArgs));
            }
            returnWriteDbData4MethodInfo.setReturnType(methodInfo.getReturnType());
            allMethodInfoList.add(returnWriteDbData4MethodInfo);
            allFullMethodList.add(returnWriteDbData4MethodInfo.getFullMethod());
        }
    }

    /**
     * 根据完整方法HASH+长度，获取方法对应的标志
     *
     * @param methodHash
     * @return
     */
    public Integer getMethodFlags(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_FLAGS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MI_ACCESS_FLAGS +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_METHOD_HASH + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryObjectOneColumn(sql, Integer.class, methodHash);
    }

    /**
     * 通过类名与方法名查询完整方法
     *
     * @param className
     * @param methodName
     * @return
     */
    public List<String> getMethodByClassMethod(String className, String methodName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_FULL_METHOD_BY_CLASS_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = " select " + DC.MI_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MI_METHOD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, dbOperWrapper.getSimpleClassName(className), methodName);
    }
}
