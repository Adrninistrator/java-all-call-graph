package com.adrninistrator.jacg.handler.method;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.classes.ClassNameAndType;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.stack.ListAsStack;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
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
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
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
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        return queryMethodInfoBySimpleClassMethod(simpleClassName, methodName);
    }

    /**
     * 根据完整类名与方法名查询方法信息，若在当前类中未查询到，则从父类/接口中查询
     *
     * @param className
     * @param methodName
     * @return
     */
    public List<WriteDbData4MethodInfo> queryMethodInfoByClassMethodSuperInterface(String className, String methodName) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        List<WriteDbData4MethodInfo> methodInfoList = queryMethodInfoBySimpleClassMethod(simpleClassName, methodName);
        if (!JavaCG2Util.isCollectionEmpty(methodInfoList)) {
            // 从当前类中查询到，返回
            return methodInfoList;
        }

        // 从当前类中未查询到，从父类/接口中查询
        List<ClassNameAndType> superClassNameAndTypeList = jacgExtendsImplHandler.queryAllSuperClassesAndInterfaces(className);
        if (JavaCG2Util.isCollectionEmpty(superClassNameAndTypeList)) {
            return null;
        }

        for (ClassNameAndType superClassNameAndType : superClassNameAndTypeList) {
            String superSimpleClassName = dbOperWrapper.querySimpleClassName(superClassNameAndType.getClassName());
            methodInfoList = queryMethodInfoBySimpleClassMethod(superSimpleClassName, methodName);
            if (!JavaCG2Util.isCollectionEmpty(methodInfoList)) {
                return methodInfoList;
            }
        }
        return null;
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
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MI_METHOD_NAME + " = ?" +
                    " order by " + DC.MI_RECORD_ID;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodInfo.class, simpleClassName, methodName);
    }

    /**
     * 根据类名查询方法信息
     *
     * @param className
     * @return
     */
    public List<WriteDbData4MethodInfo> queryMethodInfoByClass(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_ALL_BY_CLASS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodInfo.class, dbOperWrapper.querySimpleClassName(className));
    }

    /**
     * 根据类名与方法名，查找对应的完整方法，假如当前类没有查找到，则从实现的接口中查找
     *
     * @param className
     * @param methodName
     * @return 包含字段： fullMethod returnType
     */
    public List<WriteDbData4MethodInfo> queryMethodByClassMethodUpper(String className, String methodName) {
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
            doQueryMethodInfoByClassMethodUpper(className, currentClassName, methodName, allMethodInfoList, allFullMethodList);

            // 查询当前处理的类的接口
            List<String> interfaceNameList = jacgExtendsImplHandler.queryImplInterfaceNameByClassName(currentClassName);
            if (!JavaCG2Util.isCollectionEmpty(interfaceNameList)) {
                for (String interfaceName : interfaceNameList) {
                    if (!handledClassNameSet.contains(interfaceName)) {
                        currentClassNameStack.push(interfaceName);
                    }
                }
            }

            // 查询当前处理的类的接口
            String superName = jacgExtendsImplHandler.querySuperClassNameByFull(currentClassName);
            if (superName != null) {
                currentClassNameStack.push(superName);
            }
        }
        if (logger.isDebugEnabled()) {
            logger.debug("根据类名与方法名，查找对应的完整方法 {} {} {}", className, methodName, StringUtils.join(allFullMethodList, "\n"));
        }
        return allMethodInfoList;
    }

    /**
     * 根据完整方法及返回类型查询方法信息
     *
     * @param fullMethod
     * @param returnType
     * @return
     */
    public WriteDbData4MethodInfo queryMethodInfoByFullMethod(String fullMethod, String returnType) {
        return queryMethodInfoByMethodHash(JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType));
    }

    /**
     * 根据方法HASH+长度查询方法信息
     *
     * @param methodHash
     * @return
     */
    public WriteDbData4MethodInfo queryMethodInfoByMethodHash(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_ALL_BY_METHOD_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_METHOD_HASH + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4MethodInfo.class, methodHash);
    }

    /**
     * 根据完整方法与返回类型判断方法是否存在，若在当前类中未查找到，则在父类与接口中查找
     *
     * @param fullMethod 完整方法
     * @param returnType 返回类型
     * @return
     */
    public boolean checkExistsMethodByFullMethodSuperInterface(String fullMethod, String returnType) {
        WriteDbData4MethodInfo methodInfo = queryMethodByFullMethodSuperInterface(fullMethod, returnType, null);
        return methodInfo != null;
    }

    /**
     * 根据完整方法与返回类型查询方法，若在当前类中未查找到，则在父类与接口中查找
     *
     * @param fullMethod         完整方法
     * @param returnType         返回类型
     * @param superClassNameList 用于记录本次查询到的父类与接口类名，可为null
     * @return
     */
    public WriteDbData4MethodInfo queryMethodByFullMethodSuperInterface(String fullMethod, String returnType, List<String> superClassNameList) {
        // 在当前类查询方法
        WriteDbData4MethodInfo methodInfo = queryMethodInfoByFullMethod(fullMethod, returnType);
        if (methodInfo != null) {
            // 指定的方法在当前类中存在
            return methodInfo;
        }
        // 指定的方法在当前类中不存在，从超类及实现的接口中查找
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        // 根据类名向上查询对应的父类、实现的接口信息
        List<ClassNameAndType> tmpSuperClassNameAndTypeList = jacgExtendsImplHandler.queryAllSuperClassesAndInterfaces(className);
        if (JavaCG2Util.isCollectionEmpty(tmpSuperClassNameAndTypeList)) {
            return null;
        }
        if (superClassNameList != null) {
            // 当参数指定的父类/接口列表非空时，记录本次查询到的父类/接口
            for (ClassNameAndType superClassNameAndType : tmpSuperClassNameAndTypeList) {
                if (!superClassNameList.contains(superClassNameAndType.getClassName())) {
                    superClassNameList.add(superClassNameAndType.getClassName());
                }
            }
        }
        String methodNameWithArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(fullMethod);
        for (ClassNameAndType superClassNameAndType : tmpSuperClassNameAndTypeList) {
            String superFullMethod = JavaCG2ClassMethodUtil.formatFullMethodWithArgTypes(superClassNameAndType.getClassName(), methodNameWithArgs);
            WriteDbData4MethodInfo superMethodInfo = queryMethodInfoByFullMethod(superFullMethod, returnType);
            if (superMethodInfo != null) {
                // 在当前类的超类或实现的接口中找到了对应的方法，返回存在
                return superMethodInfo;
            }
        }
        return null;
    }

    // 根据类名与方法名，查找对应的完整方法，假如当前类没有查找到，则从实现的接口中查找，执行查询操作
    private void doQueryMethodInfoByClassMethodUpper(String className, String currentClassName, String methodName, List<WriteDbData4MethodInfo> allMethodInfoList,
                                                     List<String> allFullMethodList) {
        // 使用当前处理的类名查询对应的方法信息
        String currentSimpleClassName = dbOperWrapper.querySimpleClassName(currentClassName);
        List<WriteDbData4MethodInfo> methodInfoList = queryMethodInfoBySimpleClassMethod(currentSimpleClassName, methodName);
        if (JavaCG2Util.isCollectionEmpty(methodInfoList)) {
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
                returnWriteDbData4MethodInfo.setFullMethod(JavaCG2ClassMethodUtil.formatFullMethodWithArgTypes(className, methodNameWithArgs));
            }
            returnWriteDbData4MethodInfo.setReturnType(methodInfo.getReturnType());
            returnWriteDbData4MethodInfo.setMethodHash(JACGClassMethodUtil.genMethodHashWithLen(returnWriteDbData4MethodInfo.getFullMethod(),
                    returnWriteDbData4MethodInfo.getReturnType()));
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
    public Integer queryMethodFlags(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_FLAGS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MI_ACCESS_FLAGS +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_METHOD_HASH + " = ?" +
                    " limit 1";
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
    public List<FullMethodWithReturnType> queryMethodByClassMethod(String className, String methodName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_FULL_METHOD_BY_CLASS_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MI_FULL_METHOD, DC.MI_RETURN_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MI_METHOD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<WriteDbData4MethodInfo> list = dbOperator.queryList(sql, WriteDbData4MethodInfo.class, dbOperWrapper.querySimpleClassName(className), methodName);
        return dbOperWrapper.genFullMethodWithReturnTypeList(list);
    }

    /**
     * 根据类名或包名前缀查询相关的方法
     *
     * @param classNamePrefix
     * @return
     */
    public List<String> queryMethodByClassNamePrefix(String classNamePrefix) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_FULL_METHOD_BY_CN_PREFIX;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MI_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_CLASS_NAME + " like concat(?, '%')";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, classNamePrefix);
    }

    /**
     * 根据jar文件序号与类名查询重复同名类方法信息
     *
     * @param jarNum
     * @param className
     * @return
     */
    public List<WriteDbData4MethodInfo> queryDupMethodInfoByClass(int jarNum, String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.DMI_QUERY_ALL_BY_JAR_CLASS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_DUP_METHOD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_DUP_METHOD_INFO.getTableName() +
                    " where " + DC.MI_JAR_NUM + " = ?" +
                    " and " + DC.MI_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodInfo.class, jarNum, dbOperWrapper.querySimpleClassName(className));
    }

    /**
     * 查询方法信息，支持完整方法，或完整方法+方法返回类型
     *
     * @param fullMethodSupportReturnType
     * @return
     */
    public WriteDbData4MethodInfo queryMethodInfoSupportReturnType(String fullMethodSupportReturnType) {
        String[] array = StringUtils.splitPreserveAllTokens(fullMethodSupportReturnType, JavaCG2Constants.FLAG_COLON);
        if (array.length != 2 && array.length != 3) {
            logger.error("查询的方法非法 {} 使用 {} 拆分后的数量为 {}", fullMethodSupportReturnType, JavaCG2Constants.FLAG_COLON, array.length);
            return null;
        }
        if (array.length == 2) {
            // 指定的是完整方法
            String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethodSupportReturnType);
            String methodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(fullMethodSupportReturnType);
            List<WriteDbData4MethodInfo> methodInfoList = queryMethodInfoByClassMethod(className, methodName);
            if (JavaCG2Util.isCollectionEmpty(methodInfoList)) {
                logger.error("未查询到相同名称的方法 {}", fullMethodSupportReturnType);
                return null;
            }
            StringBuilder stringBuilder = new StringBuilder();
            List<WriteDbData4MethodInfo> matchedMethodInfoList = new ArrayList<>();
            for (WriteDbData4MethodInfo methodInfo : methodInfoList) {
                if (fullMethodSupportReturnType.equals(methodInfo.getFullMethod())) {
                    matchedMethodInfoList.add(methodInfo);
                    if (stringBuilder.length() > 0) {
                        stringBuilder.append(" ");
                    }
                    stringBuilder.append(methodInfo.getFullMethod()).append(JavaCG2Constants.CONFIG_LIST);
                }
            }
            if (JavaCG2Util.isCollectionEmpty(matchedMethodInfoList)) {
                logger.error("未查询到指定的方法 {}", fullMethodSupportReturnType);
                return null;
            }
            if (matchedMethodInfoList.size() > 1) {
                logger.error("查询到多个完整方法相同的方法，假如需要唯一确定，需要使用{完整方法}:{方法返回类型}的形式 {}", stringBuilder);
            }
            return matchedMethodInfoList.get(0);
        }

        // 指定的是完整方法+方法返回类型
        WriteDbData4MethodInfo methodInfo = queryMethodInfoByFullMethod(array[0], array[1]);
        if (methodInfo == null) {
            logger.error("通过完整方法+方法返回类型未查询到对应方法信息 {}", fullMethodSupportReturnType);
            return null;
        }
        return methodInfo;
    }
}
