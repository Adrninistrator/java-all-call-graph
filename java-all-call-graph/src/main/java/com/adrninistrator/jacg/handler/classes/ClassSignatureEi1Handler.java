package com.adrninistrator.jacg.handler.classes;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description: 类的签名中涉及继承与实现的信息1查询处理类
 */
public class ClassSignatureEi1Handler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(ClassSignatureEi1Handler.class);

    private final JACGExtendsImplHandler jacgExtendsImplHandler;

    public ClassSignatureEi1Handler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        this.jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    public ClassSignatureEi1Handler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    /**
     * 查询指定类在继承父类/实现接口时签名中的类名列表，使用相关类的完整类名
     *
     * @param className      指定类的唯一类名
     * @param upperClassName 上层父类/接口的完整类名
     * @return
     */
    public List<String> queryClassSignatureEi1InfoFull(String className, String upperClassName) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        return queryClassSignatureEi1InfoSimple(simpleClassName, upperClassName);
    }

    /**
     * 查询指定类在继承父类/实现接口时签名中的类名列表，使用相关类的唯一类名
     *
     * @param simpleClassName 指定类的唯一类名
     * @param upperClassName  上层父类/接口的完整类名
     * @return
     */
    public List<String> queryClassSignatureEi1InfoSimple(String simpleClassName, String upperClassName) {
        // 查询指定类存在签名时对应的继承父类/实现接口
        List<String> superOrItfClassNameList = querySuperOrItfClassNameList(simpleClassName);
        if (JavaCG2Util.isCollectionEmpty(superOrItfClassNameList)) {
            logger.error("指定类不存在对应的签名信息 {}", simpleClassName);
            return Collections.emptyList();
        }

        for (String superOrItfClassName : superOrItfClassNameList) {
            logger.debug("找到指定类对应的父类/接口 {} {}", simpleClassName, superOrItfClassName);
            if (superOrItfClassName.equals(upperClassName) || jacgExtendsImplHandler.checkExtendsOrImplFull(upperClassName, superOrItfClassName)) {
                /*
                    当前的继承父类/实现接口与指定的相同，或指定的类是当前的继承父类/实现接口的父类/接口
                    查询指定类，及指定父类/接口对应的签名中的类名列表
                 */
                List<String> signatureClassList = queryClassSignatureClassNameList(simpleClassName, superOrItfClassName);
                if (JavaCG2Util.isCollectionEmpty(superOrItfClassNameList)) {
                    logger.error("指定类不存在对应父类/接口的签名信息 {} {}", simpleClassName, superOrItfClassName);
                }
                return signatureClassList;
            }
        }
        return Collections.emptyList();
    }

    /**
     * 查询指定类存在签名时对应的继承父类/实现接口
     *
     * @param simpleClassName
     * @return
     */
    private List<String> querySuperOrItfClassNameList(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CSEI1_QUERY_SUPER_INTERFACE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.CSEI1_SUPER_ITF_CLASS_NAME + ")" +
                    " from " + DbTableInfoEnum.DTIE_CLASS_SIGNATURE_EI1.getTableName() +
                    " where " + DC.CSEI1_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryListOneColumn(sql, String.class, simpleClassName);
    }

    /**
     * 查询指定类，及指定父类/接口对应的签名中的类名列表
     *
     * @param simpleClassName
     * @param superOrItfClassName
     * @return
     */
    private List<String> queryClassSignatureClassNameList(String simpleClassName, String superOrItfClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CSEI1_QUERY_SIGNATURE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CSEI1_SIGN_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_CLASS_SIGNATURE_EI1.getTableName() +
                    " where " + DC.CSEI1_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.CSEI1_SUPER_ITF_CLASS_NAME + " = ?" +
                    " order by " + DC.CSEI1_SEQ;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryListOneColumn(sql, String.class, simpleClassName, superOrItfClassName);
    }
}
