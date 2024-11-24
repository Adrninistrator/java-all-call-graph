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
 * @description: 类的继承或实现的泛型信息查询处理类
 */
public class ClassExtImplGenericsTypeHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(ClassExtImplGenericsTypeHandler.class);

    private final JACGExtendsImplHandler jacgExtendsImplHandler;

    public ClassExtImplGenericsTypeHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        this.jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    public ClassExtImplGenericsTypeHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    /**
     * 查询指定类在继承父类/实现接口时泛型类型中的类名列表，使用相关类的完整类名
     *
     * @param className      指定类的唯一类名
     * @param upperClassName 上层父类/接口的完整类名
     * @return
     */
    public List<String> queryClassExtImplGenericsTypeListByFull(String className, String upperClassName) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        return queryClassExtImplGenericsTypeListBySimple(simpleClassName, upperClassName);
    }

    /**
     * 查询指定类在继承父类/实现接口时泛型类型中的类名列表，使用相关类的唯一类名
     *
     * @param simpleClassName 指定类的唯一类名
     * @param upperClassName  上层父类/接口的完整类名
     * @return
     */
    public List<String> queryClassExtImplGenericsTypeListBySimple(String simpleClassName, String upperClassName) {
        // 查询指定类在继承父类/实现接口时存在泛型类型时的父类/接口类名
        List<String> superOrItfClassNameList = querySuperOrItfClassNameList(simpleClassName);
        if (JavaCG2Util.isCollectionEmpty(superOrItfClassNameList)) {
            logger.info("指定类在继承父类/实现接口时不存在泛型类型信息 {}", simpleClassName);
            return Collections.emptyList();
        }

        for (String superOrItfClassName : superOrItfClassNameList) {
            logger.debug("找到指定类对应的父类/接口 {} {}", simpleClassName, superOrItfClassName);
            if (superOrItfClassName.equals(upperClassName) || jacgExtendsImplHandler.checkExtendsOrImplFull(upperClassName, superOrItfClassName)) {
                /*
                    当前的继承父类/实现接口与指定的相同，或指定的类是当前的继承父类/实现接口的父类/接口
                    查询指定类在继承指定父类/实现指定接口时对应泛型类型的类名列表
                 */
                List<String> genericsTypeList = queryGenericsTypeList(simpleClassName, superOrItfClassName);
                if (JavaCG2Util.isCollectionEmpty(superOrItfClassNameList)) {
                    logger.info("未查询到指定类在继承指定父类/实现指定接口时对应泛型类型的类名列表 {} {}", simpleClassName, superOrItfClassName);
                }
                return genericsTypeList;
            }
        }
        return Collections.emptyList();
    }

    /**
     * 查询指定类在继承父类/实现接口时存在泛型类型时的父类/接口类名
     *
     * @param simpleClassName
     * @return
     */
    private List<String> querySuperOrItfClassNameList(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CEIGT_QUERY_SUPER_INTERFACE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.CEIGT_SUPER_ITF_CLASS_NAME + ")" +
                    " from " + DbTableInfoEnum.DTIE_CLASS_EXT_IMPL_GENERICS_TYPE.getTableName() +
                    " where " + DC.CEIGT_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryListOneColumn(sql, String.class, simpleClassName);
    }

    /**
     * 查询指定类在继承指定父类/实现指定接口时对应泛型类型的类名列表
     *
     * @param simpleClassName
     * @param superOrItfClassName
     * @return
     */
    private List<String> queryGenericsTypeList(String simpleClassName, String superOrItfClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CEIGT_QUERY_GENERICS_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CEIGT_GENERICS_TYPE +
                    " from " + DbTableInfoEnum.DTIE_CLASS_EXT_IMPL_GENERICS_TYPE.getTableName() +
                    " where " + DC.CEIGT_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.CEIGT_SUPER_ITF_CLASS_NAME + " = ?" +
                    " and " + DC.CEIGT_GENERICS_TYPE + " != ''" +
                    " order by " + DC.CEIGT_SEQ;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryListOneColumn(sql, String.class, simpleClassName, superOrItfClassName);
    }
}
