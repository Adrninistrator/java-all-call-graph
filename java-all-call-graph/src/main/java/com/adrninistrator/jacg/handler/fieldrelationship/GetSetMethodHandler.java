package com.adrninistrator.jacg.handler.fieldrelationship;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4GetMethod;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SetMethod;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/7/20
 * @description: get、set方法处理类
 */
public class GetSetMethodHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(GetSetMethodHandler.class);

    private final JACGExtendsImplHandler extendsImplHandler;

    public GetSetMethodHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        extendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    public GetSetMethodHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        extendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    /**
     * 检查获取到的get/set方法中的类名，若与指定的类名不同则进行修改
     *
     * @param className
     * @param getSetMethod
     */
    private void checkClassName(String className, BaseWriteDbData4GetSetMethod getSetMethod) {
        if (!className.equals(getSetMethod.getClassName())) {
            // 当前的类名与参数中的类名不同，说明当前方法在超类中
            getSetMethod.setInSuperClass(true);
            getSetMethod.setClassName(className);
            getSetMethod.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
            String methodNameWithArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(getSetMethod.getFullMethod());
            getSetMethod.setFullMethod(JavaCG2ClassMethodUtil.formatFullMethodWithArgTypes(className, methodNameWithArgs));
        }
    }

    /**
     * 根据类名，查询对应的get/set方法，若指定的类中不存在则从超类中查询
     *
     * @param queryGetMethod true: 查询get方法 false: 查询set方法
     * @param className      完整类名
     * @return
     */
    public List<BaseWriteDbData4GetSetMethod> queryGetSetMethodByClassNameSuper(boolean queryGetMethod, String className) {
        List<BaseWriteDbData4GetSetMethod> returnList = new ArrayList<>();
        String currentClassName = className;
        while (true) {
            List<BaseWriteDbData4GetSetMethod> list = queryGetSetMethodByClassName(queryGetMethod, currentClassName);
            if (list != null) {
                for (BaseWriteDbData4GetSetMethod getSetMethod : list) {
                    // 检查获取到的get/set方法中的类名，若与指定的类名不同则进行修改
                    checkClassName(className, getSetMethod);
                    returnList.add(getSetMethod);
                }
            }
            // 当前类未查询到对应的get/set方法，再查询父类的
            String superClassName = extendsImplHandler.querySuperClassNameByFull(currentClassName);
            if (superClassName == null) {
                break;
            }
            currentClassName = superClassName;
        }
        return returnList;
    }

    /**
     * 根据类名，查询对应的get/set方法，使用记录id进行排序
     *
     * @param queryGetMethod true: 查询get方法 false: 查询set方法
     * @param className      完整类名
     * @return
     */
    public List<BaseWriteDbData4GetSetMethod> queryGetSetMethodByClassName(boolean queryGetMethod, String className) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        SqlKeyEnum sqlKeyEnum = queryGetMethod ? SqlKeyEnum.GM_QUERY_BY_CLASS_NAME : SqlKeyEnum.SM_QUERY_BY_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.GET_SET_METHOD_COLUMNS) +
                    " from " + getGetSetMethodTableName(queryGetMethod) +
                    " where " + DC.GSM_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<BaseWriteDbData4GetSetMethod> list = dbOperator.queryList(sql, BaseWriteDbData4GetSetMethod.class, simpleClassName);
        if (list != null) {
            list.sort(Comparator.comparingInt(BaseWriteDbData4GetSetMethod::getRecordId));
            for (BaseWriteDbData4GetSetMethod getSetMethod : list) {
                getSetMethod.setGetOrSet(queryGetMethod);
            }
        }
        return list;
    }

    /**
     * 根据类名与方法名，查询对应的get/set方法，若指定的类中不存在则从超类中查询
     *
     * @param queryGetMethod true: 查询get方法 false: 查询set方法
     * @param className      完整类名
     * @param methodName     get方法名
     * @return
     */
    public BaseWriteDbData4GetSetMethod queryGetSetMethodByClassMethodSuper(boolean queryGetMethod, String className, String methodName) {
        String currentClassName = className;
        while (true) {
            BaseWriteDbData4GetSetMethod getSetMethod = queryGetSetMethodByClassMethod(queryGetMethod, currentClassName, methodName);
            if (getSetMethod != null) {
                // 检查获取到的get/set方法中的类名，若与指定的类名不同则进行修改
                checkClassName(className, getSetMethod);
                return getSetMethod;
            }
            // 当前类未查询到对应的get/set方法，再查询父类的
            String superClassName = extendsImplHandler.querySuperClassNameByFull(currentClassName);
            if (superClassName == null) {
                return null;
            }
            currentClassName = superClassName;
        }
    }

    /**
     * 根据类名与方法名，查询对应的get/set方法
     *
     * @param queryGetMethod true: 查询get方法 false: 查询set方法
     * @param className      完整类名
     * @param methodName     get方法名
     * @return
     */
    public BaseWriteDbData4GetSetMethod queryGetSetMethodByClassMethod(boolean queryGetMethod, String className, String methodName) {
        return queryGetSetMethodBySimpleClassMethod(queryGetMethod, dbOperWrapper.querySimpleClassName(className), methodName);
    }

    /**
     * 根据类名与方法名，查询对应的get/set方法
     *
     * @param queryGetMethod  true: 查询get方法 false: 查询set方法
     * @param simpleClassName 唯一类名
     * @param methodName      get方法名
     * @return
     */
    public BaseWriteDbData4GetSetMethod queryGetSetMethodBySimpleClassMethod(boolean queryGetMethod, String simpleClassName, String methodName) {
        SqlKeyEnum sqlKeyEnum = queryGetMethod ? SqlKeyEnum.GM_QUERY_BY_METHOD_NAME : SqlKeyEnum.SM_QUERY_BY_METHOD_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.GET_SET_METHOD_COLUMNS) +
                    " from " + getGetSetMethodTableName(queryGetMethod) +
                    " where " + DC.GSM_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.GSM_METHOD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        BaseWriteDbData4GetSetMethod getSetMethod = dbOperator.queryObject(sql, BaseWriteDbData4GetSetMethod.class, simpleClassName, methodName);
        if (getSetMethod != null) {
            getSetMethod.setGetOrSet(queryGetMethod);
        }
        return getSetMethod;
    }

    /**
     * 根据类名与字段名，查询对应的get/set方法
     *
     * @param queryGetMethod true: 查询get方法 false: 查询set方法
     * @param className      完整类名
     * @param fieldName      get方法对应的字段名
     * @return
     */
    public BaseWriteDbData4GetSetMethod queryGetSetMethodByFieldName(boolean queryGetMethod, String className, String fieldName) {
        List<BaseWriteDbData4GetSetMethod> list = queryGetSetMethodListByFieldName(queryGetMethod, className, fieldName);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return null;
        }
        if (list.size() > 1) {
            logger.error("类的字段存在多个{}方法 {} {}", (queryGetMethod ? JavaCG2Constants.METHOD_PREFIX_GET : JavaCG2Constants.METHOD_PREFIX_SET), className, fieldName);
        }
        return list.get(0);
    }

    /**
     * 根据类名与字段名，查询对应的get/set方法列表
     *
     * @param queryGetMethod true: 查询get方法 false: 查询set方法
     * @param className      完整类名
     * @param fieldName      get方法对应的字段名
     * @return
     */
    public List<BaseWriteDbData4GetSetMethod> queryGetSetMethodListByFieldName(boolean queryGetMethod, String className, String fieldName) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        SqlKeyEnum sqlKeyEnum = queryGetMethod ? SqlKeyEnum.GM_QUERY_BY_FIELD_NAME : SqlKeyEnum.SM_QUERY_BY_FIELD_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.GET_SET_METHOD_COLUMNS) +
                    " from " + getGetSetMethodTableName(queryGetMethod) +
                    " where " + DC.GSM_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.GSM_FIELD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<BaseWriteDbData4GetSetMethod> list = dbOperator.queryList(sql, BaseWriteDbData4GetSetMethod.class, simpleClassName, fieldName);
        if (list != null) {
            for (BaseWriteDbData4GetSetMethod getSetMethod : list) {
                getSetMethod.setGetOrSet(queryGetMethod);
            }
        }
        return list;
    }

    /**
     * 根据类名与字段名，查询对应的get/set方法，若指定的类中不存在则从超类中查询
     *
     * @param queryGetMethod true: 查询get方法 false: 查询set方法
     * @param className      完整类名
     * @param fieldName      get方法对应的字段名
     * @return
     */
    public BaseWriteDbData4GetSetMethod queryGetSetMethodByFieldNameSuper(boolean queryGetMethod, String className, String fieldName) {
        String currentClassName = className;
        while (true) {
            BaseWriteDbData4GetSetMethod getSetMethod = queryGetSetMethodByFieldName(queryGetMethod, currentClassName, fieldName);
            if (getSetMethod != null) {
                // 检查获取到的get/set方法中的类名，若与指定的类名不同则进行修改
                checkClassName(className, getSetMethod);
                return getSetMethod;
            }
            // 当前类未查询到对应的get/set方法，再查询父类的
            String superClassName = extendsImplHandler.querySuperClassNameByFull(currentClassName);
            if (superClassName == null) {
                return null;
            }
            currentClassName = superClassName;
        }
    }

    /**
     * 根据完整类名与set方法名，查询对应的get方法，若指定的类中不存在则从超类中查询
     *
     * @param className  完整类名
     * @param methodName set方法名
     * @return
     */
    public WriteDbData4GetMethod queryGetMethodBySetMethodSuper(String className, String methodName) {
        String currentClassName = className;
        while (true) {
            WriteDbData4GetMethod getMethod = queryGetMethodBySetMethod(currentClassName, methodName);
            if (getMethod != null) {
                // 检查获取到的get/set方法中的类名，若与指定的类名不同则进行修改
                checkClassName(className, getMethod);
                return getMethod;
            }
            // 当前类未查询到对应的get/set方法，再查询父类的
            String superClassName = extendsImplHandler.querySuperClassNameByFull(currentClassName);
            if (superClassName == null) {
                return null;
            }
            currentClassName = superClassName;
        }
    }

    /**
     * 根据完整类名与set方法名，查询对应的get方法
     *
     * @param className  完整类名
     * @param methodName set方法名
     * @return
     */
    public WriteDbData4GetMethod queryGetMethodBySetMethod(String className, String methodName) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.GM_QUERY_BY_SET_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_GET_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_GET_METHOD.getTableName() +
                    " where " + DC.GSM_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.GSM_FIELD_NAME + " in " +
                    "(" +
                    " select " + DC.GSM_FIELD_NAME +
                    " from " + DbTableInfoEnum.DTIE_SET_METHOD.getTableName() +
                    " where " + DC.GSM_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.GSM_METHOD_NAME + " = ?"
                    + ")";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4GetMethod.class, simpleClassName, simpleClassName, methodName);
    }

    /**
     * 根据完整类名与get方法名，查询对应的set方法
     *
     * @param className  完整类名
     * @param methodName get方法名
     * @return
     */
    public WriteDbData4SetMethod querySetMethodByGetMethod(String className, String methodName) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SM_QUERY_BY_GET_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_SET_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_SET_METHOD.getTableName() +
                    " where " + DC.GSM_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.GSM_FIELD_NAME + " in " +
                    "(" +
                    " select " + DC.GSM_FIELD_NAME +
                    " from " + DbTableInfoEnum.DTIE_GET_METHOD.getTableName() +
                    " where " + DC.GSM_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.GSM_METHOD_NAME + " = ?"
                    + ")";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4SetMethod.class, simpleClassName, simpleClassName, methodName);
    }

    /**
     * 查询使用了指定类型字段对应的类名
     *
     * @param queryGetMethod true: 查询get方法 false: 查询set方法
     * @param fieldType      字段类型
     * @return
     */
    public List<String> queryClassesByFieldType(boolean queryGetMethod, String fieldType) {
        SqlKeyEnum sqlKeyEnum = queryGetMethod ? SqlKeyEnum.GM_QUERY_BY_FIELD_TYPE : SqlKeyEnum.SM_QUERY_BY_FIELD_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + DC.GSM_CLASS_NAME +
                    " from " + getGetSetMethodTableName(queryGetMethod) +
                    " where " + DC.GSM_SIMPLE_FIELD_TYPE_NAD + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, dbOperWrapper.querySimpleClassName(fieldType));
    }

    private String getGetSetMethodTableName(boolean getMethod) {
        return getMethod ? DbTableInfoEnum.DTIE_GET_METHOD.getTableName() : DbTableInfoEnum.DTIE_SET_METHOD.getTableName();
    }
}
