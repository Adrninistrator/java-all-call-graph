package com.adrninistrator.jacg.handler.mybatis;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.annotation.StringAnnotationAttribute;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSSelectColumn;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSSetColumn;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSTable;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSWriteTable;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MybatisMSEntity;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MybatisMSGetSetDb;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.dto.genericstype.GenericsTypeValue;
import com.adrninistrator.jacg.handler.dto.genericstype.MethodArgGenericsTypeInfo;
import com.adrninistrator.jacg.handler.dto.mybatis.MyBatisMSTableInfo;
import com.adrninistrator.jacg.handler.dto.mybatis.mapper.AbstractMyBatisMapperArg;
import com.adrninistrator.jacg.handler.dto.mybatis.mapper.MyBatisMSMapperParamDbInfo;
import com.adrninistrator.jacg.handler.dto.mybatis.mapper.MyBatisMapperArgAndParamDbInfo;
import com.adrninistrator.jacg.handler.dto.mybatis.mapper.MyBatisMapperArgBasic;
import com.adrninistrator.jacg.handler.dto.mybatis.mapper.MyBatisMapperArgNotSupport;
import com.adrninistrator.jacg.handler.dto.mybatis.mapper.MyBatisMapperArgObject;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import com.adrninistrator.mybatismysqltableparser.common.enums.MySqlStatementEnum;
import com.adrninistrator.mybatismysqltableparser.util.MyBatisTableParserUtil;
import com.alibaba.druid.sql.ast.expr.SQLBinaryOperator;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/15
 * @description: 对MyBatis Mapper、Entity的处理类（使用MySQL）
 */
public class MyBatisMSMapperEntityHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisMSMapperEntityHandler.class);

    private final AnnotationHandler annotationHandler;
    private final MethodArgReturnHandler methodArgReturnHandler;
    private final ClassInfoHandler classInfoHandler;

    public MyBatisMSMapperEntityHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
    }

    public MyBatisMSMapperEntityHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
    }

    /**
     * 获取MyBatis Mapper对应的数据库表信息
     *
     * @param mapperClassName
     * @param mapperMethodName
     * @return
     */
    public MyBatisMSTableInfo queryMyBatisMySqlTableInfo(String mapperClassName, String mapperMethodName) {
        String mapperSimpleClassName = dbOperWrapper.querySimpleClassName(mapperClassName);

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMT_QUERY_TABLE_BY_MAPPER;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MMT_SQL_STATEMENT, DC.MMT_TABLE_NAME) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_TABLE.getTableName() +
                    " where " + DC.MMT_MAPPER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MMT_MAPPER_METHOD_NAME + " = ?" +
                    " order by " + JACGSqlUtil.joinColumns(DC.MMT_SQL_STATEMENT, DC.MMT_TABLE_SEQ);
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<WriteDbData4MyBatisMSTable> list = dbOperator.queryList(sql, WriteDbData4MyBatisMSTable.class, mapperSimpleClassName, mapperMethodName);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            logger.error("未查询到MyBatis Mapper对应的数据库表信息 {} {}", mapperClassName, mapperMethodName);
            return null;
        }

        MyBatisMSTableInfo myBatisMySqlTableInfo = new MyBatisMSTableInfo();
        Map<String, List<String>> tableNameMap = new HashMap<>();
        for (WriteDbData4MyBatisMSTable writeDbData4MyBatisMSTable : list) {
            List<String> tableNameList = tableNameMap.computeIfAbsent(writeDbData4MyBatisMSTable.getSqlStatement(), k -> new ArrayList<>());
            tableNameList.add(writeDbData4MyBatisMSTable.getTableName());
        }

        for (Map.Entry<String, List<String>> entry : tableNameMap.entrySet()) {
            String sqlStatement = entry.getKey();
            List<String> tableNameList = entry.getValue();
            if (MySqlStatementEnum.DSSE_SELECT.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setSelectTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_SELECT_4_UPDATE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setSelect4UpdateTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_INSERT.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setInsertTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_INSERT_IGNORE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setInsertIgnoreTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_INSERT_OR_UPDATE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setInsertOrUpdateTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_REPLACE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setReplaceTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_UPDATE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setUpdateTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_DELETE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setDeleteTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_ALTER.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setAlterTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_TRUNCATE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setTruncateTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_CREATE.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setCreateTableList(tableNameList);
            } else if (MySqlStatementEnum.DSSE_DROP.getInitials().equals(sqlStatement)) {
                myBatisMySqlTableInfo.setDropTableList(tableNameList);
            }
        }

        return myBatisMySqlTableInfo;
    }

    /**
     * 获取MyBatis Mapper对应的写数据库表信息
     *
     * @param mapperClassName
     * @param mapperMethodName
     * @return
     */
    public WriteDbData4MyBatisMSWriteTable queryMyBatisMySqlWriteTableInfo(String mapperClassName, String mapperMethodName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMWT_QUERY_TABLE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MMWT_SQL_STATEMENT, DC.MMWT_TABLE_NAME) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_WRITE_TABLE.getTableName() +
                    " where " + DC.MMWT_MAPPER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MMWT_MAPPER_METHOD_NAME + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String mapperSimpleClassName = dbOperWrapper.querySimpleClassName(mapperClassName);
        WriteDbData4MyBatisMSWriteTable writeDbData4MyBatisMSWriteTable = dbOperator.queryObject(sql, WriteDbData4MyBatisMSWriteTable.class, mapperSimpleClassName,
                mapperMethodName);
        if (writeDbData4MyBatisMSWriteTable == null) {
            logger.error("未查询到MyBatis Mapper对应的写数据库表信息 {} {}", mapperClassName, mapperMethodName);
        }
        return writeDbData4MyBatisMSWriteTable;
    }

    /**
     * 根据MyBatis Entity类名，查询对应的信息
     *
     * @param entityClassName
     * @return
     */
    public List<WriteDbData4MybatisMSEntity> queryMybatisMSEntityByEntity(String entityClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MME_QUERY_BY_ENTITY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_MYBATIS_MS_ENTITY) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_ENTITY.getTableName() +
                    " where " + DC.MME_ENTITY_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MybatisMSEntity.class, dbOperWrapper.querySimpleClassName(entityClassName));
    }

    /**
     * 查询所有的MyBatis Entity简单类名
     *
     * @return
     */
    public List<String> queryAllMybatisMSEntitySCN() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MME_QUERY_ALL_ENTITY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + DC.MME_ENTITY_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_ENTITY.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class);
    }

    /**
     * 查询所有的MyBatis Entity完整类名
     *
     * @return
     */
    public List<String> queryAllMybatisMSEntityClassName() {
        List<String> list = queryAllMybatisMSEntitySCN();
        List<String> classNameList = new ArrayList<>(list.size());
        for (String simpleClassName : list) {
            String className = classInfoHandler.queryClassNameBySimple(simpleClassName);
            if (className != null) {
                classNameList.add(className);
            }
        }
        return classNameList;
    }

    /**
     * 根据MyBatis的Entity的类名查询对应的数据库表名
     *
     * @param entityClassName MyBatis的Entity的类名
     * @return
     */
    public String queryTableNameByMyBatisEntity(String entityClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MME_QUERY_TABLE_NAME_BY_ENTITY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + DC.MME_TABLE_NAME +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_ENTITY.getTableName() +
                    " where " + DC.MME_ENTITY_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String entitySimpleClassName = dbOperWrapper.querySimpleClassName(entityClassName);
        List<String> list = dbOperator.queryListOneColumn(sql, String.class, entitySimpleClassName);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return null;
        }
        if (list.size() > 1) {
            Collections.sort(list);
            logger.warn("根据MyBatis的Entity的类名查询到多个数据库表名 {} {}", entityClassName, StringUtils.join(list, " "));
        }
        return list.get(0);
    }

    /**
     * 根据MyBatis的Entity的类名及字段名，查询对应的数据库表列名
     *
     * @param entityClassName MyBatis的Entity的类名
     * @param entityFieldName MyBatis的Entity的字段名
     * @return
     */
    public String queryColumnNameByMyBatisEntity(String entityClassName, String entityFieldName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMC_QUERY_TABLE_COLUMN_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + DC.MMC_COLUMN_NAME +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_COLUMN.getTableName() +
                    " where " + DC.MMC_ENTITY_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MMC_ENTITY_FIELD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String entitySimpleClassName = dbOperWrapper.querySimpleClassName(entityClassName);
        List<String> list = dbOperator.queryListOneColumn(sql, String.class, entitySimpleClassName, entityFieldName);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return null;
        }
        if (list.size() > 1) {
            // 根据MyBatis的Entity的类名及字段名查询到多个数据库列名，有可能列名存在不同大小写的形式，判断不区分大小写时是否还存在多种可能
            Set<String> lowerColumnNameSet = new HashSet<>();
            for (String columnName : list) {
                lowerColumnNameSet.add(columnName.toLowerCase());
            }
            if (lowerColumnNameSet.size() > 1) {
                logger.warn("根据MyBatis的Entity的类名及字段名查询到多个数据库列名 {} {} [{}]", entityClassName, entityFieldName, StringUtils.join(lowerColumnNameSet, ","));
            }
        }
        return list.get(0);
    }

    /**
     * 查询在mybatis_ms_entity表中一个Entity对应多个数据库表的情况
     *
     * @return key: Entity的简单类名 value: Entity对应的多条对应mybatis_ms_entity表记录
     */
    public Map<String, List<WriteDbData4MybatisMSEntity>> queryEntityWithMultiTableMap() {
        // 查询在mybatis_ms_entity表中出现多于一次的Entity信息
        List<String> multiEntitySimpleClassNameList = queryMultiEntitySimpleClassNameList();
        if (JavaCG2Util.isCollectionEmpty(multiEntitySimpleClassNameList)) {
            // 未查询到
            return Collections.emptyMap();
        }
        Map<String, List<WriteDbData4MybatisMSEntity>> map = new HashMap<>();
        // 查询到，遍历
        for (String multiEntitySimpleClassName : multiEntitySimpleClassNameList) {
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MME_QUERY_BY_ENTITY_SIMPLE_CLASS_NAME;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_MYBATIS_MS_ENTITY) +
                        " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_ENTITY.getTableName() +
                        " where " + DC.MME_ENTITY_SIMPLE_CLASS_NAME + " = ?" +
                        " order by " + DC.MME_RECORD_ID;
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }
            List<WriteDbData4MybatisMSEntity> mybatisMSEntityList = dbOperator.queryList(sql, WriteDbData4MybatisMSEntity.class, multiEntitySimpleClassName);
            // 记录出现的数据库表名
            Set<String> tableNameSet = new HashSet<>();
            for (WriteDbData4MybatisMSEntity mybatisMSEntity : mybatisMSEntityList) {
                if (StringUtils.isNotBlank(mybatisMSEntity.getTableName())) {
                    tableNameSet.add(mybatisMSEntity.getTableName());
                }
            }
            if (tableNameSet.size() > 1) {
                // mybatis_ms_entity表中当前Entity对应多个数据库表
                map.put(multiEntitySimpleClassName, mybatisMSEntityList);
            }
        }
        return map;
    }

    /**
     * 查询在mybatis_ms_entity表中出现多于一次的Entity信息
     *
     * @return 在mybatis_ms_entity表中出现多于一次的Entity信息
     */
    public List<String> queryMultiEntitySimpleClassNameList() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MME_QUERY_MULTI_ENTITY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MME_ENTITY_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_ENTITY.getTableName() +
                    " where " + DC.MME_ENTITY_SIMPLE_CLASS_NAME + " is not null" +
                    " and " + DC.MME_ENTITY_SIMPLE_CLASS_NAME + " <> ''" +
                    " group by " + DC.MME_ENTITY_SIMPLE_CLASS_NAME +
                    " having count( " + DC.MME_ENTITY_SIMPLE_CLASS_NAME + ") > 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class);
    }

    /**
     * 分页查询MyBatis的Entity与Mapper、表名信息
     *
     * @param lastQuery     是否最后一次查询
     * @param startRecordId 起始记录ID
     * @param endRecordId   结束记录ID
     * @return
     */
    public List<WriteDbData4MybatisMSEntity> queryMyBatisEntityByPage(boolean lastQuery, int startRecordId, int endRecordId) {
        SqlKeyEnum sqlKeyEnum = lastQuery ? SqlKeyEnum.MME_QUERY_ALL_BY_ID : SqlKeyEnum.MME_QUERY_ALL_BY_ID_LAST;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_MYBATIS_MS_ENTITY) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_ENTITY.getTableName() +
                    " where " + DC.MME_RECORD_ID + " > ?";
            if (!lastQuery) {
                sql += " and " + DC.MME_RECORD_ID + " <= ?";
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<Object> argList = new ArrayList<>();
        argList.add(startRecordId);
        if (!lastQuery) {
            argList.add(endRecordId);
        }
        return dbOperator.queryList(sql, WriteDbData4MybatisMSEntity.class, argList.toArray());
    }

    /**
     * 根据MyBatis Mapper唯一类名查询 mybatis_ms_table 表中的Mapper方法名与sql语句类型
     *
     * @param mapperSimpleClassName
     * @return
     */
    public List<WriteDbData4MyBatisMSTable> queryMapperMethodAndSqlStatement(String mapperSimpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMT_QUERY_BY_MAPPER;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + JACGSqlUtil.joinColumns(DC.MMT_MAPPER_METHOD_NAME, DC.MMT_SQL_STATEMENT) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_TABLE.getTableName() +
                    " where " + DC.MMT_MAPPER_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MyBatisMSTable.class, mapperSimpleClassName);
    }

    /**
     * 根据Mapper类名、方法名、参数名从 mybatis_ms_set_column 表查询对应记录
     *
     * @param mapperClassName
     * @param mapperMethodName
     * @param parameterName
     * @return
     */
    public List<WriteDbData4MyBatisMSSetColumn> queryMyBatisMSSetColumnByParameter(String mapperClassName, String mapperMethodName, String parameterName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMSETC_QUERY_ALL_BY_PARAMETER;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_MYBATIS_MS_SET_COLUMN) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_SET_COLUMN.getTableName() +
                    " where " + DC.MMSETC_MAPPER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MMSETC_MAPPER_METHOD_NAME + " = ?" +
                    " and " + DC.MMSETC_PARAM_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MyBatisMSSetColumn.class, dbOperWrapper.querySimpleClassName(mapperClassName), mapperMethodName, parameterName);
    }

    /**
     * 解析MyBatis Mapper方法参数
     *
     * @param mapperFullMethod
     * @param mapperReturnType
     * @return
     */
    private List<AbstractMyBatisMapperArg> parseMyBatisMapperArg(String mapperFullMethod, String mapperReturnType) {
        // 获取方法参数类型列表
        List<String> argTypeList = JACGClassMethodUtil.genMethodArgTypeList(mapperFullMethod);
        List<AbstractMyBatisMapperArg> myBatisMapperArgList = new ArrayList<>(argTypeList.size());
        if (argTypeList.isEmpty()) {
            return myBatisMapperArgList;
        }

        // 获取方法参数泛型类型
        MethodArgGenericsTypeInfo methodArgGenericsTypeInfo = methodArgReturnHandler.queryArgsGenericsTypeInfo(JACGClassMethodUtil.genMethodHashWithLen(mapperFullMethod,
                mapperReturnType));

        for (int i = 0; i < argTypeList.size(); i++) {
            if (methodArgGenericsTypeInfo != null) {
                GenericsTypeValue methodArgGenericsTypeValue = methodArgGenericsTypeInfo.get(i);
                if (methodArgGenericsTypeValue != null) {
                    logger.warn("MyBatis Mapper方法参数使用泛型类型，暂不支持 {} {}", mapperFullMethod, i);
                    myBatisMapperArgList.add(new MyBatisMapperArgNotSupport());
                    continue;
                }
            }

            String argType = argTypeList.get(i);
            AbstractMyBatisMapperArg myBatisMapperArg;
            // 判断参数类型
            if (JavaCG2ClassMethodUtil.isCustomType(argType)) {
                myBatisMapperArg = new MyBatisMapperArgObject();
            } else {
                myBatisMapperArg = new MyBatisMapperArgBasic();
            }

            // 记录参数在sql语句中的名称
            String argNameInSql;
            // 判断参数是否有@Param注解
            StringAnnotationAttribute stringAnnotationAttribute = annotationHandler.queryAttribute4MethodArgAnnotation(mapperFullMethod, mapperReturnType, i,
                    JACGCommonNameConstants.MYBATIS_PARAM_ANNOTATION_NAME, JACGCommonNameConstants.ANNOTATION_ATTRIBUTE_NAME_VALUE, StringAnnotationAttribute.class);
            if (stringAnnotationAttribute != null) {
                argNameInSql = stringAnnotationAttribute.getAttributeString();
            } else {
                // 获取参数名称
                argNameInSql = methodArgReturnHandler.queryMethodArgName(mapperFullMethod, mapperReturnType, i);
            }
            myBatisMapperArg.setArgType(argType);
            myBatisMapperArg.setArgNameInSql(argNameInSql);
            myBatisMapperArgList.add(myBatisMapperArg);
        }
        return myBatisMapperArgList;
    }

    /**
     * 查询MyBatis Mapper方法对应sql语句中update set子句的数据库信息
     *
     * @param mapperClassName
     * @param mapperMethodName
     * @return
     */
    public List<MyBatisMSMapperParamDbInfo> queryMyBatisMSMapperParamDbInfo4Set(String mapperClassName, String mapperMethodName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMSETC_QUERY_COLUMN_INFO_BY_MAPPER;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.COMMON_TABLE_NAME, DC.COMMON_COLUMN_NAME, DC.COMMON_PARAM_OBJ_NAME, DC.COMMON_PARAM_NAME) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_SET_COLUMN.getTableName() +
                    " where " + DC.MMSETC_MAPPER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MMSETC_MAPPER_METHOD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, MyBatisMSMapperParamDbInfo.class, dbOperWrapper.querySimpleClassName(mapperClassName), mapperMethodName);
    }

    /**
     * 查询MyBatis Mapper方法对应sql语句中where子句的数据库信息
     *
     * @param mapperClassName
     * @param mapperMethodName
     * @return
     */
    public List<MyBatisMSMapperParamDbInfo> queryMyBatisMSMapperParamDbInfo4Where(String mapperClassName, String mapperMethodName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMWC_QUERY_COLUMN_INFO_BY_MAPPER;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.COMMON_TABLE_NAME, DC.COMMON_COLUMN_NAME, DC.COMMON_PARAM_OBJ_NAME, DC.COMMON_PARAM_NAME) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_WHERE_COLUMN.getTableName() +
                    " where " + DC.MMWC_MAPPER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MMWC_MAPPER_METHOD_NAME + " = ?" +
                    " and " + DC.MMWC_OPERATION + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, MyBatisMSMapperParamDbInfo.class, dbOperWrapper.querySimpleClassName(mapperClassName), mapperMethodName,
                SQLBinaryOperator.Equality.getName());
    }

    /**
     * 查询指定MyBatis Mapper方法的参数所对应的数据库信息，包括对应的set与where的字段与数据库信息
     *
     * @param mapperClassName
     * @param mapperMethodName
     * @param mapperFullMethod
     * @param mapperReturnType
     * @param myBatisMapperArgAndParamDbInfoList4Where 不允许为空，保存参数在where子句中对应的字段信息，序号与参数序号相同
     * @param myBatisMapperArgAndParamDbInfoList4Set   允许为空，非空时保存参数在update set子句中对应的字段信息，序号与参数序号相同
     * @return
     */
    public List<AbstractMyBatisMapperArg> queryParamDbInfo4MyBatisMapperMethod(String mapperClassName, String mapperMethodName, String mapperFullMethod, String mapperReturnType,
                                                                               List<MyBatisMapperArgAndParamDbInfo> myBatisMapperArgAndParamDbInfoList4Where,
                                                                               List<MyBatisMapperArgAndParamDbInfo> myBatisMapperArgAndParamDbInfoList4Set) {
        // 解析MyBatis Mapper方法参数
        List<AbstractMyBatisMapperArg> myBatisMapperArgList = parseMyBatisMapperArg(mapperFullMethod, mapperReturnType);
        if (JavaCG2Util.isCollectionEmpty(myBatisMapperArgList)) {
            return null;
        }

        // 查询MyBatis Mapper方法对应sql语句中where子句的数据库信息
        List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList4Where = queryMyBatisMSMapperParamDbInfo4Where(mapperClassName, mapperMethodName);
        // 为MyBatis Mapper方法所有的参数查找匹配的数据库信息，where子句
        findParamDbInfo4MyBatisMapperArgs(myBatisMapperArgList, myBatisMSMapperParamDbInfoList4Where, myBatisMapperArgAndParamDbInfoList4Where);

        if (myBatisMapperArgAndParamDbInfoList4Set != null) {
            // 需要处理update set子句
            // 查询MyBatis Mapper方法对应sql语句中update set子句的数据库信息
            List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList4Set = queryMyBatisMSMapperParamDbInfo4Set(mapperClassName, mapperMethodName);
            // 为MyBatis Mapper方法所有的参数查找匹配的数据库信息，update set子句
            findParamDbInfo4MyBatisMapperArgs(myBatisMapperArgList, myBatisMSMapperParamDbInfoList4Set, myBatisMapperArgAndParamDbInfoList4Set);
        }
        return myBatisMapperArgList;
    }

    /**
     * 从MyBatis的Mapper方法参数所对应的数据库信息表，根据通过get/set方法关联的字段关系ID查询相关信息
     *
     * @param fldRelationshipId
     * @return
     */
    public List<WriteDbData4MybatisMSGetSetDb> queryMybatisMSGetSetDbInfoByFldRelationshipId(int fldRelationshipId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMMAD_QUERY_BY_FLD_RELATIONSHIP_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_MYBATIS_MS_GET_SET_DB) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_GET_SET_DB.getTableName() +
                    " where " + DC.MMGSD_FLD_RELATIONSHIP_ID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MybatisMSGetSetDb.class, fldRelationshipId);
    }

    /**
     * 从MyBatis的Mapper方法参数所对应的数据库信息表，根据get方法调用ID查询相关信息
     *
     * @param
     * @return
     */
    public List<WriteDbData4MybatisMSGetSetDb> queryMybatisMSGetSetDbInfoByGetMethodCallId(int getMethodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMMAD_QUERY_BY_GET_METHOD_CALL_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_MYBATIS_MS_GET_SET_DB) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_GET_SET_DB.getTableName() +
                    " where " + DC.MMGSD_GET_METHOD_CALL_ID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MybatisMSGetSetDb.class, getMethodCallId);
    }

    /**
     * 从MyBatis的Mapper方法参数所对应的数据库信息表，根据get方法调用ID查询相关信息
     *
     * @param
     * @return
     */
    public List<WriteDbData4MybatisMSGetSetDb> queryMybatisMSGetSetDbInfoBySetMethodCallId(int setMethodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMMAD_QUERY_BY_SET_METHOD_CALL_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_MYBATIS_MS_GET_SET_DB) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_GET_SET_DB.getTableName() +
                    " where " + DC.MMGSD_SET_METHOD_CALL_ID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MybatisMSGetSetDb.class, setMethodCallId);
    }

    /**
     * 从MyBatis的XML中select的字段信息表查询数据
     *
     * @param mapperClassName
     * @param mapperMethodName
     * @return
     */
    public List<WriteDbData4MyBatisMSSelectColumn> queryMybatisMSSelectDbInfo(String mapperClassName, String mapperMethodName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MMSELC_QUERY_COLUMN_INFO_BY_MAPPER;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MMSELC_TABLE_NAME, DC.MMSELC_COLUMN_NAME, DC.MMSELC_COLUMN_ALIAS) +
                    " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_SELECT_COLUMN.getTableName() +
                    " where " + DC.MMSELC_MAPPER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MMSELC_MAPPER_METHOD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MyBatisMSSelectColumn.class, dbOperWrapper.querySimpleClassName(mapperClassName), mapperMethodName);
    }

    /**
     * 为MyBatis Mapper方法所有的参数查找匹配的数据库信息
     *
     * @param myBatisMapperArgList
     * @param myBatisMSMapperParamDbInfoList
     * @param myBatisMapperArgAndParamDbInfoList 将相关信息写入这个list中
     */
    private void findParamDbInfo4MyBatisMapperArgs(List<AbstractMyBatisMapperArg> myBatisMapperArgList, List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList,
                                                   List<MyBatisMapperArgAndParamDbInfo> myBatisMapperArgAndParamDbInfoList) {
        if (JavaCG2Util.isCollectionEmpty(myBatisMSMapperParamDbInfoList)) {
            return;
        }

        // 遍历当前方法的每个参数
        for (int argSeq = 0; argSeq < myBatisMapperArgList.size(); argSeq++) {
            MyBatisMapperArgAndParamDbInfo myBatisMapperArgAndParamDbInfo = new MyBatisMapperArgAndParamDbInfo();
            AbstractMyBatisMapperArg myBatisMapperArg = myBatisMapperArgList.get(argSeq);
            myBatisMapperArgAndParamDbInfo.setMyBatisMapperArg(myBatisMapperArg);
            myBatisMapperArgAndParamDbInfoList.add(myBatisMapperArgAndParamDbInfo);
            if (myBatisMapperArg instanceof MyBatisMapperArgNotSupport) {
                continue;
            }
            // 生成MyBatis Mapper方法参数在SQL语句中可以使用的名称列表
            List<String> possibleArgNameList = MyBatisTableParserUtil.genMyBatisMapperPossibleArgNameList(myBatisMapperArg.getArgNameInSql(), argSeq);
            for (String possibleArgName : possibleArgNameList) {
                // 为MyBatis Mapper方法指定的参数查找匹配的数据库信息
                findMyBatisMapperParamInfo(myBatisMapperArgList.size() > 1, possibleArgName, myBatisMSMapperParamDbInfoList, myBatisMapperArgAndParamDbInfo);
            }
        }
    }

    /**
     * 为MyBatis Mapper方法指定的参数查找匹配的数据库信息
     *
     * @param mapperMethodMultiArg
     * @param possibleArgName
     * @param myBatisMSMapperParamDbInfoList
     * @param myBatisMSMapperParamDbInfoList
     */
    private void findMyBatisMapperParamInfo(boolean mapperMethodMultiArg, String possibleArgName,
                                            List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList, MyBatisMapperArgAndParamDbInfo myBatisMapperArgAndParamDbInfo) {
        for (MyBatisMSMapperParamDbInfo myBatisMSMapperParamDbInfo : myBatisMSMapperParamDbInfoList) {
            // 当Mapper方法参数数量大于1时，判断MyBatis Mapper方法的参数与sql语句中的参数是否匹配
            if (mapperMethodMultiArg && !checkMyBatisMapperParamMatches(possibleArgName, myBatisMSMapperParamDbInfo)) {
                continue;
            }
            // 为当前的参数记录匹配的数据库信息
            List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoListInArg = myBatisMapperArgAndParamDbInfo.getMyBatisMSMapperParamDbInfoList();
            if (myBatisMSMapperParamDbInfoListInArg == null) {
                myBatisMSMapperParamDbInfoListInArg = new ArrayList<>();
                myBatisMapperArgAndParamDbInfo.setMyBatisMSMapperParamDbInfoList(myBatisMSMapperParamDbInfoListInArg);
            }
            if (!myBatisMSMapperParamDbInfoListInArg.contains(myBatisMSMapperParamDbInfo)) {
                myBatisMSMapperParamDbInfoListInArg.add(myBatisMSMapperParamDbInfo);
            }
        }
    }

    /**
     * 判断MyBatis Mapper方法的参数与sql语句中的参数是否匹配
     *
     * @param possibleArgName
     * @param myBatisMSMapperParamDbInfo
     * @return
     */
    private boolean checkMyBatisMapperParamMatches(String possibleArgName, MyBatisMSMapperParamDbInfo myBatisMSMapperParamDbInfo) {
        if (StringUtils.isNotBlank(myBatisMSMapperParamDbInfo.getParamObjName())) {
            // sql语句中有使用对象名称，比较Java代码的参数名称和sql语句中的对象名称
            return StringUtils.equals(possibleArgName, myBatisMSMapperParamDbInfo.getParamObjName());
        }
        // sql语句中未使用对象名称，比较Java代码的参数名称和sql语句中的参数名称
        return StringUtils.equals(possibleArgName, myBatisMSMapperParamDbInfo.getParamName());
    }
}