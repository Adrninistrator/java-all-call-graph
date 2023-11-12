package com.adrninistrator.jacg.handler.method;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodArgGenericsType;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodReturnGenericsType;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.generics_type.MethodArgGenericsTypeInfo;
import com.adrninistrator.jacg.handler.dto.generics_type.GenericsTypeValue;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/21
 * @description: 方法参数泛型类型处理类
 */
public class MethodArgGenericsTypeHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(MethodArgGenericsTypeHandler.class);

    public MethodArgGenericsTypeHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public MethodArgGenericsTypeHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 根据方法HASH+长度查询查询对应的方法参数泛型类型
     *
     * @param methodHash
     * @return
     */
    public MethodArgGenericsTypeInfo queryGenericsTypeInfo(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MAGT_QUERY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MAGT_ARG_SEQ, DC.MAGT_TYPE, DC.MAGT_GENERICS_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ARG_GENERICS_TYPE.getTableName() +
                    " where " + DC.MAGT_METHOD_HASH + " = ?" +
                    " order by " + JACGSqlUtil.joinColumns(DC.MAGT_ARG_SEQ, DC.MAGT_TYPE, DC.MAGT_TYPE_SEQ);
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<WriteDbData4MethodArgGenericsType> list = dbOperator.queryList(sql, WriteDbData4MethodArgGenericsType.class, methodHash);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            logger.error("根据方法HASH+长度查询查询对应的方法参数泛型类型不存在 {}", methodHash);
            return null;
        }

        MethodArgGenericsTypeInfo methodArgGenericsTypeInfo = new MethodArgGenericsTypeInfo();
        Map<Integer, GenericsTypeValue> genericsTypeMap = new HashMap<>();
        for (WriteDbData4MethodArgGenericsType writeDbData4MethodArgGenericsType : list) {
            GenericsTypeValue methodArgGenericsTypeValue = genericsTypeMap.computeIfAbsent(writeDbData4MethodArgGenericsType.getArgSeq(),
                    k -> new GenericsTypeValue());
            if (JavaCGConstants.FILE_KEY_METHOD_ARGS_RETURN_TYPE.equals(writeDbData4MethodArgGenericsType.getType())) {
                methodArgGenericsTypeValue.setType(writeDbData4MethodArgGenericsType.getGenericsType());
            } else {
                methodArgGenericsTypeValue.addGenericsType(writeDbData4MethodArgGenericsType.getGenericsType());
            }
        }

        for (Map.Entry<Integer, GenericsTypeValue> entry : genericsTypeMap.entrySet()) {
            methodArgGenericsTypeInfo.putTypeValue(entry.getKey(), entry.getValue());
        }
        return methodArgGenericsTypeInfo;
    }

    /**
     * 根据方法HASH+长度查询对应的方法返回泛型类型
     *
     * @param methodHash
     * @return
     */
    public GenericsTypeValue queryReturnGenericsTypeInfo(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MRGT_QUERY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MRGT_TYPE, DC.MRGT_GENERICS_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_RETURN_GENERICS_TYPE.getTableName() +
                    " where " + DC.MRGT_METHOD_HASH + " = ?" +
                    " order by " + JACGSqlUtil.joinColumns(DC.MRGT_TYPE, DC.MRGT_TYPE_SEQ);
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<WriteDbData4MethodReturnGenericsType> list = dbOperator.queryList(sql, WriteDbData4MethodReturnGenericsType.class, methodHash);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return null;
        }

        GenericsTypeValue methodReturnGenericsTypeInfo = new GenericsTypeValue();
        for (WriteDbData4MethodReturnGenericsType writeDbData4MethodReturnGenericsType : list) {
            if (JavaCGConstants.FILE_KEY_METHOD_ARGS_RETURN_TYPE.equals(writeDbData4MethodReturnGenericsType.getType())) {
                methodReturnGenericsTypeInfo.setType(writeDbData4MethodReturnGenericsType.getGenericsType());
            } else {
                methodReturnGenericsTypeInfo.addGenericsType(writeDbData4MethodReturnGenericsType.getGenericsType());
            }
        }
        return methodReturnGenericsTypeInfo;
    }
}
