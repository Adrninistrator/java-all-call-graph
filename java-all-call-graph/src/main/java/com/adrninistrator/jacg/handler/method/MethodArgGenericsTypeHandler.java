package com.adrninistrator.jacg.handler.method;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.method_arg_generics_type.MethodArgGenericsTypeInfo;
import com.adrninistrator.jacg.handler.dto.method_arg_generics_type.MethodArgGenericsTypeValue;
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

        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{methodHash});
        if (JavaCGUtil.isCollectionEmpty(list)) {
            logger.error("根据方法HASH+长度查询查询对应的方法参数泛型类型不存在 {}", methodHash);
            return null;
        }

        MethodArgGenericsTypeInfo methodArgGenericsTypeInfo = new MethodArgGenericsTypeInfo();
        Map<Integer, MethodArgGenericsTypeValue> genericsTypeMap = new HashMap<>();
        for (Map<String, Object> map : list) {
            Integer argSeq = (Integer) map.get(DC.MAGT_ARG_SEQ);
            String type = (String) map.get(DC.MAGT_TYPE);
            String genericsType = (String) map.get(DC.MAGT_GENERICS_TYPE);
            MethodArgGenericsTypeValue methodArgGenericsTypeValue = genericsTypeMap.computeIfAbsent(argSeq, k -> new MethodArgGenericsTypeValue());
            if (JavaCGConstants.FILE_KEY_METHOD_ARGS_RETURN_TYPE.equals(type)) {
                methodArgGenericsTypeValue.setArgType(genericsType);
            } else {
                methodArgGenericsTypeValue.addArgGenericsType(genericsType);
            }
        }

        for (Map.Entry<Integer, MethodArgGenericsTypeValue> entry : genericsTypeMap.entrySet()) {
            methodArgGenericsTypeInfo.putTypeValue(entry.getKey(), entry.getValue());
        }
        return methodArgGenericsTypeInfo;
    }
}
