package com.adrninistrator.jacg.handler.fieldrelationship;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldRelationship;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.field.FieldBehavior;
import com.adrninistrator.jacg.handler.fieldrelationship.filler.FieldBehaviorFillerInterface;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/9/15
 * @description: 字段原始关联关系报告生成类（get/set方法关联关系）
 */
public class FieldRelationshipRawReportHandler extends BaseHandler implements QueryByPageCallBack<WriteDbData4FieldRelationship> {
    private static final Logger logger = LoggerFactory.getLogger(FieldRelationshipRawReportHandler.class);

    private final FieldRelationshipHandler fieldRelationshipHandler;

    private final GetSetMethodHandler getSetMethodHandler;

    public FieldRelationshipRawReportHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        fieldRelationshipHandler = new FieldRelationshipHandler(dbOperWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
    }

    public FieldRelationshipRawReportHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        fieldRelationshipHandler = new FieldRelationshipHandler(dbOperWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
    }

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        return queryEndIdByPage(currentStartId, DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP, DC.FR_FLD_RELATIONSHIP_ID);
    }

    @Override
    public List<WriteDbData4FieldRelationship> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        logger.debug("分页查询 {} {} {}", lastQuery, currentStartId, currentEndId);
        SqlKeyEnum sqlKeyEnum = lastQuery ? SqlKeyEnum.FR_QUERY_ALL_BY_ID : SqlKeyEnum.FR_QUERY_ALL_BY_ID_LAST;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP.getTableName() +
                    " where " + DC.FR_VALID + " = ?" +
                    " and " + DC.FR_FLD_RELATIONSHIP_ID + " > ?";
            if (!lastQuery) {
                sql += " and " + DC.FR_FLD_RELATIONSHIP_ID + " <= ?";
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Object> argList = new ArrayList<>();
        argList.add(JavaCGYesNoEnum.YES.getIntValue());
        argList.add(currentStartId);
        if (!lastQuery) {
            argList.add(currentEndId);
        }
        return dbOperator.queryList(sql, WriteDbData4FieldRelationship.class, argList.toArray());
    }

    @Override
    public boolean handleDataList(List<WriteDbData4FieldRelationship> dataList, Object... argsByPage) throws Exception {
        BufferedWriter writer = JACGUtil.getArgAt(0, argsByPage);
        FieldBehaviorFillerInterface[] fieldBehaviorFillers = JACGUtil.getArgAt(1, argsByPage);
        for (WriteDbData4FieldRelationship fieldRelationship : dataList) {
            // 查询get方法对应的字段名称
            String getMethodFieldName = queryGetMethodFieldName(fieldRelationship);
            // 查询st方法对应的字段名称
            String setMethodFieldName = querySetMethodFieldName(fieldRelationship);
            if (getMethodFieldName == null || setMethodFieldName == null) {
                logger.error("未查询到对应的get/set方法 [{}] [{}] [{}] [{}]", fieldRelationship.getGetClassName(), fieldRelationship.getGetMethodName(),
                        fieldRelationship.getSetClassName(), fieldRelationship.getSetMethodName());
                continue;
            }
            if (fieldRelationship.getGetClassName().equals(fieldRelationship.getSetClassName()) &&
                    getMethodFieldName.equals(setMethodFieldName)) {
                // 同一个字段的赋值，忽略
                continue;
            }

            // 为get方法对应的字段生成已填充或不需要填充的字段行为
            List<FieldBehavior> getFieldBehaviorList = fieldRelationshipHandler.genFilledFieldBehavior(fieldRelationship, getMethodFieldName, true,
                    fieldBehaviorFillers);
            // 为set方法对应的字段生成已填充或不需要填充的字段行为
            List<FieldBehavior> setFieldBehaviorList = fieldRelationshipHandler.genFilledFieldBehavior(fieldRelationship, setMethodFieldName, false,
                    fieldBehaviorFillers);

            for (FieldBehavior getFieldBehavior : getFieldBehaviorList) {
                for (FieldBehavior setFieldBehavior : setFieldBehaviorList) {
                    String getFieldData = getFieldBehavior.genLineContent(JavaCGConstants.FLAG_TAB);
                    String setFieldData = setFieldBehavior.genLineContent(JavaCGConstants.FLAG_TAB);
                    JavaCGFileUtil.write2FileWithTab(writer, getFieldData, setFieldData, fieldRelationship.getType(),
                            fieldRelationship.getCallerFullMethod(), String.valueOf(fieldRelationship.getCallerLineNumber()));
                }
            }
        }
        return true;
    }

    /**
     * 生成所有的get方法对应的set方法的字段关联关系报告
     *
     * @param reportFilePath       生成的报告文件路径
     * @param fieldBehaviorFillers 为字段行为填充信息的对象数组，在执行完当前方法后需要调用 close 方法进行关闭，建议使用 try-with-resource 方式
     * @return
     */
    public boolean genRawFieldsReport(String reportFilePath, FieldBehaviorFillerInterface... fieldBehaviorFillers) {
        if (ArrayUtils.isEmpty(fieldBehaviorFillers)) {
            logger.error("未指定为字段行为填充信息类的实例");
            return false;
        }

        // 初始化为字段行为填充信息的类
        for (FieldBehaviorFillerInterface fieldBehaviorFiller : fieldBehaviorFillers) {
            fieldBehaviorFiller.init();
        }

        // 写报告文件时使用追加模式
        try (BufferedWriter writer = JavaCGFileUtil.genBufferedWriter(reportFilePath)) {
            // 分页查询并处理
            return QueryByPageHandler.queryAndHandle(this, JavaCGConstants.RECORD_ID_MIN_BEFORE, writer, fieldBehaviorFillers);
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 查询get方法对应的字段名称
    private String queryGetMethodFieldName(WriteDbData4FieldRelationship fieldRelationship) {
        if (JACGConstants.EMPTY_CLASS_METHOD.equals(fieldRelationship.getGetMethodName())) {
            return JACGConstants.EMPTY_CLASS_METHOD;
        }
        BaseWriteDbData4GetSetMethod getMethod = getSetMethodHandler.queryGetSetMethodByClassMethodSuper(true, fieldRelationship.getGetClassName(),
                fieldRelationship.getGetMethodName());
        if (getMethod == null) {
            return null;
        }
        return getMethod.getFieldName();
    }

    // 查询set方法对应的字段名称
    private String querySetMethodFieldName(WriteDbData4FieldRelationship fieldRelationship) {
        if (JACGConstants.EMPTY_CLASS_METHOD.equals(fieldRelationship.getSetMethodName())) {
            return JACGConstants.EMPTY_CLASS_METHOD;
        }
        BaseWriteDbData4GetSetMethod setMethod = getSetMethodHandler.queryGetSetMethodByClassMethodSuper(false, fieldRelationship.getSetClassName(),
                fieldRelationship.getSetMethodName());
        if (setMethod == null) {
            return null;
        }
        return setMethod.getFieldName();
    }
}