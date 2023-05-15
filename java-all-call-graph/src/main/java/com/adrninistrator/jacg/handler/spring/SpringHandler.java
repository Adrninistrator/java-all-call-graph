package com.adrninistrator.jacg.handler.spring;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.comparator.Comparator4SpringControllerInfo;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringController;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringTask;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfo;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.util.JavaCGMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/6
 * @description: Spring相关的查询处理类
 */
public class SpringHandler extends BaseHandler {
    public SpringHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public SpringHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 获取所有的Spring Controller信息，包括URI与完整方法
     *
     * @return
     */
    public List<SpringControllerInfo> getAllControllerMethod() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPC_QUERY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.SPC_SHOW_URI, DC.SPC_FULL_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_CONTROLLER.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<WriteDbData4SpringController> list = dbOperator.queryList(sql, WriteDbData4SpringController.class);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }
        List<SpringControllerInfo> springControllerInfoList = new ArrayList<>(list.size());
        for (WriteDbData4SpringController springController : list) {
            SpringControllerInfo springControllerInfo = new SpringControllerInfo(springController.getShowUri(), springController.getFullMethod());
            springControllerInfoList.add(springControllerInfo);
        }
        springControllerInfoList.sort(Comparator4SpringControllerInfo.getInstance());
        return springControllerInfoList;
    }

    /**
     * 获取所有的Spring Task方法
     *
     * @return
     */
    public List<String> getAllTaskMethod() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPT_QUERY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.SPT_CLASS_NAME, DC.SPT_METHOD_NAME) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_TASK.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<WriteDbData4SpringTask> list = dbOperator.queryList(sql, WriteDbData4SpringTask.class);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }
        List<String> springTaskMethodList = new ArrayList<>(list.size());
        for (WriteDbData4SpringTask springTask : list) {
            String fullMethod = JavaCGMethodUtil.formatFullMethodStr(springTask.getClassName(), springTask.getMethodName());
            springTaskMethodList.add(fullMethod);
        }
        Collections.sort(springTaskMethodList);
        return springTaskMethodList;
    }

    /**
     * 根据完整方法查询Spring Controller对应的URI列表
     *
     * @param fullMethod
     * @return 空列表: 指定的方法不是Spring Controller方法
     */
    public List<String> getControllerUriList(String fullMethod) {
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPC_QUERY_BY_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.SPC_SHOW_URI +
                    " from " + DbTableInfoEnum.DTIE_SPRING_CONTROLLER.getTableName() +
                    " where " + DC.SPC_METHOD_HASH + " = ?" +
                    " order by " + DC.SPC_SEQ;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryListOneColumn(sql, String.class, methodHash);
    }

    /**
     * 根据完整方法查询Spring Controller对应的URI，获取第1个
     *
     * @param fullMethod
     * @return null: 指定的方法不是Spring Controller方法
     */
    public String getControllerUri(String fullMethod) {
        List<String> uriList = getControllerUriList(fullMethod);
        if (JavaCGUtil.isCollectionEmpty(uriList)) {
            return null;
        }
        return uriList.get(0);
    }

    /**
     * 判断指定的方法是否为Spring Task方法
     *
     * @param fullMethod
     * @return
     */
    public boolean checkSpringTask(String fullMethod) {
        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodName = JACGClassMethodUtil.getMethodNameFromFull(fullMethod);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPT_QUERY_BY_CLASS_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select count(*) from " + DbTableInfoEnum.DTIE_SPRING_TASK.getTableName() +
                    " where " + DC.SPT_CLASS_NAME + " = ?" +
                    " and " + DC.SPT_METHOD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Long count = dbOperator.queryObjectOneColumn(sql, Long.class, className, methodName);
        return count != null && count > 0;
    }
}