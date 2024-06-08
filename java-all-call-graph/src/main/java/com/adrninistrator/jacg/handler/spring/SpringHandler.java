package com.adrninistrator.jacg.handler.spring;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.comparator.Comparator4SpringControllerInfo;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringController;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringTask;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfo;
import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfoDetail;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/1/6
 * @description: Spring相关的查询处理类
 */
public class SpringHandler extends BaseHandler {

    private final MethodInfoHandler methodInfoHandler;

    private final MethodArgReturnHandler methodArgReturnHandler;

    public SpringHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
    }

    public SpringHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
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
            String fullMethod = JavaCGClassMethodUtil.formatFullMethodStr(springTask.getClassName(), springTask.getMethodName());
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
     * 查询所有的用于上传文件的Spring Controller信息
     *
     * @return
     */
    public List<SpringControllerInfoDetail> queryUploadFileControllerInfo() {
        Set<String> fullMethodSetAll = new HashSet<>();
        // 查询方法参数包含Spring用于上传文件的完整方法
        Set<String> fullMethodSet1 = methodArgReturnHandler.findMethodByArgType(JACGCommonNameConstants.SPRING_MULTI_PART_FILE_CLASS);
        Set<String> fullMethodSet2 = methodArgReturnHandler.findMethodByArgType(JACGCommonNameConstants.SPRING_COMMONS_MULTI_PART_FILE_CLASS);
        fullMethodSetAll.addAll(fullMethodSet1);
        fullMethodSetAll.addAll(fullMethodSet2);
        if (fullMethodSetAll.isEmpty()) {
            return null;
        }
        List<SpringControllerInfoDetail> springControllerInfoDetailList = new ArrayList<>();
        List<String> fullMethodList = new ArrayList<>(fullMethodSetAll);
        Collections.sort(fullMethodList);
        for (String fullMethod : fullMethodList) {
            // 判断方法是否属于Spring Controller方法
            String uri = getControllerUri(fullMethod);
            if (uri == null) {
                continue;
            }
            // 查询方法返回类型
            WriteDbData4MethodInfo methodInfo = methodInfoHandler.queryMethodInfoByFullMethod(fullMethod);
            SpringControllerInfoDetail springControllerInfoDetail = new SpringControllerInfoDetail(uri, fullMethod, methodInfo.getReturnType());
            springControllerInfoDetailList.add(springControllerInfoDetail);
        }
        return springControllerInfoDetailList;
    }

    /**
     * 判断指定的方法是否为Spring Task方法
     *
     * @param fullMethod
     * @return
     */
    public boolean checkSpringTask(String fullMethod) {
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPT_QUERY_BY_FULL_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.SPT_METHOD_HASH +
                    " from " + DbTableInfoEnum.DTIE_SPRING_TASK.getTableName() +
                    " where " + DC.SPT_METHOD_HASH + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String result = dbOperator.queryObjectOneColumn(sql, String.class, methodHash);
        return result != null;
    }
}