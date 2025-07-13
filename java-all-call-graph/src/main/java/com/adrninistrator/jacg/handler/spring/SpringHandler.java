package com.adrninistrator.jacg.handler.spring;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.comparator.Comparator4FullMethodWithReturnType;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAdvice;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringBean;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringController;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringTask;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.conf.ConfigHandler;
import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfo;
import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfoDetail;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.aspectj.weaver.AdviceKind;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/6
 * @description: Spring相关的查询处理类
 */
public class SpringHandler extends BaseHandler {

    private static final Logger logger = LoggerFactory.getLogger(SpringHandler.class);
    private final MethodInfoHandler methodInfoHandler;
    private final ConfigHandler configHandler;

    public SpringHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        configHandler = new ConfigHandler(dbOperWrapper);
    }

    public SpringHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        configHandler = new ConfigHandler(dbOperWrapper);
    }

    /**
     * 获取所有的Spring Controller信息，包括URI与完整方法
     *
     * @return
     */
    public List<SpringControllerInfo> queryAllControllerInfo() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPC_QUERY_ALL;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.SPC_SHOW_URI, DC.SPC_FULL_METHOD, DC.SPC_RETURN_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_CONTROLLER.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<WriteDbData4SpringController> list = dbOperator.queryList(sql, WriteDbData4SpringController.class);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }
        List<SpringControllerInfo> springControllerInfoList = new ArrayList<>(list.size());
        for (WriteDbData4SpringController springController : list) {
            springControllerInfoList.add(new SpringControllerInfo(springController.getShowUri(), springController.getFullMethod(), springController.getReturnType()));
        }
        springControllerInfoList.sort(Comparator.comparing(SpringControllerInfo::getFullMethod));
        return springControllerInfoList;
    }

    /**
     * 获取所有的Spring Controller的完整方法
     *
     * @return
     */
    public List<String> queryAllControllerFullMethod() {
        List<SpringControllerInfo> list = queryAllControllerInfo();
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }
        List<String> fullMethodList = new ArrayList<>();
        for (SpringControllerInfo springControllerInfo : list) {
            fullMethodList.add(springControllerInfo.getFullMethod());
        }
        return fullMethodList;
    }

    /**
     * 查询Spring Controller对应的全部简单类名
     *
     * @return
     */
    public List<String> queryAllControllerSCN() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPC_QUERY_ALL_SCN;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + DC.SPC_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_SPRING_CONTROLLER.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class);
    }

    /**
     * 根据简单类名查询对应的Spring Controller
     *
     * @param simpleClassName
     * @return
     */
    public List<WriteDbData4SpringController> queryControllerBySCN(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPC_QUERY_BY_SCN;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_SPRING_CONTROLLER) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_CONTROLLER.getTableName() +
                    " where " + DC.SPC_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4SpringController.class, simpleClassName);
    }

    /**
     * 获取所有的Spring Task方法
     *
     * @return
     */
    public List<String> queryAllTaskMethod() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPT_QUERY;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.SPT_CLASS_NAME, DC.SPT_METHOD_NAME) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_TASK.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<WriteDbData4SpringTask> list = dbOperator.queryList(sql, WriteDbData4SpringTask.class);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }
        List<String> springTaskMethodList = new ArrayList<>(list.size());
        for (WriteDbData4SpringTask springTask : list) {
            String fullMethod = JavaCG2ClassMethodUtil.formatFullMethodStr(springTask.getClassName(), springTask.getMethodName());
            springTaskMethodList.add(fullMethod);
        }
        Collections.sort(springTaskMethodList);
        return springTaskMethodList;
    }

    /**
     * 根据完整方法查询Spring Controller对应的URI列表
     *
     * @param fullMethod
     * @param returnType
     * @return 空列表: 指定的方法不是Spring Controller方法
     */
    public List<String> queryControllerUriList(String fullMethod, String returnType) {
        String methodHash = JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType);
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
     * @param returnType
     * @return null: 指定的方法不是Spring Controller方法
     */
    public String queryControllerUri(String fullMethod, String returnType) {
        List<String> uriList = queryControllerUriList(fullMethod, returnType);
        if (JavaCG2Util.isCollectionEmpty(uriList)) {
            return null;
        }
        return uriList.get(0);
    }

    private void checkParseMethodCallTypeValue() {
        if (!configHandler.checkParseMethodCallTypeValue()) {
            logger.error("使用 java-callgraph2 组件处理方法调用时未解析被调用对象和参数可能的类型与值，无法判断 Spring Controller 方法参数是否有被使用" +
                    "需要将参数值指定为 {} {}", Boolean.TRUE, configureWrapper.genConfigUsage(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE));
            throw new JavaCG2RuntimeException("使用 java-callgraph2 组件处理方法调用时未解析被调用对象和参数可能的类型与值，无法判断 Spring Controller 方法参数是否有被使用，请按照日志提示处理");
        }
    }

    /**
     * 查询所有可能用于文件上传的Spring Controller方法
     *
     * @return
     */
    public List<WriteDbData4SpringController> queryFileUploadControllerMethod() {
        checkParseMethodCallTypeValue();

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPC_QUERY_FILE_UPLOAD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_SPRING_CONTROLLER) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_CONTROLLER.getTableName() +
                    " where " + DC.SPC_MAYBE_FILE_UPLOAD + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<WriteDbData4SpringController> list = dbOperator.queryList(sql, WriteDbData4SpringController.class, JavaCG2YesNoEnum.YES.getIntValue());
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }
        list.sort(Comparator.comparing(WriteDbData4SpringController::getShowUri));
        return list;
    }

    /**
     * 查询所有的用于文件上传的Spring Controller方法详情
     *
     * @return
     */
    public List<SpringControllerInfoDetail> queryFileUploadControllerMethodDetail() {
        // 查询所有可能用于文件上传的Spring Controller方法
        List<WriteDbData4SpringController> fileUploadMethodList = queryFileUploadControllerMethod();
        if (JavaCG2Util.isCollectionEmpty(fileUploadMethodList)) {
            return Collections.emptyList();
        }
        List<SpringControllerInfoDetail> springControllerInfoDetailList = new ArrayList<>(fileUploadMethodList.size());
        for (WriteDbData4SpringController fileUploadMethod : fileUploadMethodList) {
            // 查询方法返回类型
            WriteDbData4MethodInfo methodInfo = methodInfoHandler.queryMethodInfoByFullMethod(fileUploadMethod.getFullMethod(), fileUploadMethod.getReturnType());
            SpringControllerInfoDetail springControllerInfoDetail = new SpringControllerInfoDetail();
            springControllerInfoDetail.setSpringController(fileUploadMethod);
            springControllerInfoDetail.setMethodInfo(methodInfo);
            springControllerInfoDetailList.add(springControllerInfoDetail);
        }
        return springControllerInfoDetailList;
    }

    /**
     * 查询所有可能用于文件上传的Spring Controller方法，仅查询方法
     *
     * @return
     */
    public List<String> queryFileUploadControllerOnlyMethod() {
        // 查询所有可能用于文件上传的Spring Controller方法
        List<WriteDbData4SpringController> fileUploadMethodList = queryFileUploadControllerMethod();
        if (JavaCG2Util.isCollectionEmpty(fileUploadMethodList)) {
            return Collections.emptyList();
        }
        List<String> fullMethodList = new ArrayList<>(fileUploadMethodList.size());
        for (WriteDbData4SpringController fileUploadMethod : fileUploadMethodList) {
            fullMethodList.add(fileUploadMethod.getFullMethod());
        }
        return fullMethodList;
    }

    /**
     * 查询所有可能用于文件下载的Spring Controller方法
     *
     * @return
     */
    public List<WriteDbData4SpringController> queryFileDownloadControllerInfo() {
        checkParseMethodCallTypeValue();

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPC_QUERY_FILE_DOWNLOAD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_SPRING_CONTROLLER) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_CONTROLLER.getTableName() +
                    " where " + DC.SPC_MAYBE_FILE_DOWNLOAD + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<WriteDbData4SpringController> list = dbOperator.queryList(sql, WriteDbData4SpringController.class, JavaCG2YesNoEnum.YES.getIntValue());
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }
        list.sort(Comparator.comparing(WriteDbData4SpringController::getShowUri));
        return list;
    }

    /**
     * 查询所有可能用于文件下载的Spring Controller方法，仅查询方法
     *
     * @return
     */
    public List<FullMethodWithReturnType> queryFileDownloadControllerMethod() {
        // 查询所有可能用于文件下载的Spring Controller方法
        List<WriteDbData4SpringController> fileDownloadMethodList = queryFileDownloadControllerInfo();
        if (JavaCG2Util.isCollectionEmpty(fileDownloadMethodList)) {
            return Collections.emptyList();
        }
        List<FullMethodWithReturnType> methodList = new ArrayList<>(fileDownloadMethodList.size());
        for (WriteDbData4SpringController fileDownloadMethod : fileDownloadMethodList) {
            methodList.add(new FullMethodWithReturnType(fileDownloadMethod.getFullMethod(), fileDownloadMethod.getReturnType()));
        }
        methodList.sort(Comparator4FullMethodWithReturnType.getInstance());
        return methodList;
    }

    /**
     * 判断指定的方法是否为Spring Task方法
     *
     * @param fullMethod
     * @param returnType
     * @return
     */
    public boolean checkSpringTask(String fullMethod, String returnType) {
        String methodHash = JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType);
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

    /**
     * 根据类名查询Spring Bean信息
     *
     * @param className
     * @return
     */
    public List<WriteDbData4SpringBean> querySpringBeanByClassName(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPB_QUERY_BY_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_SPRING_BEAN) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_BEAN.getTableName() +
                    " where " + DC.SPB_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4SpringBean.class, dbOperWrapper.querySimpleClassName(className));
    }

    /**
     * 根据Bean名称查询Spring Bean信息
     *
     * @param beanName
     * @return
     */
    public List<WriteDbData4SpringBean> querySpringBeanByBeanName(String beanName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPB_QUERY_BY_BEAN_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_SPRING_BEAN) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_BEAN.getTableName() +
                    " where " + DC.SPB_SPRING_BEAN_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4SpringBean.class, beanName);
    }

    /**
     * 根据Bean名称查询Spring Bean类名
     *
     * @param beanName
     * @return
     */
    public String queryClassNameBySpringBeanName(String beanName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPB_QUERY_CLASS_NAME_BY_BEAN_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.SPB_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_SPRING_BEAN.getTableName() +
                    " where " + DC.SPB_SPRING_BEAN_NAME + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, beanName);
    }

    /**
     * 根据Bean名称查询Spring Bean类名
     *
     * @return
     */
    public List<String> queryDistinctBeanClassName() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SPB_QUERY_DISTINCT_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.SPB_CLASS_NAME + ")" +
                    " from " + DbTableInfoEnum.DTIE_SPRING_BEAN.getTableName() +
                    " order by " + DC.SPB_CLASS_NAME;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class);
    }

    /**
     * 根据XML中定义的pointcut ID查询对应的表达式
     *
     * @param pointcutId
     * @return
     */
    public String queryExpressionByPointcutId(String pointcutId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SAP_QUERY_EXPRESSION_BY_POINTCUT_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.SAP_EXPRESSION +
                    " from " + DbTableInfoEnum.DTIE_SPRING_AOP_POINTCUT.getTableName() +
                    " where " + DC.SAP_XML_POINTCUT_ID + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, pointcutId);
    }

    /**
     * 根据XML中定义的pointcut ID查询对应的表达式
     *
     * @param aspectId
     * @return
     */
    public String queryBeanNameByAspectId(String aspectId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SAAS_QUERY_BEAN_NAME_BY_ASPECT_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.SAAS_XML_ASPECT_REF +
                    " from " + DbTableInfoEnum.DTIE_SPRING_AOP_ASPECT.getTableName() +
                    " where " + DC.SAAS_XML_ASPECT_ID + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, aspectId);
    }

    /**
     * 查询Spring AOP advice中的Around信息
     *
     * @return
     */
    public List<WriteDbData4SpringAopAdvice> querySpringAopAroundInAdvice() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SAAD_QUERY_SPRING_AOP_AROUND_IN_ADVICE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_SPRING_AOP_ADVICE) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_AOP_ADVICE.getTableName() +
                    " where " + DC.SAAD_ADVICE_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4SpringAopAdvice.class, AdviceKind.Around.getName());
    }

    /**
     * 查询所有的Spring AOP advice
     *
     * @return
     */
    public List<WriteDbData4SpringAopAdvice> querySpringAopAdvice() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.SAAD_QUERY_SPRING_AOP_ADVICE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_SPRING_AOP_ADVICE) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_AOP_ADVICE.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4SpringAopAdvice.class);
    }
}