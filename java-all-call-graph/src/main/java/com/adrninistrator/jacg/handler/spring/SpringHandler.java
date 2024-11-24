package com.adrninistrator.jacg.handler.spring;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringController;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringTask;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.conf.JavaCG2ConfigHandler;
import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfo;
import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfoDetail;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
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
    private final JavaCG2ConfigHandler javaCG2ConfigHandler;

    public SpringHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        javaCG2ConfigHandler = new JavaCG2ConfigHandler(dbOperWrapper);
    }

    public SpringHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        javaCG2ConfigHandler = new JavaCG2ConfigHandler(dbOperWrapper);
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
            sql = "select " + JACGSqlUtil.joinColumns(DC.SPC_SHOW_URI, DC.SPC_FULL_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_SPRING_CONTROLLER.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<WriteDbData4SpringController> list = dbOperator.queryList(sql, WriteDbData4SpringController.class);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }
        List<SpringControllerInfo> springControllerInfoList = new ArrayList<>(list.size());
        for (WriteDbData4SpringController springController : list) {
            springControllerInfoList.add(new SpringControllerInfo(springController.getShowUri(), springController.getFullMethod()));
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
     * @return 空列表: 指定的方法不是Spring Controller方法
     */
    public List<String> queryControllerUriList(String fullMethod) {
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
    public String queryControllerUri(String fullMethod) {
        List<String> uriList = queryControllerUriList(fullMethod);
        if (JavaCG2Util.isCollectionEmpty(uriList)) {
            return null;
        }
        return uriList.get(0);
    }

    private void checkParseMethodCallTypeValue() {
        if (!javaCG2ConfigHandler.checkParseMethodCallTypeValue()) {
            logger.error("使用 java-callgraph2 组件处理方法调用时未解析被调用对象和参数可能的类型与值，无法判断 Spring Controller 方法参数是否有被使用" +
                            "需要将配置文件 {} 的参数 {} 值指定为 true，可参考 test.runbycode.example.TestSetJavaCG2Config 类", JavaCG2Constants.FILE_PATH_CONFIG,
                    JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getKey());
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
            WriteDbData4MethodInfo methodInfo = methodInfoHandler.queryMethodInfoByFullMethod(fileUploadMethod.getFullMethod());
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
    public List<WriteDbData4SpringController> queryFileDownloadControllerMethod() {
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
    public List<String> queryFileDownloadControllerOnlyMethod() {
        // 查询所有可能用于文件下载的Spring Controller方法
        List<WriteDbData4SpringController> fileDownloadMethodList = queryFileDownloadControllerMethod();
        if (JavaCG2Util.isCollectionEmpty(fileDownloadMethodList)) {
            return Collections.emptyList();
        }
        List<String> fullMethodList = new ArrayList<>(fileDownloadMethodList.size());
        for (WriteDbData4SpringController fileDownloadMethod : fileDownloadMethodList) {
            fullMethodList.add(fileDownloadMethod.getFullMethod());
        }
        return fullMethodList;
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