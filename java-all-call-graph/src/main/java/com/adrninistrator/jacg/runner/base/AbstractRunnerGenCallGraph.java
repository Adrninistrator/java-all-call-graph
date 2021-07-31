package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.common.Constants;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.util.CommonUtil;
import com.adrninistrator.jacg.util.FileUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description:
 */

public abstract class AbstractRunnerGenCallGraph extends AbstractRunner {

    private static final Logger logger = LoggerFactory.getLogger(AbstractRunnerGenCallGraph.class);

    // 配置文件中指定的需要处理的任务
    protected Set<String> taskSet;

    // 保存当前生成输出文件时的目录前缀
    protected String outputDirPrefix;

    /*
        方法注解信息
        key：方法HASH+长度
        value：所有注解排序后拼接，分隔符为半角逗号,
     */
    protected Map<String, String> methodAnnotationsMap = new HashMap<>(100);

    // 从方法调用关系表查询指定的类是否存在
    protected boolean checkClassNameExists(String className) {
        /*
            对于 select xxx limit n union all select xxx limit n 写法
            10.0.10-MariaDB-V2.0R132D001-20170821-1540 版本允许
            10.1.9-MariaDBV1.0R012D003-20180427-1600 版本不允许
            因此使用 select xxx from ((select xxx limit n) union all (select xxx limit n)) as a的写法
         */
        String sql = sqlCacheMap.get(Constants.SQL_KEY_MC_QUERY_CLASS_EXISTS);
        if (sql == null) {
            sql = "select class_name from ((select " + DC.MC_CALLER_CLASS_NAME + " as class_name from " + Constants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() +
                    " where " + DC.MC_CALLER_CLASS_NAME + " = ? limit 1) union all (select " + DC.MC_CALLEE_CLASS_NAME + " as class_name " +
                    "from " + Constants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName() +
                    " where " + DC.MC_CALLEE_CLASS_NAME + " = ? limit 1)) as az";
            cacheSql(Constants.SQL_KEY_MC_QUERY_CLASS_EXISTS, sql);
        }

        List<Object> list = dbOperator.queryListOneObject(sql, new Object[]{className, className});
        if (list == null) {
            return false;
        }

        if (CommonUtil.isCollectionEmpty(list)) {
            logger.error("指定的类从调用关系表中未查询到，请检查是否需要使用完整类名，或使用简单类名 {}", className);

            // 猜测对应的类名
            guessClassName(className);
            return false;
        }

        return true;
    }

    // 猜测对应的类名
    protected void guessClassName(String className) {
        if (className.contains(Constants.FLAG_DOT)) {
            // 完整类名，查找对应的简单类名
            String sql = sqlCacheMap.get(Constants.SQL_KEY_CN_QUERY_SIMPLE_CLASS);
            if (sql == null) {
                sql = "select " + DC.CN_SIMPLE_NAME + " from " + Constants.TABLE_PREFIX_CLASS_NAME + confInfo.getAppName() +
                        " where " + DC.CN_FULL_NAME + " = ?";
                cacheSql(Constants.SQL_KEY_CN_QUERY_SIMPLE_CLASS, sql);
            }

            List<Object> list = dbOperator.queryListOneObject(sql, new Object[]{className});
            if (!CommonUtil.isCollectionEmpty(list)) {
                logger.error("指定的完整类名请确认是否需要使用简单类名形式 {}->{}", className, list.get(0));
            }
            return;
        }

        // 简单类名，查找对应的完整类名
        String sql = sqlCacheMap.get(Constants.SQL_KEY_CN_QUERY_FULL_CLASS);
        if (sql == null) {
            sql = "select " + DC.CN_FULL_NAME + " from " + Constants.TABLE_PREFIX_CLASS_NAME + confInfo.getAppName() +
                    " where " + DC.CN_SIMPLE_NAME + " = ?";
            cacheSql(Constants.SQL_KEY_CN_QUERY_FULL_CLASS, sql);
        }

        List<Object> list = dbOperator.queryListOneObject(sql, new Object[]{className});
        if (!CommonUtil.isCollectionEmpty(list)) {
            logger.error("指定的简单类名请确认是否存在同名类，是否需要使用完整类名形式 {}->{}", className, StringUtils.join(list, " "));
        }
    }

    // 读取方法注解
    protected boolean readMethodAnnotation() {
        logger.info("读取方法注解");
        String sql = sqlCacheMap.get(Constants.SQL_KEY_MA_QUERY_METHOD_ANNOTATION);
        if (sql == null) {
            String columns = StringUtils.join(Constants.TABLE_COLUMNS_METHOD_ANNOTATION, Constants.FLAG_COMMA_WITH_SPACE);
            sql = "select " + columns + " from " + Constants.TABLE_PREFIX_METHOD_ANNOTATION + confInfo.getAppName() +
                    " order by " + DC.MA_METHOD_HASH + ", " + DC.MA_ANNOTATION_NAME;
            cacheSql(Constants.SQL_KEY_MA_QUERY_METHOD_ANNOTATION, sql);
        }

        List<Map<String, Object>> list = dbOperator.queryList(sql, null);
        if (list == null) {
            return false;
        }

        if (CommonUtil.isCollectionEmpty(list)) {
            return true;
        }

        for (Map<String, Object> map : list) {
            String methodhash = (String) map.get(DC.MA_METHOD_HASH);
            String annotationName = (String) map.get(DC.MA_ANNOTATION_NAME);

            String existedAnnotationName = methodAnnotationsMap.get(methodhash);
            if (existedAnnotationName == null) {
                methodAnnotationsMap.put(methodhash, Constants.FLAG_AT + annotationName);
            } else {
                methodAnnotationsMap.put(methodhash, existedAnnotationName + Constants.FLAG_AT + annotationName);
            }
        }

        return true;
    }

    // 读取配置文件中指定的需要处理的任务
    protected boolean readTaskInfo(String taskFile) {
        taskSet = FileUtil.readFile2Set(taskFile);
        if (CommonUtil.isCollectionEmpty(taskSet)) {
            logger.error("读取文件不存在或内容为空 {}", taskFile);
            return false;
        }

        if (taskSet.size() < confInfo.getThreadNum()) {
            logger.info("将线程数修改为需要处理的任务数 {}", taskSet.size());
            confInfo.setThreadNum(taskSet.size());
        }

        return true;
    }

    // 创建输出文件所在目录
    protected boolean createOutputDit(String prefix) {
        outputDirPrefix = prefix + File.separator + CommonUtil.currentTime();
        logger.info("创建保存输出文件的目录 {}", outputDirPrefix);
        // 判断目录是否存在，不存在时尝试创建
        return FileUtil.isDirectoryExists(outputDirPrefix);
    }

    // 生成输出文件前缀，包含了当前方法的调用层级
    protected String genOutputPrefix(int level) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("[").append(level).append("]")
                .append(Constants.FLAG_HASHTAG)
                .append(CommonUtil.genOutputFlag(level));
        return stringBuilder.toString();
    }

    // 将输出文件合并
    protected void combineOutputFile(String fileName) {
        if (confInfo.isGenCombinedOutput()) {
            List<File> outputFileList = FileUtil.findFileInDir(outputDirPrefix, Constants.EXT_TXT);
            if (!CommonUtil.isCollectionEmpty(outputFileList) && outputFileList.size() > 1) {
                String combinedOutputFilePath = outputDirPrefix + File.separator + Constants.COMBINE_FILE_NAME_PREFIX + fileName + Constants.EXT_TXT;
                FileUtil.combineTextFile(combinedOutputFilePath, outputFileList);
            }
        }
    }
}
