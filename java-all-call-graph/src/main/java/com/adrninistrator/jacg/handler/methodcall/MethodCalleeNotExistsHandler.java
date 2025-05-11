package com.adrninistrator.jacg.handler.methodcall;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/12/5
 * @description: 查询方法调用中不存在的被调用方法
 */
public class MethodCalleeNotExistsHandler extends BaseHandler implements QueryByPageCallBack<String> {
    private static final Logger logger = LoggerFactory.getLogger(MethodCalleeNotExistsHandler.class);

    private final ClassInfoHandler classInfoHandler;
    private final MethodInfoHandler methodInfoHandler;
    private final MethodCallHandler methodCallHandler;

    public MethodCalleeNotExistsHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
    }

    public MethodCalleeNotExistsHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
    }

    /**
     * 查询方法调用中不存在的被调用方法
     *
     * @param ignoreClassInJdk            是否忽略JDK中的类被调用的情况 true: 忽略 false: 不忽略
     * @param ignoreCallerClassPrefixList 需要忽略的调用类名前缀，可使用包名，或名包+类名
     * @param ignoreCalleeClassPrefixList 需要忽略的被调用类名前缀，可使用包名，或名包+类名
     */
    public void query(boolean ignoreClassInJdk, List<String> ignoreCallerClassPrefixList, List<String> ignoreCalleeClassPrefixList) {
        StringBuilder currentSimpleClassName = new StringBuilder();
        Set<String> ignoreCallerClassPrefixSet = new HashSet<>(ignoreCallerClassPrefixList == null ? Collections.emptyList() : ignoreCallerClassPrefixList);
        Set<String> ignoreCalleeClassPrefixSet = new HashSet<>(ignoreCalleeClassPrefixList == null ? Collections.emptyList() : ignoreCalleeClassPrefixList);
        if (ignoreClassInJdk) {
            ignoreCalleeClassPrefixSet.addAll(Arrays.asList(JavaCG2CommonNameConstants.PACKAGES_JDK));
        }
        String[] ignoreCallerClassPrefixes = ignoreCallerClassPrefixSet.toArray(new String[0]);
        String[] ignoreCalleeClassPrefixes = ignoreCalleeClassPrefixSet.toArray(new String[1]);

        QueryByPageHandler.queryAndHandle(this, 0, currentSimpleClassName, ignoreCallerClassPrefixes, ignoreCalleeClassPrefixes);
    }

    @Override
    public List<String> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        // 获取代表当前查询的被调用简单类名的参数
        StringBuilder currentSimpleClassName = (StringBuilder) argsByPage[0];

        List<String> list;
        int queryByPageSize = JACGConstants.DB_PAGE_HANDLE_SIZE;
        if (currentSimpleClassName.length() == 0) {
            // 首次查询
            String sql = "select distinct(" + DC.MC_CALLEE_SIMPLE_CLASS_NAME + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " limit ?";
            String finalSql = dbOperWrapper.formatSql(sql);
            list = dbOperator.queryListOneColumn(finalSql, String.class, queryByPageSize);
        } else {
            // 非首次查询
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLEE_SIMPLE_CLASS_NAME_BY_PAGE;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "select distinct(" + DC.MC_CALLEE_SIMPLE_CLASS_NAME + ")" +
                        " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                        " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " > ?" +
                        " order by " + DC.MC_CALLEE_SIMPLE_CLASS_NAME +
                        " limit ?";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }

            list = dbOperator.queryListOneColumn(sql, String.class, currentSimpleClassName.toString(), queryByPageSize);
        }

        currentSimpleClassName.setLength(0);
        if (!JavaCG2Util.isCollectionEmpty(list)) {
            // 查询结果非空，修改当前查询的被调用简单类名
            currentSimpleClassName.append(list.get(list.size() - 1));
        }
        return list;
    }

    @Override
    public boolean handleDataList(List<String> dataList, Object... argsByPage) throws Exception {
        String[] ignoreCallerClassPrefixes = (String[]) argsByPage[1];
        String[] ignoreCalleeClassPrefixes = (String[]) argsByPage[2];

        for (String calleeSimpleClassName : dataList) {
            // 查询被调用类完整类名
            String calleeClassName = methodCallHandler.queryCalleeClassNameBySimple(calleeSimpleClassName);
            // 判断是否需要跳过不处理的被调用类
            if (!StringUtils.startsWithAny(calleeClassName, ignoreCalleeClassPrefixes)) {
                // 处理一个被调用类
                handleOneCalleeClass(calleeClassName, calleeSimpleClassName);
            }
        }

        return true;
    }

    // 查询结果为空时需要结束循环查询
    @Override
    public boolean exitWhenQueryEmpty() {
        return true;
    }

    // 处理一个被调用类
    private void handleOneCalleeClass(String calleeClassName, String calleeSimpleClassName) {
        WriteDbData4ClassInfo classInfo = classInfoHandler.queryClassInfoBySCN(calleeSimpleClassName);
        if (classInfo == null) {
            logger.error("未找到被调用类 {}", calleeClassName);
            return;
        }

        // 查找指定被调用类的被调用方法，使用简单类名
        List<FullMethodWithReturnType> calleeMethodList = methodCallHandler.queryNormalCalleeMethodBySCN(calleeSimpleClassName);
        if (JavaCG2Util.isCollectionEmpty(calleeMethodList)) {
            logger.warn("未找到被调用方法 {}", calleeClassName);
            return;
        }
        for (FullMethodWithReturnType calleeMethod : calleeMethodList) {
            // 根据完整方法查询方法信息，若在当前类中未查找到，则在父类与接口中查找
            boolean calleeMethodExists = methodInfoHandler.checkExistsMethodByFullMethodSuperInterface(calleeMethod.getFullMethod(), calleeMethod.getReturnType());
            if (!calleeMethodExists) {
                logger.error("未找到被调用方法 {}", calleeMethod);
            }
        }

        // 处理被调用的重复类
        List<WriteDbData4ClassInfo> dupClassInfoList = classInfoHandler.queryDupClassInfoBySCN(calleeSimpleClassName);
        if (JavaCG2Util.isCollectionEmpty(dupClassInfoList)) {
            return;
        }
    }
}
