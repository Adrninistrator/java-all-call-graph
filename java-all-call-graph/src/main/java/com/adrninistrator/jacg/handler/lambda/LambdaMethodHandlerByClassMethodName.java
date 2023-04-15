package com.adrninistrator.jacg.handler.lambda;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.lambda.LambdaMethodCallDetail;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/2/15
 * @description: Lambda表达式方法相关信息查询处理类，通过类名与方法名查询
 */
public class LambdaMethodHandlerByClassMethodName extends LambdaMethodHandlerByClassNamePrefix {
    private static final Logger logger = LoggerFactory.getLogger(LambdaMethodHandlerByClassMethodName.class);

    public LambdaMethodHandlerByClassMethodName(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    /**
     * 通过Lambda表达式被调用方类名、方法名，查询Lambda表达式中实际被调用的方法
     *
     * @param lambdaCalleeClassName  Lambda表达式被调用方类名
     * @param lambdaCalleeMethodName Lambda表达式被调用方方法名
     * @return
     */
    public List<String> queryCalleeFullMethodByLambdaCallee(String lambdaCalleeClassName, String lambdaCalleeMethodName) {
        List<LambdaMethodCallDetail> list = queryDetailByLambdaCallee(lambdaCalleeClassName, lambdaCalleeMethodName);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }

        List<String> calleeFullMethodList = new ArrayList<>(list.size());
        for (LambdaMethodCallDetail lambdaMethodCallDetail : list) {
            calleeFullMethodList.add(lambdaMethodCallDetail.getCalleeFullMethod());
        }
        return calleeFullMethodList;
    }

    /**
     * 通过Lambda表达式被调用方类名、方法名，查询Lambda表达式方法调用信息，包含各方法的详细信息
     *
     * @param lambdaCalleeClassName  Lambda表达式被调用方类名
     * @param lambdaCalleeMethodName Lambda表达式被调用方方法名
     * @return
     */
    public List<LambdaMethodCallDetail> queryDetailByLambdaCallee(String lambdaCalleeClassName, String lambdaCalleeMethodName) {
        if (StringUtils.isAnyBlank(lambdaCalleeClassName, lambdaCalleeMethodName)) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        logger.info("通过Lambda表达式被调用方类名、方法名，查询Lambda表达式方法调用信息 {} {}", lambdaCalleeClassName, lambdaCalleeMethodName);
        // 执行查询操作
        List<LambdaMethodCallDetail> list = queryByClassNamePrefixDetail(lambdaCalleeClassName, null);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }

        List<LambdaMethodCallDetail> newList = new ArrayList<>(list.size());
        for (LambdaMethodCallDetail lambdaMethodCallDetail : list) {
            MethodDetail lambdaCalleeFullMethodDetail = lambdaMethodCallDetail.getLambdaCalleeFullMethodDetail();
            if (lambdaCalleeFullMethodDetail != null &&
                    StringUtils.equals(lambdaCalleeFullMethodDetail.getClassName(), lambdaCalleeClassName) &&
                    StringUtils.equals(lambdaCalleeFullMethodDetail.getMethodName(), lambdaCalleeMethodName)) {
                newList.add(lambdaMethodCallDetail);
            }
        }
        return newList;
    }

    /**
     * 通过Lambda表达式下一个被调用方类名、方法名查询Lambda表达式方法调用信息，包含各方法的详细信息
     *
     * @param lambdaNextCalleeClassName  Lambda表达式下一个被调用方类名
     * @param lambdaNextCalleeMethodName Lambda表达式下一个被调用方方法名
     * @return
     */
    public List<LambdaMethodCallDetail> queryDetailByLambdaNextCallee(String lambdaNextCalleeClassName, String lambdaNextCalleeMethodName) {
        if (StringUtils.isAnyBlank(lambdaNextCalleeClassName, lambdaNextCalleeMethodName)) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        logger.info("通过Lambda表达式下一个被调用方类名、方法名，查询Lambda表达式方法调用信息 {} {}", lambdaNextCalleeClassName, lambdaNextCalleeMethodName);
        // 执行查询操作
        List<LambdaMethodCallDetail> list = queryByClassNamePrefixDetail(null, lambdaNextCalleeClassName);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }

        List<LambdaMethodCallDetail> newList = new ArrayList<>(list.size());
        for (LambdaMethodCallDetail lambdaMethodCallDetail : list) {
            MethodDetail lambdaNextCalleeFullMethodDetail = lambdaMethodCallDetail.getLambdaNextCalleeFullMethodDetail();
            if (lambdaNextCalleeFullMethodDetail != null &&
                    StringUtils.equals(lambdaNextCalleeFullMethodDetail.getClassName(), lambdaNextCalleeClassName) &&
                    StringUtils.equals(lambdaNextCalleeFullMethodDetail.getMethodName(), lambdaNextCalleeMethodName)) {
                newList.add(lambdaMethodCallDetail);
            }
        }
        return newList;
    }
}
