package com.adrninistrator.jacg.handler.lambda;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.lambda.LambdaMethodCall;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2Util;
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
        List<LambdaMethodCall> list = queryDetailByLambdaCallee(lambdaCalleeClassName, lambdaCalleeMethodName);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }

        List<String> calleeFullMethodList = new ArrayList<>(list.size());
        for (LambdaMethodCall lambdaMethodCall : list) {
            calleeFullMethodList.add(lambdaMethodCall.getCalleeFullMethod());
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
    public List<LambdaMethodCall> queryDetailByLambdaCallee(String lambdaCalleeClassName, String lambdaCalleeMethodName) {
        if (StringUtils.isAnyBlank(lambdaCalleeClassName, lambdaCalleeMethodName)) {
            throw new JavaCG2RuntimeException("参数不允许为空");
        }

        logger.info("通过Lambda表达式被调用方类名、方法名，查询Lambda表达式方法调用信息 {} {}", lambdaCalleeClassName, lambdaCalleeMethodName);
        // 执行查询操作
        List<LambdaMethodCall> list = queryByClassNamePrefix(lambdaCalleeClassName, null);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }

        List<LambdaMethodCall> newList = new ArrayList<>(list.size());
        for (LambdaMethodCall lambdaMethodCall : list) {
            if (lambdaMethodCall.getLambdaCalleeFullMethod() != null) {
                MethodDetailNoReturnType lambdaCalleeFullMethodDetailNoReturnType = JACGClassMethodUtil.genMethodDetailNoReturnType(lambdaMethodCall.getLambdaCalleeFullMethod());
                if (StringUtils.equals(lambdaCalleeFullMethodDetailNoReturnType.getClassName(), lambdaCalleeClassName) &&
                        StringUtils.equals(lambdaCalleeFullMethodDetailNoReturnType.getMethodName(), lambdaCalleeMethodName)) {
                    newList.add(lambdaMethodCall);
                }
            }
        }
        return newList;
    }

    /**
     * 通过Lambda表达式下一个被调用方类名、方法名查询Lambda表达式方法调用信息，包含各方法的详细信息
     *
     * @param lambdaNextClassName  Lambda表达式下一个被调用方类名
     * @param lambdaNextMethodName Lambda表达式下一个被调用方方法名
     * @return
     */
    public List<LambdaMethodCall> queryDetailByLambdaNext(String lambdaNextClassName, String lambdaNextMethodName) {
        if (StringUtils.isAnyBlank(lambdaNextClassName, lambdaNextMethodName)) {
            throw new JavaCG2RuntimeException("参数不允许为空");
        }

        logger.info("通过Lambda表达式下一个被调用方类名、方法名，查询Lambda表达式方法调用信息 {} {}", lambdaNextClassName, lambdaNextMethodName);
        // 执行查询操作
        List<LambdaMethodCall> list = queryByClassNamePrefix(null, lambdaNextClassName);
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }

        List<LambdaMethodCall> newList = new ArrayList<>(list.size());
        for (LambdaMethodCall lambdaMethodCall : list) {
            if (lambdaMethodCall.getLambdaNextFullMethod() != null) {
                MethodDetailNoReturnType lambdaNextFullMethodDetailNoReturnType = JACGClassMethodUtil.genMethodDetailNoReturnType(lambdaMethodCall.getLambdaNextFullMethod());
                if (StringUtils.equals(lambdaNextFullMethodDetailNoReturnType.getClassName(), lambdaNextClassName) &&
                        StringUtils.equals(lambdaNextFullMethodDetailNoReturnType.getMethodName(), lambdaNextMethodName)) {
                    newList.add(lambdaMethodCall);
                }
            }
        }
        return newList;
    }
}
