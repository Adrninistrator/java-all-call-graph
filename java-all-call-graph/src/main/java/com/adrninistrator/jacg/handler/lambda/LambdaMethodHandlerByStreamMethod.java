package com.adrninistrator.jacg.handler.lambda;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.lambda.LambdaMethodCall;
import com.adrninistrator.jacg.dto.lambda.LambdaMethodCallDetail;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/13
 * @description: Lambda表达式方法相关信息查询处理类，通过Stream条件查询
 */
public class LambdaMethodHandlerByStreamMethod extends BaseLambdaMethodPageHandler {
    private static final Logger logger = LoggerFactory.getLogger(LambdaMethodHandlerByStreamMethod.class);

    public LambdaMethodHandlerByStreamMethod(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    @Override
    public List<LambdaMethodCall> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        Boolean lambdaNextIsStream = JACGUtil.getArgAt(0, argsByPage);
        Boolean lambdaNextIsIntermediate = JACGUtil.getArgAt(1, argsByPage);
        logger.info("通过Stream条件查询 {} {} {} {} {}", lastQuery ? "最后一次分页查询" : "非最后一次分页查询", currentStartId, currentEndId, lambdaNextIsStream, lambdaNextIsIntermediate);

        // 生成查询使用的sql语句
        String sql = genQuerySql(lastQuery);
        List<Object> argList = new ArrayList<>();
        argList.add(currentStartId);
        if (!lastQuery) {
            // 非最后一次分页查询
            argList.add(currentEndId);
        }

        if (lambdaNextIsStream != null) {
            sql = sql + " and lmi." + DC.LMI_LAMBDA_NEXT_IS_STREAM + " = ?";
            argList.add(JavaCG2YesNoEnum.parseIntValue(lambdaNextIsStream));
        }
        if (Boolean.TRUE.equals(lambdaNextIsIntermediate)) {
            sql = sql + " and lmi." + DC.LMI_LAMBDA_NEXT_IS_INTERMEDIATE + " = ?";
            argList.add(JavaCG2YesNoEnum.YES.getStrValue());
        }
        if (Boolean.FALSE.equals(lambdaNextIsIntermediate)) {
            sql = sql + " and lmi." + DC.LMI_LAMBDA_NEXT_IS_TERMINAL + " = ?";
            argList.add(JavaCG2YesNoEnum.YES.getStrValue());
        }
        return dbOperator.queryList(sql, LambdaMethodCall.class, argList.toArray());
    }

    /**
     * 通过Stream条件查询Lambda表达式方法调用信息
     *
     * @param lambdaNextIsStream       下一个被调用方法是否为Stream，若为null则查询全部数据
     * @param lambdaNextIsIntermediate true: 下一个被调用方法为Stream的intermediate（中间）操作 false: 下一个被调用方法为Stream的terminal（终端）操作 仅当参数1非null时当前参数可为非null
     * @return
     */
    public List<LambdaMethodCall> queryByStreamMethod(Boolean lambdaNextIsStream, Boolean lambdaNextIsIntermediate) {
        if (!Boolean.TRUE.equals(lambdaNextIsStream) && (lambdaNextIsIntermediate != null)) {
            throw new JavaCG2RuntimeException("仅当参数1为true时，参数2允许为非null");
        }

        logger.info("通过Stream条件查询Lambda表达式方法调用信息 {} {}", lambdaNextIsStream, lambdaNextIsIntermediate);
        // 分页查询，结果合并到List中
        return QueryByPageHandler.queryAll2List(this, JavaCG2Constants.METHOD_CALL_ID_MIN_BEFORE, lambdaNextIsStream, lambdaNextIsIntermediate);
    }

    /**
     * 通过Stream条件查询Lambda表达式方法调用信息，包含各方法的详细信息
     *
     * @param lambdaNextIsStream       下一个被调用方法是否为Stream，若为null则查询全部数据
     * @param lambdaNextIsIntermediate true: 下一个被调用方法为Stream的intermediate（中间）操作 false: 下一个被调用方法为Stream的terminal（终端）操作 仅当参数1非null时当前参数可为非null
     * @return
     */
    public List<LambdaMethodCallDetail> queryByStreamMethodDetail(Boolean lambdaNextIsStream, Boolean lambdaNextIsIntermediate) {
        // 执行查询操作
        List<LambdaMethodCall> list = queryByStreamMethod(lambdaNextIsStream, lambdaNextIsIntermediate);
        return genDetailList(list);
    }
}
