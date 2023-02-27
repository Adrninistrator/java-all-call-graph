package com.adrninistrator.jacg.handler.lambda;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.lambda.LambdaMethodCall;
import com.adrninistrator.jacg.dto.lambda.LambdaMethodCallDetail;
import com.adrninistrator.jacg.util.JACGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/13
 * @description: Lambda表达式方法相关信息查询处理类，通过类名前缀查询
 */
public class LambdaMethodHandlerByClassNamePrefix extends BaseLambdaMethodHandler {
    private static final Logger logger = LoggerFactory.getLogger(LambdaMethodHandlerByClassNamePrefix.class);

    public LambdaMethodHandlerByClassNamePrefix(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    @Override
    protected List<Map<String, Object>> queryByPage(int startCallId, int endCallId, Object... args) {
        String lambdaCalleeClassNamePrefix = JACGUtil.getArgAt(0, args);
        String lambdaNextCalleeClassNamePrefix = JACGUtil.getArgAt(1, args);
        boolean lastQuery = (endCallId == JACGConstants.PAGE_QUERY_LAST);
        logger.info("通过类名前缀查询 {} {} {} {} {}", lastQuery ? "最后一次分页查询" : "非最后一次分页查询", startCallId, endCallId, lambdaCalleeClassNamePrefix, lambdaNextCalleeClassNamePrefix);

        // 生成查询使用的sql语句
        String sql = genQuerySql(lastQuery);
        List<Object> argList = new ArrayList<>();
        argList.add(startCallId);
        if (!lastQuery) {
            // 非最后一次分页查询
            argList.add(endCallId);
        }

        if (lambdaCalleeClassNamePrefix != null) {
            // 查询被调用的Lambda表达式类名前缀满足的记录
            sql = sql + " and lmi." + DC.LMI_LAMBDA_CALLEE_CLASS_NAME + " like concat(?,'%')";
            argList.add(lambdaCalleeClassNamePrefix);
        }

        if (lambdaNextCalleeClassNamePrefix != null) {
            // 根据Lambda表达式下一个被调用方类名前缀查询
            sql = sql + " and lmi." + DC.LMI_LAMBDA_NEXT_CLASS_NAME + " like concat(?,'%')";
            argList.add(lambdaNextCalleeClassNamePrefix);
        }
        return dbOperator.queryList(sql, argList.toArray());
    }

    /**
     * 通过类名前缀查询Lambda表达式方法调用信息
     *
     * @param lambdaCalleeClassNamePrefix     Lambda表达式被调用方类名前缀
     * @param lambdaNextCalleeClassNamePrefix Lambda表达式下一个被调用方类名前缀
     * @return
     */
    public List<LambdaMethodCall> queryByClassNamePrefix(String lambdaCalleeClassNamePrefix, String lambdaNextCalleeClassNamePrefix) {
        logger.info("通过类名前缀查询Lambda表达式方法调用信息 {} {}", lambdaCalleeClassNamePrefix, lambdaNextCalleeClassNamePrefix);
        // 执行查询操作
        return query(lambdaCalleeClassNamePrefix, lambdaNextCalleeClassNamePrefix);
    }

    /**
     * 通过类名前缀查询Lambda表达式方法调用信息，包含各方法的详细信息
     *
     * @param lambdaCalleeClassNamePrefix     Lambda表达式被调用方类名前缀
     * @param lambdaNextCalleeClassNamePrefix Lambda表达式下一个被调用方类名前缀
     * @return
     */
    public List<LambdaMethodCallDetail> queryByClassNamePrefixDetail(String lambdaCalleeClassNamePrefix, String lambdaNextCalleeClassNamePrefix) {
        // 执行查询操作
        List<LambdaMethodCall> list = queryByClassNamePrefix(lambdaCalleeClassNamePrefix, lambdaNextCalleeClassNamePrefix);
        return genDetailList(list);
    }
}
