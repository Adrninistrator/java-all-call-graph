package com.adrninistrator.jacg.extensions.methodcall;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/2/22
 * @description: 补充 Apache Commons Chain 的方法调用
 * 为 org.apache.commons.chain.impl.ChainBase#addCommand(org.apache.commons.chain.Command) 方法补充被调用方法
 */
public class JACGApacheCommonsChainMethodCallExtension extends AbstractJACGMethodCallExtension {

    private static final Logger logger = LoggerFactory.getLogger(JACGObjInstanceMethodNameMCE.class);

    public static final String METHOD_NAME = "execute";
    public static final String ARG_TYPES = "(org.apache.commons.chain.Context)";

    private final MethodCallInfoHandler methodCallInfoHandler;

    public JACGApacheCommonsChainMethodCallExtension(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    @Override
    public boolean handle(WriteDbData4MethodCall methodCall) {
        if (!JavaCG2ApacheCommonsChainMethodCallExtension.TYPE.equals(methodCall.getCallType())) {
            // 返回未处理当前方法调用
            return false;
        }

        // 获取对应的原始方法调用ID
        int rawMethodCallId = Integer.parseInt(methodCall.getDescription());

        // 查询被调用对象类型列表
        List<String> calleeClassNameList = methodCallInfoHandler.queryMethodCallObjArgTypes(rawMethodCallId, 1);
        if (JavaCG2Util.isCollectionEmpty(calleeClassNameList)) {
            logger.warn("未查询到被调用对象类型 {}", rawMethodCallId);
        } else if (calleeClassNameList.size() > 1) {
            logger.warn("查询到被调用对象类型存在多种 {} {}", rawMethodCallId, StringUtils.join(calleeClassNameList, " "));
        }

        if (!JavaCG2Util.isCollectionEmpty(calleeClassNameList)) {
            String calleeClassName = calleeClassNameList.get(0);
            // 修改被调用方法
            String calleeSimpleClassName = dbOperWrapper.querySimpleClassName(calleeClassName);
            methodCall.setCalleeSimpleClassName(calleeSimpleClassName);
            methodCall.setCalleeMethodName(METHOD_NAME);
            String calleeFullMethod = JavaCG2ClassMethodUtil.formatFullMethod(calleeClassName, METHOD_NAME, ARG_TYPES);
            methodCall.setCalleeFullMethod(calleeFullMethod);
            methodCall.setCalleeMethodHash(JACGUtil.genHashWithLen(calleeFullMethod));
            methodCall.setRawReturnType(JavaCG2ConstantTypeEnum.CONSTTE_BOOLEAN.getType());
            methodCall.setCalleeJarNum(0);
        }

        // 修改方法调用类型及描述
        methodCall.setCallType(JavaCG2CallTypeEnum.CTE_MANUAL_ADDED.getType());
        methodCall.setDescription("补充 Apache Commons Chain 的方法调用 " + rawMethodCallId);
        return true;
    }
}