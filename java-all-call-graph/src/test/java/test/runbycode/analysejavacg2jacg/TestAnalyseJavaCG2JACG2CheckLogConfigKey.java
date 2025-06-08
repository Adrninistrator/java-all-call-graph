package test.runbycode.analysejavacg2jacg;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallStaticFieldMCR;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4MethodCallClassField;
import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallWithInfo;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.handler.methodcall.FindMethodCallByCallInfoHandler;
import com.adrninistrator.javacg2.conf.enums.interfaces.ConfigInterface;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/6/2
 * @description: 检查java-callgraph2、java-all-call-graph组件是否在日志中直接打印配置参数
 */
public class TestAnalyseJavaCG2JACG2CheckLogConfigKey {

    private static final Logger logger = LoggerFactory.getLogger(TestAnalyseJavaCG2JACG2CheckLogConfigKey.class);

    @Test
    public void test() {
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        Set<String> illegalCallerSet = new HashSet<>();
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper);
             FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler = new FindMethodCallByCallInfoHandler(configureWrapper)) {
            List<String> classNameList = jacgExtendsImplHandler.queryChildClassListByFull(ConfigInterface.class.toString(), false, true, false, true);
            for (String className : classNameList) {
                List<MethodCallWithInfo<BaseWriteDbData4MethodCallClassField>> methodCallWithInfoList = findMethodCallByCallInfoHandler.queryMethodCallByClassField4Method(true,
                        className, null, Logger.class.getName(), null);
                if (!JavaCG2Util.isCollectionEmpty(methodCallWithInfoList)) {
                    for (MethodCallWithInfo<BaseWriteDbData4MethodCallClassField> methodCallWithInfo : methodCallWithInfoList) {
                        WriteDbData4MethodCall methodCall = methodCallWithInfo.getMethodCall();
                        String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCallerFullMethod());
                        illegalCallerSet.add(callerClassName + ":" + methodCall.getCallerLineNumber());
                    }
                }
                List<MethodCallWithInfo<WriteDbData4MethodCallStaticFieldMCR>> methodCallWithInfoList2 =
                        findMethodCallByCallInfoHandler.queryMethodCallByStaticFieldMCR4Method(className, null, Logger.class.getName(), null);
                if (!JavaCG2Util.isCollectionEmpty(methodCallWithInfoList2)) {
                    for (MethodCallWithInfo<WriteDbData4MethodCallStaticFieldMCR> methodCallWithInfo : methodCallWithInfoList2) {
                        WriteDbData4MethodCall methodCall = methodCallWithInfo.getMethodCall();
                        String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCallerFullMethod());
                        illegalCallerSet.add(callerClassName + ":" + methodCall.getCallerLineNumber());
                    }
                }
            }
        }
        if (illegalCallerSet.isEmpty()) {
            logger.info("检查通过");
            return;
        }
        List<String> illegalCallerList = new ArrayList<>(illegalCallerSet);
        Collections.sort(illegalCallerList);
        logger.error("检查不通过，数量 {}\n{}", illegalCallerList.size(), StringUtils.join(illegalCallerList, "\n"));
        Assert.fail("检查不通过");
    }
}
