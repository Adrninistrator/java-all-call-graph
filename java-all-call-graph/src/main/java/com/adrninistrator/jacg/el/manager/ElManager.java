package com.adrninistrator.jacg.el.manager;

import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.el.enums.ElAllowedVariableEnum;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.conf.BaseConfigureWrapper;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.el.handler.ElHandler;
import com.adrninistrator.javacg2.el.manager.CommonElManager;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/8/21
 * @description: 表达式管理类
 */
public class ElManager extends CommonElManager {

    private static final Logger logger = LoggerFactory.getLogger(ElManager.class);

    public ElManager(BaseConfigureWrapper configureWrapper, ElConfigInterface[] elConfigInterfaces, String outputDirPath) {
        super(configureWrapper, elConfigInterfaces, outputDirPath);
    }

    @Override
    protected boolean chooseDebugMode(BaseConfigureWrapper configureWrapper) {
        return configureWrapper.getMainConfig(ConfigKeyEnum.CKE_EL_DEBUG_MODE);
    }

    /**
     * 检查是否需要跳过记录方法调用
     *
     * @param methodCallType   方法调用类型
     * @param callerFullMethod 调用方完整方法
     * @param calleeFullMethod 被调用方完整方法
     * @param methodCallFlags  方法调用标志
     * @return
     */
    public boolean checkIgnoreMethodCall(String methodCallType, String callerFullMethod, String calleeFullMethod, int methodCallFlags) {
        ElHandler elHandler = getElHandlerMap(ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        methodCallAddData4Type(elHandler, methodCallType, usedVariableMap, displayMap);
        methodCallAddData4Caller(elHandler, callerFullMethod, usedVariableMap, displayMap);
        methodCallAddData4Callee(elHandler, calleeFullMethod, usedVariableMap, displayMap);
        List<String> methodCallFlagEnumNameList = MethodCallFlagsEnum.getAllEnumNames(methodCallFlags);
        methodCallAddData4Flags(elHandler, methodCallFlagEnumNameList, usedVariableMap, displayMap);

        if (logger.isDebugEnabled()) {
            logger.debug("方法调用标志 callerFullMethod {} calleeFullMethod {} methodCallFlags {} methodCallFlagEnumNameList {}", callerFullMethod, calleeFullMethod, methodCallFlags,
                    StringUtils.join(methodCallFlagEnumNameList, " "));
        }
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 为方法调用添加数据，标志
     *
     * @param elHandler
     * @param methodCallFlagEnumNameList
     * @param usedVariableMap
     * @param displayMap
     * @return true: 有使用对应表达式变量 false: 未使用对应表达式变量
     */
    private boolean methodCallAddData4Flags(ElHandler elHandler, List<String> methodCallFlagEnumNameList, Map<String, Object> usedVariableMap, Map<String, Object> displayMap) {
        displayMap.put(ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM.getVariableName(), StringUtils.join(methodCallFlagEnumNameList, " "));
        if (elHandler.checkVariableNameSpecified(ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM)) {
            usedVariableMap.put(ElAllowedVariableEnum.EAVE_MC_FLAGS_ENUM.getVariableName(), methodCallFlagEnumNameList);
            return true;
        }
        return false;
    }

    /**
     * 检查是否需要在解析Spring AOP影响方法时跳过Spring Bean
     *
     * @param springBeanClassName
     * @return
     */
    public boolean checkIgnoreSpringBean4AOP(String springBeanClassName) {
        ElHandler elHandler = getElHandlerMap(ElConfigEnum.ECE_SPRING_AOP_IGNORE_SPRING_BEAN_CLASS);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        addData4Class(elHandler, usedVariableMap, displayMap, springBeanClassName);
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * JarDiff生成方法完整调用链时判断是否需要跳过方法
     *
     * @param genCalleeGraph 生成向上的完整方法调用链还是生成向下的
     * @param fullMethod     完整方法
     * @return
     */
    public boolean checkJarDiffGenAllCallGraphIgnore(boolean genCalleeGraph, String fullMethod) {
        ElConfigEnum elConfigEnum = genCalleeGraph ? ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLEE : ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLER;
        ElHandler elHandler = getElHandlerMap(elConfigEnum);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        addData4Method(elHandler, usedVariableMap, displayMap, fullMethod);
        return elHandler.runExpression(usedVariableMap, displayMap);
    }
}
