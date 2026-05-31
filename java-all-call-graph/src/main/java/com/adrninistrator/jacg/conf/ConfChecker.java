package com.adrninistrator.jacg.conf;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.exceptions.JavaCG2ConfigException;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2026/5/24
 * @description: 配置参数检查类
 */
public class ConfChecker {

    private static final Logger logger = LoggerFactory.getLogger(ConfChecker.class);

    /**
     * 检查所有的配置参数
     *
     * @param configureWrapper
     */
    public static void checkAll(ConfigureWrapper configureWrapper) {
        cheCallGraphFileOrMemory(configureWrapper);
        checkCalleeArgTypePolymorphism(configureWrapper);
    }

    /**
     * 检查生成调用链时生成文件或在内存返回配置参数
     *
     * @param configureWrapper
     */
    public static void cheCallGraphFileOrMemory(ConfigureWrapper configureWrapper) {
        boolean callGraphWriteToFile = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE);
        boolean callGraphReturnInMemory = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY);
        if (!callGraphWriteToFile && !callGraphReturnInMemory) {
            String errorMsg = "是否将生成的调用链数据写入文件的开关，与是否将生成的调用链数据在内存中返回的开关，不允许都设置为true" +
                    ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE.getFileName() + " " + ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE.getConfigPrintInfo() + " " + ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY.getConfigPrintInfo();
            logger.error(errorMsg);
            throw new JavaCG2ConfigException(errorMsg);
        }
    }

    /**
     * 检查方法参数作为被调用对象涉及多态时的类型替换时使用的配置
     * @param configureWrapper
     */
    public static void checkCalleeArgTypePolymorphism(ConfigureWrapper configureWrapper) {
        Set<String> configSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM);
        if (JavaCG2Util.isCollectionEmpty(configSet)) {
            return;
        }
        String configDesc = " " + OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM.getConfigPrintInfo() + " ";
        // 处理指定的配置
        for (String config : configSet) {
            String[] array = StringUtils.splitPreserveAllTokens(config, JavaCG2Constants.FLAG_EQUAL);
            if (array.length != 2) {
                String errorMsg = "配置参数非法，不是 xxx=yyy 格式" + configDesc + config;
                logger.error(errorMsg);
                throw new JavaCG2ConfigException(errorMsg);
            }
            if (!JavaCG2Util.isNumStr(array[1])) {
                String errorMsg = "配置参数非法，不是合法的参数序号" + configDesc + config;
                logger.error(errorMsg);
                throw new JavaCG2ConfigException(errorMsg);
            }
            int argSeq = Integer.parseInt(array[1]);
            if (argSeq < JavaCG2Constants.METHOD_CALL_ARGUMENTS_START_SEQ) {
                String errorMsg = "配置参数非法，参数序号应大于等于 " + JavaCG2Constants.METHOD_CALL_ARGUMENTS_START_SEQ + configDesc + config;
                logger.error(errorMsg);
                throw new JavaCG2ConfigException(errorMsg);
            }
        }
    }

    private ConfChecker() {
        throw new IllegalStateException("illegal");
    }
}
