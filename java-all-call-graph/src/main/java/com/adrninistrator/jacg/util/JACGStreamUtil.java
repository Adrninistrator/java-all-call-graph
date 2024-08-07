package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2023/1/12
 * @description: Stream工具类
 */
public class JACGStreamUtil {
    /**
     * 判断是否为Stream相关类
     *
     * @param className
     * @return
     */
    public static boolean isStreamClass(String className) {
        return StringUtils.equalsAny(className, JACGCommonNameConstants.STREAM_CLASS_NAMES);
    }

    /**
     * 判断是否为Stream的intermediate（中间）操作
     *
     * @param methodName
     * @return
     */
    public static boolean isStreamIntermediateMethod(String methodName) {
        return StringUtils.equalsAny(methodName, JACGCommonNameConstants.STREAM_INTERMEDIATE_METHOD_NAMES);
    }

    /**
     * 判断是否为Stream的terminal（终端）操作
     *
     * @param methodName
     * @return
     */
    public static boolean isStreamTerminalMethod(String methodName) {
        return StringUtils.equalsAny(methodName, JACGCommonNameConstants.STREAM_TERMINAL_METHOD_NAMES);
    }

    private JACGStreamUtil() {
        throw new IllegalStateException("illegal");
    }
}
