package com.adrninistrator.jacg.el.constants;

import com.adrninistrator.jacg.common.enums.InputDirEnum;

import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/9/27
 * @description:
 */
public class ElConstants {

    private static final Map<String, String[]> EL_DIR_USAGE_MAP = new HashMap<>();

    static {
        EL_DIR_USAGE_MAP.put(InputDirEnum.IDE_GEN_ALL_CALL_GRAPH.getDirName(),
                new String[]{"在 生成方法完整调用链时 使用的配置参数",
                        "表达式配置参数的说明如下：",
                        "若当前配置文件中的表达式执行结果为 true，则跳过在方法完整调用链中生成对应的方法",
                        "若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的方法"});
        EL_DIR_USAGE_MAP.put(InputDirEnum.IDE_SPRING_AOP.getDirName(),
                new String[]{"在 解析Spring AOP影响方法时 使用的配置参数",
                        "表达式配置参数的说明如下：",
                        "若当前配置文件中的表达式执行结果为 true，则跳过处理对应的Spring Bean",
                        "若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的Spring Bean"});
        EL_DIR_USAGE_MAP.put(InputDirEnum.IDE_JAR_DIFF.getDirName(),
                new String[]{"在 对JarDiff找到发生变化的方法生成向上/向下完整方法调用链 使用的配置参数",
                        "表达式配置参数的说明如下：",
                        "若当前配置文件中的表达式执行结果为 true，则跳过为对应的方法生成完整方法调用链",
                        "若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的方法"});
    }

    public static Map<String, String[]> getElDirUsageMap() {
        return EL_DIR_USAGE_MAP;
    }

    private ElConstants() {
        throw new IllegalStateException("illegal");
    }
}
