package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2024/5/16
 * @description: 写数据库的类所写的文件枚举
 * 仅记录"加工生成"的文件（readFile=false 的 WriteDbHandler 在处理过程中自行写出的文件），
 * 不包含 JavaCG2Entry 直接生成的中间文件（readFile=true 的 Handler 读取的文件不在此枚举中）
 */
public enum WriteDbHandlerWriteFileEnum {
    WDHWFE_MYBATIS_MS_GET_SET("mybatis_ms_get_set"),
    WDHWFE_SET_METHOD_ASSIGN_INFO("set_method_assign_info"),
    WDHWFE_SPRING_CONTROLLER("spring_controller"),
    WDHWFE_SPRING_TASK_JAVA("spring_task_java"),
    WDHWFE_SPRING_AOP_ADVICE_AROUND("spring_aop_advice_around"),
    WDHWFE_SPRING_AOP_ADVICE_AFFECTED_METHOD("spring_aop_advice_affected_method"),
    WDHWFE_NONE("/none\\"),
    ;

    WriteDbHandlerWriteFileEnum(String name) {
        this.name = name;
    }

    private final String name;

    public String getName() {
        return name;
    }
}
