package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2024/5/16
 * @description: 写数据库的类所写的文件枚举
 */
public enum WriteDbHandlerWriteFileEnum {
    WDHWFE_MYBATIS_MS_GET_SET("mybatis_ms_get_set"),
    WDHWFE_SET_METHOD_ASSIGN_INFO("set_method_assign_info"),
    WDHWFE_SPRING_CONTROLLER("spring_controller"),
    WDHWFE_SPRING_TASK_ANNOTATION("spring_task_annotation"),
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
