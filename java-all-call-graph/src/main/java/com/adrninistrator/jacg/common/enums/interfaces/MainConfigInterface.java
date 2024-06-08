package com.adrninistrator.jacg.common.enums.interfaces;

/**
 * @author adrninistrator
 * @date 2023/3/27
 * @description: 主要配置文件继承的接口
 */
public interface MainConfigInterface extends ConfigInterface {
    // 字段名称
    String getFileName();

    // 字段类型
    Class<?> getType();

    // 是否不允许为空
    boolean notBlank();
}
