package com.adrninistrator.jacg.common.enums.interfaces;

/**
 * @author adrninistrator
 * @date 2023/3/27
 * @description: 主要配置文件继承的接口
 */
public interface MainConfigInterface extends ConfigInterface {
    String getFileName();

    Class<?> getType();
}
