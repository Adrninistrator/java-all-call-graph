package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.base.AbstractRunnerWriteDbSpecialMode;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;

/**
 * @author adrninistrator
 * @date 2025/8/10
 * @description: 使用Jar兼容性检查模式，仅解析类相关信息并写入数据库，写入H2数据库文件，app.name固定为jacg
 */
public class RunnerWriteDbOnlyClassMode extends AbstractRunnerWriteDbSpecialMode {

    /**
     * 构造函数，使用配置文件中的参数
     */
    public RunnerWriteDbOnlyClassMode() {
        this(new JavaCG2ConfigureWrapper(false), new ConfigureWrapper(false));
    }

    /**
     * 构造函数，使用代码中指定的参数
     *
     * @param javaCG2ConfigureWrapper
     * @param configureWrapper
     */
    public RunnerWriteDbOnlyClassMode(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper) {
        super(javaCG2ConfigureWrapper, configureWrapper);
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_ONLY_CLASS_MODE, Boolean.TRUE.toString());
    }

    /**
     * 对当前使用的配置进行自定义设置，适用于在构造函数中需要修改的场景
     *
     * @param configureWrapper
     */
    @Override
    protected void configConfigureWrapper(ConfigureWrapper configureWrapper) {
        // 使用固定app.name的H2数据库文件
        configureWrapper.useFixedAppNameH2Db();
    }
}
