package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/8/10
 * @description: 生成Java方法调用关系文件并写入数据库，特殊解析模式
 */
public abstract class AbstractRunnerWriteDbSpecialMode extends RunnerWriteDb {

    private static final Logger logger = LoggerFactory.getLogger(AbstractRunnerWriteDbSpecialMode.class);

    private final List<String> jarDirPathList;
    private final String dbShowInfo;

    public AbstractRunnerWriteDbSpecialMode(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper) {
        super(javaCG2ConfigureWrapper, configureWrapper);
        jarDirPathList = javaCG2ConfigureWrapper.getOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR);
        if (JavaCG2Util.isCollectionEmpty(jarDirPathList)) {
            logger.error("需要配置参数 {}", javaCG2ConfigureWrapper.genConfigUsage(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR));
            throw new RuntimeException("配置参数不完整");
        }
        dbShowInfo = dbOperator.getDbConfInfo().getShowDbInfo();
    }

    // 结束前的处理
    @Override
    protected void beforeExit() {
        super.beforeExit();

        if (!someTaskFail) {
            logger.info("解析了以下指定的jar文件或目录 {} 使用的数据库为 {}", StringUtils.join(jarDirPathList, " "), dbShowInfo);
        }
    }
}
