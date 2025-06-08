package com.adrninistrator.jacg.jardiff.runner;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.jardiff.dto.method.ModifiedMethodInfo;
import com.adrninistrator.jacg.jardiff.filter.ModifiedMethodFilterInterface;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/5/17
 * @description: 比较新旧两个目录中不同版本jar文件的方法修改情况，以及向下的完整方法调用链
 */
public class RunnerGenJarDiffCallerGraph extends AbstractRunnerGenJarDiffCallGraph {

    public RunnerGenJarDiffCallerGraph(ModifiedMethodFilterInterface... modifiedMethodFilters) {
        this(new JavaCG2ConfigureWrapper(false), new ConfigureWrapper(false), modifiedMethodFilters);
    }

    public RunnerGenJarDiffCallerGraph(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper, ModifiedMethodFilterInterface... modifiedMethodFilters) {
        super(javaCG2ConfigureWrapper, configureWrapper, modifiedMethodFilters);
    }

    // 处理发生变化的jar文件与方法
    @Override
    protected boolean handleModifiedJarAndMethods(ConfigureWrapper configureWrapperNew, Map<String, WriteDbData4JarInfo> modifiedJarMap,
                                                  Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap, Map<String, String> modifiedClassJarMap,
                                                  Set<String> modifiedMethodSet, List<String> jarFileNameListNew) {
        configureWrapperNew.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_0.getDetail());
        configureWrapperNew.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, modifiedMethodSet);
        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller(configureWrapperNew);
        // 生成发生变化的方法向下的方法完整调用调用链
        boolean success = runnerGenAllGraph4Caller.run();
        if (!success) {
            return false;
        }

        // 在本次生成目录的根目录创建目录，作为输出以下文件的目录
        String jarDiffDirPath = runnerGenAllGraph4Caller.getCurrentOutputDirPath() + File.separator + JACGConstants.DIR_CALLER_JAR_DIFF_SUMMARY;
        if (!JavaCG2FileUtil.isDirectoryExists(jarDiffDirPath)) {
            return false;
        }
        // 生成jar文件中发生变化的方法基本信息
        return writeModifiedMethodsBaseFile(jarDiffDirPath, jarFileNameListNew, modifiedJarMap, jarModifiedMethodInfoMap);
    }

    @Override
    protected OtherConfigFileUseListEnum chooseCompareDirEnum() {
        return OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_CALLER_GRAPH_DIR;
    }
}
