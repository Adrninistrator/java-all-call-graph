package com.adrninistrator.jacg.compatibility.runner;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.compatibility.handler.CalleeMethodCheckHandler;
import com.adrninistrator.jacg.compatibility.handler.DupClassCheckHandler;
import com.adrninistrator.jacg.compatibility.handler.UsedOtherFieldCheckHandler;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.runner.base.AbstractRunner;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2024/12/5
 * @description: Jar兼容性检查入口类
 */
public class RunnerJarCompatibilityCheck extends AbstractRunner {

    /*
          jar文件信息Map
          key jar文件序号
          value   jar文件信息
       */
    protected final Map<Integer, WriteDbData4JarInfo> jarInfoMap = new HashMap<>();

    protected List<DbOperWrapper> otherDbOperWrapperList;

    public RunnerJarCompatibilityCheck(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    @Override
    protected boolean preHandle() {
        // 创建保存输出文件的目录
        if (!createOutputDir(JACGConstants.DIR_OUTPUT_JAR_COMPATIBILITY)) {
            return false;
        }

        // 生成数据库包装类列表
        otherDbOperWrapperList = genOtherDbOperWrapperList();
        return true;
    }

    @Override
    protected void handle() {
        // 查询解析的jar文件信息
        queryJarInfoMap();

        try (DupClassCheckHandler dupClassCheckHandler = new DupClassCheckHandler(configureWrapper);
             CalleeMethodCheckHandler calleeMethodCheckHandler = new CalleeMethodCheckHandler(configureWrapper);
             UsedOtherFieldCheckHandler usedOtherFieldCheckHandler = new UsedOtherFieldCheckHandler(configureWrapper)) {
            dupClassCheckHandler.setCurrentOutputDirPath(currentOutputDirPath);
            dupClassCheckHandler.setJarInfoMap(jarInfoMap);
            // 检查重复同名类
            if (!dupClassCheckHandler.check()) {
                recordTaskFail();
                return;
            }

            /*
                保存类名是否存在
                key 类名
                value   是否存在
             */
            Map<String, Boolean> classNameExistsMap = new HashMap<>();

            // 检查方法调用中的调用方法
            calleeMethodCheckHandler.setOtherDbOperWrapperList(otherDbOperWrapperList);
            calleeMethodCheckHandler.setClassNameExistsMap(classNameExistsMap);
            calleeMethodCheckHandler.setCurrentOutputDirPath(currentOutputDirPath);
            calleeMethodCheckHandler.setJarInfoMap(jarInfoMap);
            calleeMethodCheckHandler.init();
            if (!calleeMethodCheckHandler.check()) {
                recordTaskFail();
                return;
            }

            // 检查被其他类使用的字段
            usedOtherFieldCheckHandler.setOtherDbOperWrapperList(otherDbOperWrapperList);
            usedOtherFieldCheckHandler.setClassNameExistsMap(classNameExistsMap);
            usedOtherFieldCheckHandler.setCurrentOutputDirPath(currentOutputDirPath);
            usedOtherFieldCheckHandler.setJarInfoMap(jarInfoMap);
            usedOtherFieldCheckHandler.init();
            if (!usedOtherFieldCheckHandler.check()) {
                recordTaskFail();
            }
        }
    }

    @Override
    // 结束前的处理
    protected void beforeExit() {
        super.beforeExit();
        // 结束前关闭数据源
        if (otherDbOperWrapperList != null) {
            for (DbOperWrapper tmpDbOperWrapper : otherDbOperWrapperList) {
                tmpDbOperWrapper.getDbOperator().closeDs(this);
            }
        }
    }

    @Override
    protected boolean checkH2DbFile() {
        // 检查H2数据库文件是否可写，不允许文件不存在
        return checkH2DbFileWritable(false);
    }

    /**
     * 是否需要操作数据库
     *
     * @return true: 需要操作数据库 false: 不需要操作数据库
     */
    @Override
    protected boolean handleDb() {
        return true;
    }

    // 查询解析的jar文件信息
    protected void queryJarInfoMap() {
        List<WriteDbData4JarInfo> jarInfoList = jarInfoHandler.queryAllJarInfo();
        for (WriteDbData4JarInfo jarInfo : jarInfoList) {
            jarInfoMap.put(jarInfo.getJarNum(), jarInfo);
        }
    }

    // 生成其他的数据库包装类列表
    private List<DbOperWrapper> genOtherDbOperWrapperList() {
        List<String> otherH2DbFilePathList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_COMPATIBILITY_OTHER_H2_DB_PATH);
        List<DbOperWrapper> otherDbOperWrapperList = new ArrayList<>();
        if (JavaCG2Util.isCollectionEmpty(otherH2DbFilePathList)) {
            return otherDbOperWrapperList;
        }
        for (String h2DbFilePath : otherH2DbFilePathList) {
            ConfigureWrapper tmpConfigureWrapper = new ConfigureWrapper();
            // 使用固定app.name的H2数据库文件
            tmpConfigureWrapper.useFixedAppNameH2Db();
            tmpConfigureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, h2DbFilePath);
            // 访问其他H2数据库时，使用只读模式
            DbOperWrapper tmpDbOperWrapper = DbInitializer.genDbOperWrapper(tmpConfigureWrapper, false, true, this);
            otherDbOperWrapperList.add(tmpDbOperWrapper);
        }
        return otherDbOperWrapperList;
    }
}
