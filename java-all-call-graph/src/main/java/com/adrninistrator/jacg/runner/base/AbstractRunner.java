package com.adrninistrator.jacg.runner.base;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.interfaces.BaseConfigInterface;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.handler.extends_impl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.markdown.writer.MarkdownWriter;
import com.adrninistrator.jacg.thread.ThreadFactory4TPE;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */
public abstract class AbstractRunner {
    private static final Logger logger = LoggerFactory.getLogger(AbstractRunner.class);

    // 配置信息包装类
    protected ConfigureWrapper configureWrapper;

    // 是否有检查过数据库文件是否可写
    protected static boolean CHECK_H2_DB_FILE_WRITEABLE = false;

    protected boolean inited = false;

    protected ConfInfo confInfo;

    protected DbOperator dbOperator;

    protected DbOperWrapper dbOperWrapper;

    protected ThreadPoolExecutor threadPoolExecutor;

    // 任务执行失败标志
    protected boolean someTaskFail = false;

    // 记录执行失败的任务信息
    protected List<String> failTaskList = new ArrayList<>();

    // 任务队列最大长度
    protected int taskQueueMaxSize;

    // 继承与实际相关的处理类
    protected JACGExtendsImplHandler jacgExtendsImplHandler;

    protected final String currentSimpleClassName = this.getClass().getSimpleName();

    /**
     * 执行任务
     *
     * @return true: 成功；false: 失败
     */
    public boolean run() {
        return run(new ConfigureWrapper());
    }

    /**
     * 初始化
     *
     * @return true: 成功；false: 失败
     */
    protected abstract boolean preHandle();

    /**
     * 执行处理
     */
    protected abstract void handle();

    /**
     * 检查H2数据库文件
     *
     * @return
     */
    protected abstract boolean checkH2DbFile();

    /**
     * 初始化
     *
     * @param configureWrapper
     * @return
     */
    public boolean init(ConfigureWrapper configureWrapper) {
        synchronized (this) {
            if (inited) {
                logger.warn("{} 已完成初始化，不会再初始化", currentSimpleClassName);
                return true;
            }

            this.configureWrapper = configureWrapper;
            confInfo = ConfManager.getConfInfo(configureWrapper);
            if (confInfo == null) {
                return false;
            }

            dbOperator = DbOperator.genInstance(confInfo, currentSimpleClassName);
            if (dbOperator == null) {
                return false;
            }

            dbOperWrapper = new DbOperWrapper(dbOperator);

            jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperator, dbOperWrapper);

            inited = true;
            return true;
        }
    }

    /**
     * 打印当前使用的配置信息
     *
     * @param currentOutputDirPath 当前的输出目录
     */
    protected void printUsedConfigInfo(String currentOutputDirPath) {
        String configMdFilePath = currentOutputDirPath;
        if (!configMdFilePath.endsWith(File.separator)) {
            configMdFilePath += File.separator;
        }
        configMdFilePath += JACGConstants.FILE_USED_CONFIG_MD;
        logger.info("{} 当前使用的配置参数信息保存到以下文件 {}", currentSimpleClassName, configMdFilePath);
        try (MarkdownWriter markdownWriter = new MarkdownWriter(configMdFilePath, true)) {
            // 打印基本的配置信息
            printConfigInfo(markdownWriter, ConfigKeyEnum.values(), JACGConstants.FILE_CONFIG);
            printConfigInfo(markdownWriter, ConfigDbKeyEnum.values(), JACGConstants.FILE_CONFIG_DB);

            // 打印Set格式的其他配置信息
            printOtherSetConfigInfo(markdownWriter, OtherConfigFileUseSetEnum.values());

            // 打印List格式的其他配置信息
            printOtherListConfigInfo(markdownWriter, OtherConfigFileUseListEnum.values());
        } catch (Exception e) {
            logger.error("{} error ", currentSimpleClassName, e);
        }
    }

    // 打印基本的配置信息
    private void printConfigInfo(MarkdownWriter markdownWriter, BaseConfigInterface[] configs, String configFileName) throws IOException {
        // 写入配置文件名
        markdownWriter.addTitle(1, configFileName);
        markdownWriter.addTableHead("参数名称", "参数说明", "参数值");

        for (BaseConfigInterface currentConfigEnum : configs) {
            String value = configureWrapper.getConfig(null, currentConfigEnum, false);
            markdownWriter.addTableBody(currentConfigEnum.getKey(), currentConfigEnum.getDesc(), (value == null ? "" : value));
        }
        markdownWriter.addEmptyLine();
    }

    // 打印List格式的其他配置信息
    private void printOtherListConfigInfo(MarkdownWriter markdownWriter, OtherConfigFileUseListEnum[] configs) throws IOException {        // 写入配置文件名
        markdownWriter.addTitle(1, "区分顺序的其他配置信息");

        List<String> findKeywordFilterList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_FIND_KEYWORD_FILTER, false);
        boolean useFindKeywordFilter = !JACGUtil.isCollectionEmpty(findKeywordFilterList);

        for (OtherConfigFileUseListEnum currentConfig : configs) {
            // 写入配置文件名
            markdownWriter.addTitle(2, currentConfig.getKey());

            markdownWriter.addListWithNewLine("参数说明");
            markdownWriter.addLineWithNewLine(currentConfig.getDesc());

            markdownWriter.addListWithNewLine("参数值");
            markdownWriter.addCodeBlock();

            if (useFindKeywordFilter && (currentConfig == OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLEE ||
                    currentConfig == OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLER)) {
                markdownWriter.addList("对调用链文件查找关键字时使用过滤器扩展类");
            } else {
                for (String configValue : configureWrapper.getOtherConfigList(currentConfig, false)) {
                    markdownWriter.addLine(configValue);
                }
            }

            markdownWriter.addCodeBlock();
        }
    }

    // 打印Set格式的其他配置信息
    private void printOtherSetConfigInfo(MarkdownWriter markdownWriter, OtherConfigFileUseSetEnum[] configs) throws IOException {
        markdownWriter.addTitle(1, "不区分顺序的其他配置信息");

        for (OtherConfigFileUseSetEnum currentConfig : configs) {
            // 写入配置文件名
            markdownWriter.addTitle(1, currentConfig.getKey());

            markdownWriter.addListWithNewLine("参数说明");
            markdownWriter.addLineWithNewLine(currentConfig.getDesc());

            markdownWriter.addListWithNewLine("参数值");
            markdownWriter.addCodeBlock();
            List<String> configValueList = new ArrayList<>(configureWrapper.getOtherConfigSet(currentConfig, false));
            for (String configValue : configValueList) {
                markdownWriter.addLine(configValue);
            }
            markdownWriter.addCodeBlock();
        }
    }

    /**
     * 执行任务，通过代码指定配置参数
     *
     * @param configureWrapper 当前使用的配置信息
     * @return
     */
    public boolean run(ConfigureWrapper configureWrapper) {
        try {
            logger.info("{} 开始执行", currentSimpleClassName);

            long startTime = System.currentTimeMillis();
            someTaskFail = false;

            if (!init(configureWrapper)) {
                logger.error("{} 初始化失败", currentSimpleClassName);
                return false;
            }

            if (!preCheck()) {
                logger.error("{} 预检查失败", currentSimpleClassName);
                return false;
            }

            if (!preHandle()) {
                logger.error("{} 预处理失败", currentSimpleClassName);
                return false;
            }

            // 执行处理
            handle();

            if (!someTaskFail) {
                long spendTime = System.currentTimeMillis() - startTime;
                logger.info("{} 执行完毕，耗时: {} S", currentSimpleClassName, spendTime / 1000.0D);
                return true;
            }

            logger.error("{} 执行失败", currentSimpleClassName);
            return false;
        } finally {
            // 结束前的处理，需要确保能执行到，在其中会关闭数据源
            beforeExit();
        }
    }

    /**
     * 预检查
     *
     * @return true: 成功；false: 失败
     */
    protected boolean preCheck() {
        if (dbOperator.isClosed()) {
            logger.error("当前类上次执行方法完毕后已将数据库关闭，若需要再次执行，需要重新创建对象再执行");
            return false;
        }

        // 使用H2数据库时，检查数据库文件
        if (confInfo.isDbUseH2() && !checkH2DbFile()) {
            return false;
        }

        return true;
    }

    // 结束前的处理
    protected void beforeExit() {
        if (dbOperator != null) {
            dbOperator.closeDs();
        }

        if (threadPoolExecutor != null) {
            threadPoolExecutor.shutdown();
        }

        if (someTaskFail) {
            logger.error("{} 有任务执行失败，请检查\n{}", currentSimpleClassName, StringUtils.join(failTaskList, "\n"));
        } else {
            logger.info("{} 任务执行完毕", currentSimpleClassName);
        }
    }

    /**
     * 创建线程池
     *
     * @param taskNum 任务数量，非空时尝试根据任务数量调中实际创建的线程数
     */
    protected void createThreadPoolExecutor(Integer taskNum) {
        if (taskNum != null && taskNum < confInfo.getThreadNum()) {
            // 任务数量比配置文件中指定的线程数少，则调小实际创建的线程数
            logger.info("{} 将线程数修改为需要处理的任务数 {}", currentSimpleClassName, taskNum);
            confInfo.setThreadNum(taskNum);
        }

        // 任务队列最大长度，设置为线程数2倍
        taskQueueMaxSize = confInfo.getThreadNum() * 2;
        threadPoolExecutor = new ThreadPoolExecutor(confInfo.getThreadNum(), confInfo.getThreadNum(), 10L, TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(taskQueueMaxSize), new ThreadFactory4TPE("jacg_worker"));
    }

    // 等待直到任务执行完毕
    protected void wait4TPEDone() {
        while (true) {
            if (threadPoolExecutor.getActiveCount() == 0 && threadPoolExecutor.getQueue().isEmpty()) {
                return;
            }
            logger.debug("{} wait4TPEDone ...", currentSimpleClassName);
            JACGUtil.sleep(100L);
        }
    }

    // 获取H2数据库文件对象
    protected File getH2DbFile() {
        return new File(confInfo.getDbH2FilePath() + JACGConstants.H2_FILE_EXT);
    }

    // 获得需要处理的jar包数组
    protected List<String> getJarPathList() {
        List<String> jarPathList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIR, true);
        logger.info("{} 需要处理的jar包或目录\n{}", currentSimpleClassName, StringUtils.join(jarPathList, "\n"));
        return jarPathList;
    }

    /**
     * 检查H2数据库文件是否可写
     * 需要进行同步控制，避免同时执行
     *
     * @param h2DbFile
     * @return
     */
    protected boolean checkH2DbFileWritable(File h2DbFile) {
        synchronized (AbstractRunner.class) {
            // 以下操作在JVM中只能成功执行一次，需要避免执行多次
            if (CHECK_H2_DB_FILE_WRITEABLE) {
                return true;
            }

            logger.info("{} 检查H2数据库文件是否可写", currentSimpleClassName);
            // 尝试以写方式打开，检查数据库文件是否被占用
            try (FileChannel channel = FileChannel.open(h2DbFile.toPath(), StandardOpenOption.WRITE)) {
                FileLock fileLock = channel.tryLock();
                if (fileLock == null) {
                    logger.error("{} H2数据库文件无法写入，请先关闭H2数据库工具打开的H2数据库文件 {}", currentSimpleClassName, JACGFileUtil.getCanonicalPath(h2DbFile));
                    return false;
                }

                fileLock.release();
                logger.info("{} H2数据库文件可写", currentSimpleClassName);
                CHECK_H2_DB_FILE_WRITEABLE = true;
                return true;
            } catch (OverlappingFileLockException e) {
                // 某些情况下会出现以上异常，当作正常处理
                logger.warn("{} 检查H2数据库文件是否可以写入时，出现重复对文件加锁的异常，当作正常处理 {} {} {}", currentSimpleClassName, JACGFileUtil.getCanonicalPath(h2DbFile),
                        e.getClass().getName(), e.getMessage());
                return true;
            } catch (Exception e) {
                logger.error("{} 检查H2数据库文件是否可以写入失败，请重试 {} ", currentSimpleClassName, JACGFileUtil.getCanonicalPath(h2DbFile), e);
                return false;
            }
        }
    }

    // 记录执行失败
    protected void recordTaskFail() {
        someTaskFail = true;
    }

    // 记录执行失败的任务信息
    protected void recordTaskFail(String taskInfo) {
        someTaskFail = true;

        synchronized (AbstractRunner.class) {
            failTaskList.add(taskInfo);
        }
    }

    /**
     * 获取配置信息
     *
     * @return
     */
    public ConfInfo getConfInfo() {
        return confInfo;
    }

    /**
     * 获取执行失败的任务信息
     *
     * @return
     */
    public List<String> getFailTaskList() {
        return failTaskList;
    }
}
