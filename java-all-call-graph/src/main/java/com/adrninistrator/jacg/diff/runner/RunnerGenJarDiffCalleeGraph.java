package com.adrninistrator.jacg.diff.runner;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.diff.dto.method.ModifiedMethodInfo;
import com.adrninistrator.jacg.dto.callstack.CallStackFileResult;
import com.adrninistrator.jacg.dto.entrymethodinfo.BaseEntryMethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import com.adrninistrator.jacg.handler.entrymethodinfo.AbstractEntryMethodInfoFiller;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/3/9
 * @description: 比较新旧两个目录中不同版本jar包的方法修改情况，以及新目录中修改方法的影响范围
 */
public class RunnerGenJarDiffCalleeGraph {

    private static final Logger logger = LoggerFactory.getLogger(RunnerGenJarDiffCalleeGraph.class);

    private final ConfigureWrapper configureWrapper;

    // 是否跳过写入数据库的步骤
    private boolean skipWriteDb;

    private final DbOperator dbOperator;

    private final DbOperWrapper dbOperWrapper;

    private final String appName;

    public RunnerGenJarDiffCalleeGraph() {
        this(new ConfigureWrapper(false));
    }

    public RunnerGenJarDiffCalleeGraph(ConfigureWrapper configureWrapper) {
        this.configureWrapper = configureWrapper;
        dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        dbOperator = dbOperWrapper.getDbOperator();
        appName = dbOperator.getAppName();
    }

    /**
     * 使用代码指定的配置参数生成
     *
     * @param entryMethodInfoFillers 对入口方法信息进行填充的类
     * @return
     */
    public boolean generate(AbstractEntryMethodInfoFiller... entryMethodInfoFillers) {
        String dirPathNew = null;

        if (!skipWriteDb) {
            List<String> jarDiffDirPathList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_DIR, true);
            if (JavaCG2Util.isCollectionEmpty(jarDiffDirPathList) || jarDiffDirPathList.size() != 2) {
                logger.error("请修改配置文件，或通过代码指定对应的参数，在其中指定两行内容，第一行为旧目录的路径，第二行为新目录的路径 {}", OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_DIR.getConfigPrintInfo());
                return false;
            }

            String dirPathOld = jarDiffDirPathList.get(0);
            dirPathNew = jarDiffDirPathList.get(1);

            // 解析新旧目录的jar包并写入数据库
            if (!writeDb(dirPathOld, dirPathNew)) {
                return false;
            }
        } else {
            logger.info("跳过与入数据库步骤");
        }

        try {
            // 查询新的发生改变的jar包
            List<Pair<WriteDbData4JarInfo, WriteDbData4JarInfo>> modifiedJarInfoList = queryModifiedJarInfo();
            if (JavaCG2Util.isCollectionEmpty(modifiedJarInfoList)) {
                return true;
            }

            /*
                保存发生变化的jar包信息
                key     新jar包名称
                value   旧jar包名称
             */
            Map<String, String> modifiedJarMap = new HashMap<>();

            /*
                保存各个jar包中发生变化的方法信息
                key     jar包名称
                value   发生变化的方法信息列表
             */
            Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap = new HashMap<>();

            /*
                保存发生变化的方法的类所在的jar包
                key     类名
                value   jar包名称
             */
            Map<String, String> modifiedClassJarMap = new HashMap<>();

            for (Pair<WriteDbData4JarInfo, WriteDbData4JarInfo> pair : modifiedJarInfoList) {
                // 获取发生变化的jar包
                recordModifiedJar(pair, modifiedJarMap, jarModifiedMethodInfoMap, modifiedClassJarMap);
            }

            // 处理发生变化的jar包
            return handleModifiedJar(modifiedJarMap, jarModifiedMethodInfoMap, modifiedClassJarMap, dirPathNew, entryMethodInfoFillers);
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        } finally {
            if (dbOperator != null) {
                dbOperator.closeDs(this);
            }
        }
    }

    /**
     * 查询新的发生改变的jar包
     *
     * @return Pair.left 发生改变的新的jar包信息 Pair.right 对应的旧jar包信息，若为null则说明Pair.left是新增的jar包
     */
    private List<Pair<WriteDbData4JarInfo, WriteDbData4JarInfo>> queryModifiedJarInfo() {
        // 查询旧的jar包信息
        List<WriteDbData4JarInfo> jarInfoListOld = queryJarInfo(true);
        // 查询新的jar包信息
        List<WriteDbData4JarInfo> jarInfoListNew = queryJarInfo(false);
        if (JavaCG2Util.isCollectionEmpty(jarInfoListNew)) {
            logger.info("从新的目录中未查询到jar包信息");
            return null;
        }

        List<Pair<WriteDbData4JarInfo, WriteDbData4JarInfo>> modifiedJarInfoList = new ArrayList<>();
        // 遍历新的与旧的jar包
        for (WriteDbData4JarInfo jarInfoNew : jarInfoListNew) {
            logger.info("处理新的jar包 {}", jarInfoNew.getJarFileName());
            String jarFileNameHeadNew = jarInfoNew.getJarFileNameHead();
            String jarFileNameExtNew = jarInfoNew.getJarFileNameExt();
            boolean findOldJar = false;
            for (WriteDbData4JarInfo jarInfoOld : jarInfoListOld) {
                if (jarFileNameHeadNew.equals(jarInfoOld.getJarFileNameHead()) && jarFileNameExtNew.equals(jarInfoOld.getJarFileNameExt())) {
                    if (jarInfoNew.getJarFileHash().equals(jarInfoOld.getJarFileHash())) {
                        logger.info("找到对应的旧jar包，内容未发生变化 {} {}", jarInfoNew.getJarFileName(), jarInfoOld.getJarFileName());
                        continue;
                    }
                    logger.info("找到对应的旧jar包，内容发生变化 {} {}", jarInfoNew.getJarFileName(), jarInfoOld.getJarFileName());
                    modifiedJarInfoList.add(new ImmutablePair<>(jarInfoNew, jarInfoOld));
                    findOldJar = true;
                    break;
                }
            }
            if (!findOldJar) {
                // 新jar包未找到对应的旧jar包，说明是新增的jar包
                logger.info("未找到对应的旧jar包 {}", jarInfoNew.getJarFileName());
                modifiedJarInfoList.add(new ImmutablePair<>(jarInfoNew, null));
            }
        }
        return modifiedJarInfoList;
    }

    // 获取发生变化的jar包
    private void recordModifiedJar(Pair<WriteDbData4JarInfo, WriteDbData4JarInfo> pair, Map<String, String> modifiedJarMap,
                                   Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap, Map<String, String> modifiedClassJarMap) {
        WriteDbData4JarInfo jarInfoNew = pair.getLeft();
        WriteDbData4JarInfo jarInfoOld = pair.getRight();
        boolean oldJarExists = jarInfoOld != null;

        List<WriteDbData4ClassInfo> classInfoListNew = queryClassInfoNew(jarInfoNew.getJarNum());
        if (JavaCG2Util.isCollectionEmpty(classInfoListNew)) {
            logger.info("未找到jar包对应的class {}", jarInfoNew.getJarFileName());
            return;
        }
        List<ModifiedMethodInfo> modifiedMethodInfoList = new ArrayList<>();
        for (WriteDbData4ClassInfo classInfoNew : classInfoListNew) {
            boolean oldClassExists = oldJarExists;
            if (oldJarExists) {
                // 存在对应的旧jar包，查询旧的class的HASH
                String oldClassFileHash = queryClassFileHashOld(jarInfoOld.getJarNum(), classInfoNew.getClassName());
                if (oldClassFileHash == null) {
                    logger.debug("不存在旧的class {} {}", jarInfoNew.getJarFileName(), classInfoNew.getClassName());
                    oldClassExists = false;
                } else if (oldClassFileHash.equals(classInfoNew.getClassFileHash())) {
                    logger.debug("class文件HASH没有变化，跳过 {} {}", jarInfoNew.getJarFileName(), classInfoNew.getClassName());
                    continue;
                }
            }
            // class文件有变化，或旧class文件不存在
            logger.debug("class文件HASH发生变化 {} {}", jarInfoNew.getJarFileName(), classInfoNew.getClassName());
            modifiedClassJarMap.put(classInfoNew.getClassName(), jarInfoNew.getJarFileName());
            // 查询新的方法信息
            List<WriteDbData4MethodInfo> methodInfoListNew = queryMethodInfoNew(jarInfoNew.getJarNum(), classInfoNew.getClassName());
            if (JavaCG2Util.isCollectionEmpty(methodInfoListNew)) {
                logger.debug("未找到class对应的method {} {}", jarInfoNew.getJarFileName(), classInfoNew.getClassName());
                continue;
            }
            for (WriteDbData4MethodInfo methodInfoNew : methodInfoListNew) {
                if (StringUtils.isBlank(methodInfoNew.getMethodInstructionsHash()) || JavaCG2CommonNameConstants.METHOD_NAME_CLINIT.equals(methodInfoNew.getMethodName())) {
                    // 新方法的方法指令HASH为空，或者方法名称为<clinit>（不会被直接调用），跳过
                    logger.debug("跳过当前方法的处理 {} {}", jarInfoNew.getJarFileName(), methodInfoNew.getFullMethod());
                    continue;
                }

                boolean oldMethodExists = oldClassExists;
                if (oldClassExists) {
                    // 存在对应的旧jar包，查询旧的方法的HASH
                    String methodInstructionsHashOld = queryMethodInstructionHashOld(methodInfoNew.getMethodHash());
                    if (methodInstructionsHashOld == null) {
                        logger.debug("不存在旧的方法 {} {}", jarInfoNew.getJarFileName(), methodInfoNew.getFullMethod());
                        oldMethodExists = false;
                    } else if (methodInstructionsHashOld.equals(methodInfoNew.getMethodInstructionsHash())) {
                        logger.debug("方法指令HASH没有变化，跳过 {} {}", jarInfoNew.getJarFileName(), methodInfoNew.getFullMethod());
                        continue;
                    }
                }
                logger.debug("方法指令HASH发生变化 {} {}", jarInfoNew.getJarFileName(), methodInfoNew.getFullMethod());
                // 方法指令有变化，或旧方法不存在，记录对应方法
                modifiedMethodInfoList.add(new ModifiedMethodInfo(methodInfoNew.getFullMethod(), oldMethodExists));
            }
        }
        if (modifiedMethodInfoList.isEmpty()) {
            return;
        }
        // 记录发生变化的jar包及方法信息
        modifiedJarMap.put(jarInfoNew.getJarFileName(), oldJarExists ? jarInfoOld.getJarFileName() : "");
        jarModifiedMethodInfoMap.put(jarInfoNew.getJarFileName(), modifiedMethodInfoList);
    }

    // 处理发生变化的jar包
    private boolean handleModifiedJar(Map<String, String> modifiedJarMap, Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap, Map<String, String> modifiedClassJarMap,
                                      String dirPathNew, AbstractEntryMethodInfoFiller... entryMethodInfoFillers) {
        Set<String> modifiedMethodSet = new HashSet<>();
        List<String> jarFileNameListNew = new ArrayList<>(modifiedJarMap.keySet());
        Collections.sort(jarFileNameListNew);
        for (String jarFileNameNew : jarFileNameListNew) {
            List<ModifiedMethodInfo> modifiedMethodInfoList = jarModifiedMethodInfoMap.get(jarFileNameNew);
            for (ModifiedMethodInfo modifiedMethodInfo : modifiedMethodInfoList) {
                modifiedMethodSet.add(modifiedMethodInfo.getFullMethod());
            }
        }

        ConfigureWrapper configureWrapperNew = configureWrapper.copy();
        // 指定数据库表名后缀使用代表新的
        configureWrapperNew.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, JACGConstants.TABLE_SUFFIX_NEW);
        configureWrapperNew.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_1.getDetail());
        configureWrapperNew.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, modifiedMethodSet);
        if (skipWriteDb) {
            configureWrapperNew.setMainConfig(ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED, Boolean.FALSE.toString());
        } else {
            configureWrapperNew.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIR, dirPathNew);
        }
        configureWrapperNew.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE, JACGConstants.CALLEE_FLAG_ENTRY);
        FindCallStackTrace findCallStackTrace = new FindCallStackTrace(true, configureWrapperNew);
        // 生成发生变化方法到入口方法的调用堆栈文件
        CallStackFileResult callStackFileResult = findCallStackTrace.find();
        if (!callStackFileResult.isSuccess()) {
            return false;
        }

        List<String> separateStackDirPathList = callStackFileResult.getSeparateStackDirPathList();
        if (separateStackDirPathList.isEmpty()) {
            logger.warn("生成发生变化方法到入口方法的调用堆栈文件为空");
            return true;
        }
        // 保存单独的调用堆栈文件的目录，作为输出目录
        String separateStackDirRootPath = new File(separateStackDirPathList.get(0)).getParent();
        String modifiedMethodsBaseFilePath = separateStackDirRootPath + File.separator + JACGConstants.FILE_JAR_DIFF_MODIFIED_METHODS_BASE;
        String modifiedMethodsStackFilePath = separateStackDirRootPath + File.separator + JACGConstants.FILE_JAR_DIFF_MODIFIED_METHODS_STACK;
        try (BufferedWriter modifiedMethodsBaseWriter = JavaCG2FileUtil.genBufferedWriter(modifiedMethodsBaseFilePath);
             BufferedWriter modifiedMethodsStackWriter = JavaCG2FileUtil.genBufferedWriter(modifiedMethodsStackFilePath)) {
            // 生成jar包中发生变化的方法基本信息
            for (String jarFileNameNew : jarFileNameListNew) {
                String jarFileNameOld = modifiedJarMap.get(jarFileNameNew);
                List<ModifiedMethodInfo> modifiedMethodInfoList = jarModifiedMethodInfoMap.get(jarFileNameNew);
                for (ModifiedMethodInfo modifiedMethodInfo : modifiedMethodInfoList) {
                    JavaCG2FileUtil.write2FileWithTab(modifiedMethodsBaseWriter, jarFileNameNew, jarFileNameOld, modifiedMethodInfo.getFullMethod(),
                            modifiedMethodInfo.isOldMethodExists() ? JACGConstants.JAR_DIFF_METHOD_EXISTED : JACGConstants.JAR_DIFF_METHOD_NEW);
                }
            }

            // 生成jar包中发生变化的方法的调用堆栈信息
            for (String separateStackDirPath : separateStackDirPathList) {
                // 处理单独的调用堆栈文件目录
                handleSeparateStackDir(modifiedMethodsStackWriter, separateStackDirPath, modifiedClassJarMap, entryMethodInfoFillers);
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 处理单独的调用堆栈文件目录
    private void handleSeparateStackDir(BufferedWriter modifiedMethodsStackWriter, String separateStackDirPath, Map<String, String> modifiedClassJarMap,
                                        AbstractEntryMethodInfoFiller... entryMethodInfoFillers) throws IOException {
        String summaryFilePath = separateStackDirPath + File.separator + JACGConstants.FILE_STACK_SUMMARY_CALLEE_MD;
        try (BufferedReader reader = JavaCG2FileUtil.genBufferedReader(summaryFilePath)) {
            String line;
            while ((line = reader.readLine()) != null) {
                String[] array = StringUtils.split(line, JavaCG2Constants.FLAG_TAB);
                String calleeMethod = array[0];
                String stackSeq = array[1];
                String callerMethod = array[2];
                String entryMethod = array[3];
                String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeMethod);
                String jarName = modifiedClassJarMap.get(calleeClassName);
                // 查询入口方法的信息
                String entryMethodInfo = queryEntryMethodInfo(entryMethod, entryMethodInfoFillers);
                JavaCG2FileUtil.write2FileWithTab(modifiedMethodsStackWriter, jarName, calleeMethod, stackSeq, callerMethod, entryMethod, entryMethodInfo);
            }
        }
    }

    // 查询入口方法的信息
    private String queryEntryMethodInfo(String entryMethod, AbstractEntryMethodInfoFiller... entryMethodInfoFillers) {
        if (ArrayUtils.isEmpty(entryMethodInfoFillers)) {
            return "";
        }

        // 使用指定的入口方法信息填充类进行处理
        for (AbstractEntryMethodInfoFiller entryMethodInfoFiller : entryMethodInfoFillers) {
            BaseEntryMethodInfo baseEntryMethodInfo = entryMethodInfoFiller.query(entryMethod);
            if (baseEntryMethodInfo != null) {
                return JACGJsonUtil.getJsonStr(baseEntryMethodInfo);
            }
        }
        return "";
    }

    // 查询新的class信息
    private List<WriteDbData4ClassInfo> queryClassInfoNew(int jarNum) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CI_QUERY_ALL_BY_JAR_NUM_NEW;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_CLASS_INFO) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_INFO.getTableName(appName, chooseTableSuffixNew()) +
                    " where " + DC.CI_JAR_NUM + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4ClassInfo.class, jarNum);
    }

    // 查询旧的class的HASH
    private String queryClassFileHashOld(int jarNum, String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CI_QUERY_HASH_BY_CLASS_JAR_OLD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CI_CLASS_FILE_HASH +
                    " from " + DbTableInfoEnum.DTIE_CLASS_INFO.getTableName(appName, chooseTableSuffixOld()) +
                    " where " + DC.CI_JAR_NUM + " = ?" +
                    " and " + DC.CI_CLASS_NAME + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, jarNum, className);
    }

    // 查询新的方法信息
    private List<WriteDbData4MethodInfo> queryMethodInfoNew(int jarNum, String className) {
        String simpleClassName = dbOperWrapper.querySimpleClassName(className, chooseTableSuffixNew());
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_ALL_BY_CLASS_JAR_METHOD_NEW;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName(appName, chooseTableSuffixNew()) +
                    " where " + DC.MI_JAR_NUM + " = ?" +
                    " and " + DC.MI_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodInfo.class, jarNum, simpleClassName);
    }

    // 查询旧的方法的HASH
    private String queryMethodInstructionHashOld(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_INSTRUCTIONS_HASH_BY_HASH_OLD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MI_METHOD_INSTRUCTIONS_HASH +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName(appName, chooseTableSuffixOld()) +
                    " where " + DC.MI_METHOD_HASH + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, methodHash);
    }

    // 查询jar包信息
    private List<WriteDbData4JarInfo> queryJarInfo(boolean old) {
        SqlKeyEnum sqlKeyEnum = old ? SqlKeyEnum.JI_QUERY_JAR_INFO_OLD : SqlKeyEnum.JI_QUERY_JAR_INFO_NEW;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_JAR_INFO) +
                    " from " + DbTableInfoEnum.DTIE_JAR_INFO.getTableName(appName, chooseTableSuffix(old)) +
                    " where " + DC.JI_JAR_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4JarInfo.class, JavaCG2Constants.FILE_KEY_JAR_INFO_PREFIX);
    }

    // 解析新旧目录的jar包并写入数据库
    private boolean writeDb(String dirPathOld, String dirPathNew) {
        if (!JavaCG2FileUtil.isDirectoryExists(dirPathOld, false)) {
            logger.error("配置文件中指定的需要比较的旧目录不存在 {} {}", OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_DIR.getConfigPrintInfo(), dirPathOld);
            return false;
        }

        if (!JavaCG2FileUtil.isDirectoryExists(dirPathNew, false)) {
            logger.error("配置文件中指定的需要比较的新目录不存在 {} {}", OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_DIR.getConfigPrintInfo(), dirPathNew);
            return false;
        }

        logger.info("解析旧目录中的jar包并写入数据库 {}", dirPathOld);
        ConfigureWrapper configureWrapperOld = configureWrapper.copy();
        // 指定数据库表名后缀使用代表旧的
        configureWrapperOld.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, JACGConstants.TABLE_SUFFIX_OLD);
        configureWrapperOld.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIR, dirPathOld);
        if (!new RunnerWriteDb(configureWrapperOld).run()) {
            return false;
        }

        logger.info("解析新目录中的jar包并写入数据库 {}", dirPathNew);
        ConfigureWrapper configureWrapperNew = configureWrapper.copy();
        // 指定数据库表名后缀使用代表新的
        configureWrapperNew.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, JACGConstants.TABLE_SUFFIX_NEW);
        configureWrapperNew.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIR, dirPathNew);
        return new RunnerWriteDb(configureWrapperNew).run();
    }

    // 获取当前使用的数据库表名后缀
    private String chooseTableSuffix(boolean old) {
        return old ? JACGConstants.TABLE_SUFFIX_OLD : JACGConstants.TABLE_SUFFIX_NEW;
    }

    private String chooseTableSuffixOld() {
        return chooseTableSuffix(true);
    }

    private String chooseTableSuffixNew() {
        return chooseTableSuffix(false);
    }

    public void setSkipWriteDb(boolean skipWriteDb) {
        this.skipWriteDb = skipWriteDb;
    }
}
