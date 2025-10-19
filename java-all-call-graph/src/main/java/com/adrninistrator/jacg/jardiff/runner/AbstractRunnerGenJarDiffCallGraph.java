package com.adrninistrator.jacg.jardiff.runner;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSFormatedSql;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.jacg.el.manager.ElManager;
import com.adrninistrator.jacg.handler.mybatis.MybatisMsFormatedSqlHandler;
import com.adrninistrator.jacg.jardiff.dto.method.ModifiedMethodInfo;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.runner.base.AbstractRunner;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/5/12
 * @description: 比较新旧两个目录中不同版本jar文件的方法修改情况，并生成向上或向下的完整方法调用链，基类
 */
public abstract class AbstractRunnerGenJarDiffCallGraph extends AbstractRunner {
    private static final Logger logger = LoggerFactory.getLogger(AbstractRunnerGenJarDiffCallGraph.class);

    public static final String[] FILE_HEADER_ARRAY_MODIFIED_METHODS_BASE = new String[]{
            "jar文件名称前缀",
            "新jar文件名称",
            "旧jar文件名称",
            "发生变化的完整方法",
            "方法在旧jar文件中是否存在"
    };

    public static final String FILE_HEADER_MODIFIED_METHODS_BASE = StringUtils.join(FILE_HEADER_ARRAY_MODIFIED_METHODS_BASE, JavaCG2Constants.FLAG_TAB);

    protected final JavaCG2ConfigureWrapper javaCG2ConfigureWrapper;

    /*
        保存各个jar文件中发生变化的方法信息
        key     jar文件名称
        value   发生变化的方法信息列表
     */
    protected final Map<String, List<ModifiedMethodInfo>> jarModifiedMethodInfoMap = new HashMap<>();

    protected final MybatisMsFormatedSqlHandler mybatisMsFormatedSqlHandler;

    private final OtherConfigFileUseListEnum compareDirEnum;

    private final boolean genCalleeGraph;

    private ElManager elManager;

    // 是否跳过写入数据库的步骤
    protected boolean skipWriteDb;

    protected AbstractRunnerGenJarDiffCallGraph(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        this.javaCG2ConfigureWrapper = javaCG2ConfigureWrapper;
        mybatisMsFormatedSqlHandler = new MybatisMsFormatedSqlHandler(dbOperWrapper);
        compareDirEnum = chooseCompareDirEnum();
        genCalleeGraph = this instanceof RunnerGenJarDiffCalleeGraph;
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

    @Override
    protected boolean preHandle() {
        String dirPathNew;
        if (skipWriteDb) {
            logger.info("跳过写入数据库步骤");
            return true;
        }
        List<String> jarDiffDirPathList = configureWrapper.getOtherConfigList(compareDirEnum);
        if (JavaCG2Util.isCollectionEmpty(jarDiffDirPathList) || jarDiffDirPathList.size() != 2) {
            logger.error("请修改配置文件，或通过代码指定对应的参数，在其中指定两行内容，第一行为旧目录的路径，第二行为新目录的路径 {}", compareDirEnum.getConfigPrintInfo());
            return false;
        }

        String dirPathOld = jarDiffDirPathList.get(0);
        dirPathNew = jarDiffDirPathList.get(1);

        // 解析新旧目录的jar文件并写入数据库
        return writeDb(dirPathOld, dirPathNew);
    }

    @Override
    protected void handle() {
        try {
            // 查询新的发生改变的jar文件
            List<Pair<WriteDbData4JarInfo, WriteDbData4JarInfo>> modifiedJarInfoList = queryModifiedJarInfo();
            if (JavaCG2Util.isCollectionEmpty(modifiedJarInfoList)) {
                logger.warn("未查询到发生改变的jar文件");
                return;
            }

            String upwardDir = genCalleeGraph ? JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLEE : JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLER;
            String downwardDir = genCalleeGraph ? JACGConstants.DIR_CALLEE_JAR_DIFF_SUMMARY : JACGConstants.DIR_CALLER_JAR_DIFF_SUMMARY;
            // 生成输出目录
            if (!createOutputDir(upwardDir, downwardDir)) {
                recordTaskFail();
                return;
            }

            // 完整方法调用链生成目录，使用输出目录父目录
            File currentOutputDir = new File(currentOutputDirPath);
            String genAllCallGraphDir = JavaCG2FileUtil.getCanonicalPath(currentOutputDir.getParentFile());
            elManager = new ElManager(configureWrapper, ElConfigEnum.values(), currentOutputDirPath);

            /*
                发生变化的jar文件信息
                key     新jar文件名称
                value   旧jar文件信息
             */
            Map<String, WriteDbData4JarInfo> modifiedJarMap = new HashMap<>();

            /*
                发生变化的方法的类所在的jar文件
                key     类名
                value   jar文件名称
             */
            Map<String, String> modifiedClassJarMap = new HashMap<>();

            for (Pair<WriteDbData4JarInfo, WriteDbData4JarInfo> pair : modifiedJarInfoList) {
                // 获取发生变化的jar文件
                recordModifiedJar(pair, modifiedJarMap, modifiedClassJarMap);
            }

            // 获得发生变化的方法
            Set<String> modifiedMethodSet = new HashSet<>();
            List<String> jarFileNameListNew = new ArrayList<>(modifiedJarMap.keySet());
            Collections.sort(jarFileNameListNew);
            for (String jarFileNameNew : jarFileNameListNew) {
                List<ModifiedMethodInfo> modifiedMethodInfoList = jarModifiedMethodInfoMap.get(jarFileNameNew);
                for (ModifiedMethodInfo modifiedMethodInfo : modifiedMethodInfoList) {
                    // 检查变化的方法是否需要跳过处理
                    if (!checkSkipModifiedMethod(modifiedMethodInfo.getFullMethod())) {
                        modifiedMethodSet.add(modifiedMethodInfo.getFullMethod());
                    }
                }
            }

            if (modifiedMethodSet.isEmpty()) {
                logger.info("未查询到发生变化的方法");
                return;
            }

            ConfigureWrapper configureWrapperNew = configureWrapper.copy();
            // 指定数据库表名后缀使用代表新的
            configureWrapperNew.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, JACGConstants.TABLE_SUFFIX_NEW);
            // 处理发生变化的jar文件
            boolean success = handleModifiedJarAndMethods(configureWrapperNew, modifiedJarMap, modifiedClassJarMap, modifiedMethodSet,
                    jarFileNameListNew, genAllCallGraphDir);
            if (!success) {
                recordTaskFail();
            }
        } catch (Exception e) {
            logger.error("error ", e);
            recordTaskFail();
        }
    }

    @Override
    protected boolean checkH2DbFile() {
        // 检查H2数据库文件是否可写，不允许文件不存在
        return checkH2DbFileWritable(false);
    }

    /**
     * 检查变化的方法是否需要跳过处理
     *
     * @param fullMethod
     * @return
     */
    private boolean checkSkipModifiedMethod(String fullMethod) {
        return elManager.checkJarDiffGenAllCallGraphIgnore(genCalleeGraph, fullMethod);
    }

    /**
     * 处理发生变化的jar文件与方法
     *
     * @param configureWrapperNew 配置包装类
     * @param modifiedJarMap      发生变化的jar文件信息
     * @param modifiedClassJarMap 发生变化的方法的类所在的jar文件
     * @param modifiedMethodSet   发生变化的方法Set
     * @param jarFileNameListNew  新jar文件名称列表
     * @param genAllCallGraphDir  生成完整方法调用链的目录
     * @return
     */
    protected abstract boolean handleModifiedJarAndMethods(ConfigureWrapper configureWrapperNew, Map<String, WriteDbData4JarInfo> modifiedJarMap,
                                                           Map<String, String> modifiedClassJarMap, Set<String> modifiedMethodSet, List<String> jarFileNameListNew,
                                                           String genAllCallGraphDir);

    /**
     * 选择保存用于需要比较的目录的枚举类
     *
     * @return
     */
    protected abstract OtherConfigFileUseListEnum chooseCompareDirEnum();

    /**
     * 查询新的发生改变的jar文件
     *
     * @return Pair.left 发生改变的新的jar文件信息 Pair.right 对应的旧jar文件信息，若为null则说明Pair.left是新增的jar文件
     */
    protected List<Pair<WriteDbData4JarInfo, WriteDbData4JarInfo>> queryModifiedJarInfo() {
        // 查询旧的jar文件信息
        List<WriteDbData4JarInfo> jarInfoListOld = queryJarInfo(true);
        // 查询新的jar文件信息
        List<WriteDbData4JarInfo> jarInfoListNew = queryJarInfo(false);
        if (JavaCG2Util.isCollectionEmpty(jarInfoListNew)) {
            logger.info("从新的目录中未查询到jar文件信息");
            return null;
        }

        List<Pair<WriteDbData4JarInfo, WriteDbData4JarInfo>> modifiedJarInfoList = new ArrayList<>();
        // 遍历新的与旧的jar文件
        for (WriteDbData4JarInfo jarInfoNew : jarInfoListNew) {
            logger.info("处理新的jar文件 {}", jarInfoNew.getJarFileName());
            String jarFileNameHeadNew = jarInfoNew.getJarFileNameHead();
            String jarFileNameExtNew = jarInfoNew.getJarFileNameExt();
            boolean findOldJar = false;
            for (WriteDbData4JarInfo jarInfoOld : jarInfoListOld) {
                if (jarFileNameHeadNew.equals(jarInfoOld.getJarFileNameHead()) && jarFileNameExtNew.equals(jarInfoOld.getJarFileNameExt())) {
                    if (jarInfoNew.getJarFileHash().equals(jarInfoOld.getJarFileHash())) {
                        logger.info("找到对应的旧jar文件，内容未发生变化 {} {}", jarInfoNew.getJarFileName(), jarInfoOld.getJarFileName());
                        findOldJar = true;
                        continue;
                    }
                    logger.info("找到对应的旧jar文件，内容发生变化 {} {}", jarInfoNew.getJarFileName(), jarInfoOld.getJarFileName());
                    modifiedJarInfoList.add(new ImmutablePair<>(jarInfoNew, jarInfoOld));
                    findOldJar = true;
                    break;
                }
            }
            if (!findOldJar) {
                // 新jar文件未找到对应的旧jar文件，说明是新增的jar文件
                logger.info("未找到对应的旧jar文件 {}", jarInfoNew.getJarFileName());
                modifiedJarInfoList.add(new ImmutablePair<>(jarInfoNew, null));
            }
        }
        return modifiedJarInfoList;
    }

    // 获取发生变化的jar文件
    protected void recordModifiedJar(Pair<WriteDbData4JarInfo, WriteDbData4JarInfo> pair, Map<String, WriteDbData4JarInfo> modifiedJarMap,
                                     Map<String, String> modifiedClassJarMap) {
        WriteDbData4JarInfo jarInfoNew = pair.getLeft();
        WriteDbData4JarInfo jarInfoOld = pair.getRight();
        boolean oldJarExists = jarInfoOld != null;

        List<WriteDbData4ClassInfo> classInfoListNew = queryClassInfoNew(jarInfoNew.getJarNum());
        if (JavaCG2Util.isCollectionEmpty(classInfoListNew)) {
            logger.info("未找到jar文件对应的class {}", jarInfoNew.getJarFileName());
            return;
        }
        List<ModifiedMethodInfo> modifiedMethodInfoList = new ArrayList<>();
        for (WriteDbData4ClassInfo classInfoNew : classInfoListNew) {
            boolean isClassMyBatisMapper = mybatisMsFormatedSqlHandler.checkClassMyBatisMapper(classInfoNew.getClassName(), chooseTableSuffixNew());
            boolean oldClassExists = oldJarExists;
            if (oldJarExists) {
                // 存在对应的旧jar文件，查询旧的class的HASH
                String oldClassFileHash = queryClassFileHashOld(jarInfoOld.getJarNum(), classInfoNew.getClassName());
                if (oldClassFileHash == null) {
                    logger.debug("不存在旧的class {} {}", jarInfoNew.getJarFileName(), classInfoNew.getClassName());
                    oldClassExists = false;
                } else if (oldClassFileHash.equals(classInfoNew.getClassFileHash()) && !isClassMyBatisMapper) {
                    logger.debug("class文件HASH没有变化，且不是MyBatis Mapper，跳过 {} {}", jarInfoNew.getJarFileName(), classInfoNew.getClassName());
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
                boolean oldMethodExists = false;
                if (oldClassExists) {
                    // 检查方法是否发生变化
                    Boolean checkResult = checkMethodModified(isClassMyBatisMapper, methodInfoNew, jarInfoNew);
                    if (checkResult == null) {
                        // 旧class存在且旧方法未发生变化时跳过
                        continue;
                    }
                    oldMethodExists = checkResult;
                }
                ModifiedMethodInfo modifiedMethodInfo = new ModifiedMethodInfo(methodInfoNew.getFullMethod(), methodInfoNew.getReturnType(), oldMethodExists);
                // 检查是否需要跳过发生变化的方法
                if (!checkSkipModifiedMethod(modifiedMethodInfo.getFullMethod())) {
                    logger.info("找到发生变化的方法 {} {}", jarInfoNew.getJarFileName(), methodInfoNew.getFullMethod());
                    // 方法指令有变化，或旧方法不存在，记录对应方法
                    modifiedMethodInfoList.add(modifiedMethodInfo);
                }
            }
        }
        if (modifiedMethodInfoList.isEmpty()) {
            return;
        }
        // 记录发生变化的jar文件及方法信息
        WriteDbData4JarInfo usedJarInfoOld;
        if (oldJarExists) {
            usedJarInfoOld = jarInfoOld;
        } else {
            usedJarInfoOld = new WriteDbData4JarInfo();
            usedJarInfoOld.setJarFileName("");
            usedJarInfoOld.setJarFileNameHead(jarInfoNew.getJarFileNameHead());
        }
        modifiedJarMap.put(jarInfoNew.getJarFileName(), usedJarInfoOld);
        jarModifiedMethodInfoMap.put(jarInfoNew.getJarFileName(), modifiedMethodInfoList);
    }

    /**
     * 检查方法是否发生变化
     *
     * @param isClassMyBatisMapper
     * @param methodInfoNew
     * @param jarInfoNew
     * @return null: 方法未发生变化 FALSE: 方法发生变化，且旧方法不存在 TRUE: 方法发生变化，且旧方法存在
     */
    private Boolean checkMethodModified(boolean isClassMyBatisMapper, WriteDbData4MethodInfo methodInfoNew, WriteDbData4JarInfo jarInfoNew) {
        String newMethodHash;
        String oldMethodHash = null;
        String newMethodMyBatisResultMapHash = null;
        String oldMethodMyBatisResultMapHash = null;
        if (isClassMyBatisMapper) {
            // MyBatis Mapper，使用对应XML中的sql语句的HASH作为方法HASH
            WriteDbData4MyBatisMSFormatedSql myBatisMSFormatedSql = mybatisMsFormatedSqlHandler.queryMapperSqlHash(methodInfoNew.getClassName(), methodInfoNew.getMethodName(),
                    chooseTableSuffixNew());
            newMethodHash = myBatisMSFormatedSql.getSqlHash();
            newMethodMyBatisResultMapHash = myBatisMSFormatedSql.getResultMapHash();
        } else {
            newMethodHash = methodInfoNew.getMethodInstructionsHash();
        }
        if (StringUtils.isBlank(newMethodHash) || JavaCG2CommonNameConstants.METHOD_NAME_CLINIT.equals(methodInfoNew.getMethodName())) {
            // 新方法的方法指令HASH为空，或者方法名为<clinit>（不会被直接调用），跳过
            logger.debug("跳过当前方法的处理 {} {}", jarInfoNew.getJarFileName(), methodInfoNew.getFullMethod());
            return null;
        }

        // 查询旧的方法的HASH
        if (isClassMyBatisMapper) {
            // MyBatis Mapper，使用对应XML中的sql语句的HASH作为方法HASH
            WriteDbData4MyBatisMSFormatedSql myBatisMSFormatedSql = mybatisMsFormatedSqlHandler.queryMapperSqlHash(methodInfoNew.getClassName(), methodInfoNew.getMethodName(),
                    chooseTableSuffixOld());
            if (myBatisMSFormatedSql != null) {
                oldMethodHash = myBatisMSFormatedSql.getSqlHash();
                oldMethodMyBatisResultMapHash = myBatisMSFormatedSql.getResultMapHash();
            }
        } else {
            oldMethodHash = queryMethodInstructionHashOld(methodInfoNew.getMethodHash());
        }
        if (oldMethodHash == null) {
            logger.debug("不存在旧的方法 {} {}", jarInfoNew.getJarFileName(), methodInfoNew.getFullMethod());
            return false;
        }
        if (!oldMethodHash.equals(newMethodHash)) {
            logger.debug("方法指令HASH发生变化，存在旧的方法 {} {}", jarInfoNew.getJarFileName(), methodInfoNew.getFullMethod());
            return true;
        }
        if (!StringUtils.equals(newMethodMyBatisResultMapHash, oldMethodMyBatisResultMapHash)) {
            logger.debug("方法指令HASH发生变化，属于返回resultMap的MyBatis Mapper方法（支持MySQL），存在旧的方法 {} {}", jarInfoNew.getJarFileName(), methodInfoNew.getFullMethod());
            return true;
        }
        if (StringUtils.isAllBlank(newMethodMyBatisResultMapHash, oldMethodMyBatisResultMapHash)) {
            logger.debug("方法指令HASH未发生变化，存在旧的方法 {} {}", jarInfoNew.getJarFileName(), methodInfoNew.getFullMethod());
        } else {
            logger.debug("方法指令HASH未发生变化，属于返回resultMap的MyBatis Mapper方法（支持MySQL），存在旧的方法 {} {}", jarInfoNew.getJarFileName(), methodInfoNew.getFullMethod());
        }
        return null;
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
                    " where " + DC.MI_METHOD_HASH + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, methodHash);
    }

    // 查询jar文件信息
    private List<WriteDbData4JarInfo> queryJarInfo(boolean old) {
        SqlKeyEnum sqlKeyEnum = old ? SqlKeyEnum.JI_QUERY_JAR_INFO_OLD : SqlKeyEnum.JI_QUERY_JAR_INFO_NEW;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_JAR_INFO) +
                    " from " + DbTableInfoEnum.DTIE_JAR_INFO.getTableName(appName, chooseTableSuffix(old)) +
                    " where " + DC.JI_JAR_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4JarInfo.class, JavaCG2Constants.FILE_KEY_JAR);
    }

    // 解析新旧目录的jar文件并写入数据库
    protected boolean writeDb(String dirPathOld, String dirPathNew) {
        if (!JavaCG2FileUtil.isDirectoryExists(dirPathOld, false)) {
            logger.error("配置文件中指定的需要比较的旧目录不存在 {} {}", compareDirEnum.getConfigPrintInfo(), dirPathOld);
            return false;
        }

        if (!JavaCG2FileUtil.isDirectoryExists(dirPathNew, false)) {
            logger.error("配置文件中指定的需要比较的新目录不存在 {} {}", compareDirEnum.getConfigPrintInfo(), dirPathNew);
            return false;
        }

        logger.info("解析旧目录中的jar文件并写入数据库 {}", dirPathOld);
        ConfigureWrapper configureWrapperOld = configureWrapper.copy();
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapperOld = javaCG2ConfigureWrapper.copy();
        configureWrapperOld.setMainConfig(ConfigKeyEnum.CKE_SKIP_WRITE_DB_WHEN_JAR_NOT_MODIFIED, Boolean.FALSE.toString());
        // 指定数据库表名后缀使用代表旧的
        configureWrapperOld.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, JACGConstants.TABLE_SUFFIX_OLD);
        javaCG2ConfigureWrapperOld.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, dirPathOld);
        if (!new RunnerWriteDb(javaCG2ConfigureWrapperOld, configureWrapperOld).run()) {
            return false;
        }

        logger.info("解析新目录中的jar文件并写入数据库 {}", dirPathNew);
        ConfigureWrapper configureWrapperNew = configureWrapper.copy();
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapperNew = javaCG2ConfigureWrapper.copy();
        configureWrapperNew.setMainConfig(ConfigKeyEnum.CKE_SKIP_WRITE_DB_WHEN_JAR_NOT_MODIFIED, Boolean.FALSE.toString());
        // 指定数据库表名后缀使用代表新的
        configureWrapperNew.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, JACGConstants.TABLE_SUFFIX_NEW);
        javaCG2ConfigureWrapperNew.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, dirPathNew);
        boolean success = new RunnerWriteDb(javaCG2ConfigureWrapperNew, configureWrapperNew).run();
        // 复制有使用的参数
        configureWrapperOld.baseCopyUsedConfigTo(configureWrapper);
        configureWrapperNew.baseCopyUsedConfigTo(configureWrapper);
        javaCG2ConfigureWrapperOld.baseCopyUsedConfigTo(javaCG2ConfigureWrapper);
        javaCG2ConfigureWrapperNew.baseCopyUsedConfigTo(javaCG2ConfigureWrapper);

        return success;
    }

    // 生成jar文件中发生变化的方法基本信息
    protected boolean writeModifiedMethodsBaseFile(String jarDiffDirPath, List<String> jarFileNameListNew, Map<String, WriteDbData4JarInfo> modifiedJarMap) {
        String modifiedMethodsBaseFilePath = jarDiffDirPath + File.separator + JACGConstants.FILE_JAR_DIFF_MODIFIED_METHODS_BASE;
        try (WriterSupportHeader modifiedMethodsBaseWriter = new WriterSupportHeader(modifiedMethodsBaseFilePath, FILE_HEADER_MODIFIED_METHODS_BASE)) {
            for (String jarFileNameNew : jarFileNameListNew) {
                WriteDbData4JarInfo jarInfoOld = modifiedJarMap.get(jarFileNameNew);
                List<ModifiedMethodInfo> modifiedMethodInfoList = jarModifiedMethodInfoMap.get(jarFileNameNew);
                for (ModifiedMethodInfo modifiedMethodInfo : modifiedMethodInfoList) {
                    modifiedMethodsBaseWriter.writeDataInLine(jarInfoOld.getJarFileNameHead(), jarFileNameNew, jarInfoOld.getJarFileName(), modifiedMethodInfo.getFullMethod(),
                            modifiedMethodInfo.isOldMethodExists() ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
        return textFileToExcel(modifiedMethodsBaseFilePath);
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

    /**
     * 需要生成excel文件
     *
     * @return
     */
    @Override
    protected boolean needGenerateExcel() {
        return true;
    }

    public void setSkipWriteDb(boolean skipWriteDb) {
        this.skipWriteDb = skipWriteDb;
    }

    public Map<String, List<ModifiedMethodInfo>> getJarModifiedMethodInfoMap() {
        return jarModifiedMethodInfoMap;
    }

    @Override
    public void setCurrentOutputDirPath(String currentOutputDirPath) {
        throw new JavaCG2RuntimeException(this.getClass().getName() + " 类不允许设置当前的输出目录");
    }
}
