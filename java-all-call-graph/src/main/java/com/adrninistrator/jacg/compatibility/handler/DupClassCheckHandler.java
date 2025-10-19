package com.adrninistrator.jacg.compatibility.handler;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.field.FieldInfoHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/8/4
 * @description: 检查重复同名类处理类
 */
public class DupClassCheckHandler extends BaseHandler implements QueryByPageCallBack<String> {

    private static final Logger logger = LoggerFactory.getLogger(DupClassCheckHandler.class);

    public static final String FILE_NAME_DUP_CLASS_INFO = "a.1.重复同名类信息" + JavaCG2Constants.EXT_MD;
    public static final String FILE_NAME_JAR_CONTAINS_DUP_CLASS = "a.2.包含重复同名类的jar文件" + JavaCG2Constants.EXT_MD;
    public static final String FILE_NAME_CONFLICT_PUBLIC_METHOD_FIELD = "a.3.重复同名类中仅部分类存在的public方法、字段" + JavaCG2Constants.EXT_MD;

    public static final String[] FILE_HEADER_ARRAY_DUP_CLASS_INFO = new String[]{
            "重复同名类序号",
            "类名",
            "class文件在jar文件中的路径",
            "class文件HASH",
            "是否存在多种class文件HASH",
            "class所在jar文件路径",
            "class所在jar文件的内部路径"
    };
    public static final String[] FILE_HEADER_ARRAY_JAR_CONTAINS_DUP_CLASS = new String[]{
            "包含重复同名类的jar文件对序号",
            "jar文件路径",
            "jar文件的内部路径"
    };
    public static final String[] FILE_HEADER_ARRAY_CONFLICT_PUBLIC_METHOD_FIELD = new String[]{
            "序号",
            "类型",
            "类名",
            "存在public方法或字段的class文件在jar文件中的路径",
            "存在public方法或字段的class文件所在jar文件路径",
            "存在public方法或字段的class文件所在jar文件内部路径",
            "public方法名及参数或字段名称",
            "public方法返回类型或字段类型",
            "不存在对应方法或字段的class文件在jar文件中的路径",
            "不存在对应方法或字段的class文件所在jar文件路径",
            "不存在对应方法或字段的class文件所在jar文件内部路径"
    };
    public static final String FILE_HEADER_DUP_CLASS_INFO = StringUtils.join(FILE_HEADER_ARRAY_DUP_CLASS_INFO, JavaCG2Constants.FLAG_TAB);
    public static final String FILE_HEADER_JAR_CONTAINS_DUP_CLASS = StringUtils.join(FILE_HEADER_ARRAY_JAR_CONTAINS_DUP_CLASS, JavaCG2Constants.FLAG_TAB);
    public static final String FILE_HEADER_CONFLICT_PUBLIC_METHOD_FIELD = StringUtils.join(FILE_HEADER_ARRAY_CONFLICT_PUBLIC_METHOD_FIELD, JavaCG2Constants.FLAG_TAB);

    private final ClassInfoHandler classInfoHandler;
    private final MethodInfoHandler methodInfoHandler;
    private final FieldInfoHandler fieldInfoHandler;

    // 代表存在相同的重复同名类的jar文件序号
    private final Set<String> dupClassInJarNumSet = new HashSet<>();

    // 代表当前查询的重复同名类类名的参数
    private String currentDupClassName;
    // 代表当前处理的类序号
    private int classSeq = 0;

    private String currentOutputDirPath;

    private Map<Integer, WriteDbData4JarInfo> jarInfoMap;

    private WriterSupportHeader writer4DupClassInfo;
    private WriterSupportHeader writer4JarContainsDupClass;
    private WriterSupportHeader writer4ConflictPublicMethodField;

    public DupClassCheckHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        fieldInfoHandler = new FieldInfoHandler(dbOperWrapper);
    }

    // 检查重复同名类
    public boolean check() {
        String outputFilePath4DupClassInfo;
        String outputFilePath4JarContainsDupClass;
        String outputFilePath4ConflictPublicMethodField;
        try {
            outputFilePath4DupClassInfo = currentOutputDirPath + File.separator + FILE_NAME_DUP_CLASS_INFO;
            outputFilePath4JarContainsDupClass = currentOutputDirPath + File.separator + FILE_NAME_JAR_CONTAINS_DUP_CLASS;
            outputFilePath4ConflictPublicMethodField = currentOutputDirPath + File.separator + FILE_NAME_CONFLICT_PUBLIC_METHOD_FIELD;
            writer4DupClassInfo = new WriterSupportHeader(outputFilePath4DupClassInfo, FILE_HEADER_DUP_CLASS_INFO);
            writer4JarContainsDupClass = new WriterSupportHeader(outputFilePath4JarContainsDupClass, FILE_HEADER_JAR_CONTAINS_DUP_CLASS);
            writer4ConflictPublicMethodField = new WriterSupportHeader(outputFilePath4ConflictPublicMethodField, FILE_HEADER_CONFLICT_PUBLIC_METHOD_FIELD);

            if (!QueryByPageHandler.queryAndHandle(this, 0)) {
                return false;
            }

            // 记录包含重复同名类的jar文件
            recordJarContainsDupClass();
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        } finally {
            IOUtils.closeQuietly(writer4DupClassInfo);
            IOUtils.closeQuietly(writer4JarContainsDupClass);
            IOUtils.closeQuietly(writer4ConflictPublicMethodField);
        }
        return textFileToExcel(outputFilePath4DupClassInfo)
                && textFileToExcel(outputFilePath4JarContainsDupClass)
                && textFileToExcel(outputFilePath4ConflictPublicMethodField);
    }

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        // 当前方法的返回值不需要使用，返回任意值
        return 0;
    }

    @Override
    public List<String> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        List<String> list;
        if (currentDupClassName == null) {
            // 首次查询
            String sql = "select distinct(" + DC.CI_CLASS_NAME + ")" +
                    " from " + DbTableInfoEnum.DTIE_DUP_CLASS_INFO.getTableName() +
                    " order by " + DC.CI_CLASS_NAME +
                    " limit ?";
            String finalSql = dbOperWrapper.formatSql(sql);
            list = dbOperator.queryListOneColumn(finalSql, String.class, JACGConstants.DB_PAGE_HANDLE_SIZE);
        } else {
            // 非首次查询
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.DCI_QUERY_CLASS_NAME_BY_PAGE;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "select distinct(" + DC.CI_CLASS_NAME + ")" +
                        " from " + DbTableInfoEnum.DTIE_DUP_CLASS_INFO.getTableName() +
                        " where " + DC.CI_CLASS_NAME + " > ?" +
                        " order by " + DC.CI_CLASS_NAME +
                        " limit ?";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }
            list = dbOperator.queryListOneColumn(sql, String.class, currentDupClassName, JACGConstants.DB_PAGE_HANDLE_SIZE);
        }
        if (!JavaCG2Util.isCollectionEmpty(list)) {
            // 查询结果非空，修改当前查询的重复同名类名
            currentDupClassName = list.get(list.size() - 1);
        }
        return list;
    }

    @Override
    public boolean handleDataList(List<String> dataList, Object... argsByPage) throws Exception {
        JavaCG2Counter methodFieldSeq = new JavaCG2Counter();
        for (String dupClassName : dataList) {
            // 当前处理的类序号加1
            classSeq++;
            // 查询当前类全部的重复同名类信息
            List<WriteDbData4ClassInfo> dupClassInfoList = classInfoHandler.queryDupClasByClassName(dupClassName);
            // 查询类信息
            WriteDbData4ClassInfo classInfo = classInfoHandler.queryClassInfoByClassName(dupClassName);
            // 保存相关重复的类的文件HASH
            Set<String> dupClassHash = new HashSet<>();
            dupClassHash.add(classInfo.getClassFileHash());
            // 保存相关重复的类的jar文件序号
            Set<Integer> jarNumSet = new HashSet<>();
            jarNumSet.add(classInfo.getJarNum());
            for (WriteDbData4ClassInfo dupClassInfo : dupClassInfoList) {
                dupClassHash.add(dupClassInfo.getClassFileHash());
                jarNumSet.add(dupClassInfo.getJarNum());

                if (dupClassInfo.getClassFileHash().equals(classInfo.getClassFileHash())) {
                    // 重复同名类与原始类文件HASH相同，跳过
                    continue;
                }

                // 处理重复同名类中的方法
                handleMethodInDupClass(dupClassInfo, classInfo, methodFieldSeq);

                // 处理重复同名类中的字段
                handleFieldInDupClass(dupClassInfo, classInfo, methodFieldSeq);
            }

            boolean existsMultiHash = dupClassHash.size() > 1;

            // 记录重复类信息
            recordDupClassInfo(classSeq, classInfo, existsMultiHash);
            for (WriteDbData4ClassInfo dupClassInfo : dupClassInfoList) {
                recordDupClassInfo(classSeq, dupClassInfo, existsMultiHash);
            }

            // 记录相关重复的类的jar文件序号
            List<Integer> jarNumList = new ArrayList<>(jarNumSet);
            Collections.sort(jarNumList);
            String jarNumStr = StringUtils.join(jarNumList, JavaCG2Constants.FLAG_TAB);
            dupClassInJarNumSet.add(jarNumStr);
        }

        return true;
    }

    // 查询结果为空时需要结束循环查询
    @Override
    public boolean exitWhenQueryEmpty() {
        return true;
    }

    // 记录重复类信息
    private void recordDupClassInfo(int classSeq, WriteDbData4ClassInfo dupClassInfo, boolean existsMultiHash) throws IOException {
        WriteDbData4JarInfo jarInfo = jarInfoMap.get(dupClassInfo.getJarNum());
        writer4DupClassInfo.writeDataInLine(String.valueOf(classSeq),
                dupClassInfo.getClassName(),
                dupClassInfo.getClassPathInJar(),
                dupClassInfo.getClassFileHash(),
                JavaCG2YesNoEnum.parseDesc(existsMultiHash),
                jarInfo.getJarFullPath(),
                jarInfo.getInnerJarPath());
    }

    // 处理重复同名类中的方法
    private void handleMethodInDupClass(WriteDbData4ClassInfo dupClassInfo, WriteDbData4ClassInfo classInfo, JavaCG2Counter methodFieldSeq) throws IOException {
        // 比较public方法在重复同名类与原始类中是否都有定义
        Set<String> method4CompatibilitySet = new HashSet<>();
        Set<String> dupMethod4CompatibilitySet = new HashSet<>();
        List<WriteDbData4MethodInfo> dupMethodInfoList = methodInfoHandler.queryDupMethodInfoByClass(dupClassInfo.getJarNum(), classInfo.getClassName());
        for (WriteDbData4MethodInfo dupMethodInfo : dupMethodInfoList) {
            if (!JavaCG2ByteCodeUtil.isPublicFlag(dupMethodInfo.getAccessFlags())) {
                continue;
            }
            String dupMethodCompatibilityStr = JACGClassMethodUtil.genMethodCompatibilityStr(dupMethodInfo);
            dupMethod4CompatibilitySet.add(dupMethodCompatibilityStr);
        }

        List<WriteDbData4MethodInfo> methodInfoList = methodInfoHandler.queryMethodInfoByClass(classInfo.getClassName());
        for (WriteDbData4MethodInfo methodInfo : methodInfoList) {
            if (!JavaCG2ByteCodeUtil.isPublicFlag(methodInfo.getAccessFlags())) {
                continue;
            }
            String methodCompatibilityStr = JACGClassMethodUtil.genMethodCompatibilityStr(methodInfo);
            method4CompatibilitySet.add(methodCompatibilityStr);
            if (!dupMethod4CompatibilitySet.contains(methodCompatibilityStr)) {
                // 记录仅在部分重复同名类存在的方法，原始类中存在的public方法在重复同名类中未找到
                recordConflictMethod(methodFieldSeq.addAndGet(), methodInfo, classInfo, dupClassInfo);
            }
        }
        for (WriteDbData4MethodInfo dupMethodInfo : dupMethodInfoList) {
            if (!JavaCG2ByteCodeUtil.isPublicFlag(dupMethodInfo.getAccessFlags())) {
                continue;
            }
            String dupMethodCompatibilityStr = JACGClassMethodUtil.genMethodCompatibilityStr(dupMethodInfo);
            if (!method4CompatibilitySet.contains(dupMethodCompatibilityStr)) {
                // 记录仅在部分重复同名类存在的方法，重复同名类中存在的public方法在原始类中未找到
                recordConflictMethod(methodFieldSeq.addAndGet(), dupMethodInfo, dupClassInfo, classInfo);
            }
        }
    }

    // 处理重复同名类中的字段
    private void handleFieldInDupClass(WriteDbData4ClassInfo dupClassInfo, WriteDbData4ClassInfo classInfo, JavaCG2Counter methodFieldSeq) throws IOException {
        // 比较public字段在重复同名类与原始类中是否都有定义
        Set<String> field4CompatibilitySet = new HashSet<>();
        Set<String> dupField4CompatibilitySet = new HashSet<>();
        List<WriteDbData4FieldInfo> dupFieldInfoList = fieldInfoHandler.queryDupFieldInfoByClassName(dupClassInfo.getJarNum(), classInfo.getClassName());
        for (WriteDbData4FieldInfo dupFieldInfo : dupFieldInfoList) {
            if (!JavaCG2ByteCodeUtil.isPublic(dupFieldInfo.getModifiers())) {
                continue;
            }
            String dupFieldCompatibilityStr = JACGClassMethodUtil.genFieldCompatibilityStr(dupFieldInfo);
            dupField4CompatibilitySet.add(dupFieldCompatibilityStr);
        }

        List<WriteDbData4FieldInfo> fieldInfoList = fieldInfoHandler.queryFieldInfoByClassName(classInfo.getClassName());
        for (WriteDbData4FieldInfo fieldInfo : fieldInfoList) {
            if (!JavaCG2ByteCodeUtil.isPublic(fieldInfo.getModifiers())) {
                continue;
            }
            String fieldCompatibilityStr = JACGClassMethodUtil.genFieldCompatibilityStr(fieldInfo);
            field4CompatibilitySet.add(fieldCompatibilityStr);
            if (!dupField4CompatibilitySet.contains(fieldCompatibilityStr)) {
                // 记录仅在部分重复同名类存在的字段，原始类中存在的public字段在重复同名类中未找到
                recordConflictField(methodFieldSeq.addAndGet(), fieldInfo, classInfo, dupClassInfo);
            }
        }

        for (WriteDbData4FieldInfo dupFieldInfo : dupFieldInfoList) {
            if (!JavaCG2ByteCodeUtil.isPublic(dupFieldInfo.getModifiers())) {
                continue;
            }
            String dupFieldCompatibilityStr = JACGClassMethodUtil.genFieldCompatibilityStr(dupFieldInfo);
            if (!field4CompatibilitySet.contains(dupFieldCompatibilityStr)) {
                // 记录仅在部分重复同名类存在的字段，重复同名类中存在的public字段在原始类中未找到
                recordConflictField(methodFieldSeq.addAndGet(), dupFieldInfo, dupClassInfo, classInfo);
            }
        }
    }

    // 记录包含重复同名类的jar文件
    private void recordJarContainsDupClass() throws IOException {
        int seq = 0;
        for (String dupClassInJarNum : dupClassInJarNumSet) {
            seq++;
            String[] jarNumArray = dupClassInJarNum.split(JavaCG2Constants.FLAG_TAB);
            for (String jarNumStr : jarNumArray) {
                int jarNum = Integer.parseInt(jarNumStr);
                WriteDbData4JarInfo jarInfo = jarInfoMap.get(jarNum);
                writer4JarContainsDupClass.writeDataInLine(String.valueOf(seq), jarInfo.getJarFullPath(), jarInfo.getInnerJarPath());
            }
        }
    }

    // 记录仅在部分重复同名类存在的方法
    private void recordConflictMethod(int seq, WriteDbData4MethodInfo existsMethodInfo, WriteDbData4ClassInfo existsClassInfo, WriteDbData4ClassInfo notExistsClassInfo) throws IOException {
        WriteDbData4JarInfo existsJarInfo = jarInfoMap.get(existsMethodInfo.getJarNum());
        WriteDbData4JarInfo notExistsJarInfo = jarInfoMap.get(notExistsClassInfo.getJarNum());
        String methodNameWithArgTypes = JACGClassMethodUtil.getMethodNameWithArgsFromFull(existsMethodInfo.getFullMethod());

        writer4ConflictPublicMethodField.writeDataInLine(
                String.valueOf(seq),
                JACGConstants.DESC_METHOD,
                existsMethodInfo.getClassName(),
                existsClassInfo.getClassPathInJar(),
                existsJarInfo.getJarFullPath(),
                existsJarInfo.getInnerJarPath(),
                methodNameWithArgTypes,
                existsMethodInfo.getReturnType(),
                notExistsClassInfo.getClassPathInJar(),
                notExistsJarInfo.getJarFullPath(),
                notExistsJarInfo.getInnerJarPath()
        );
    }

    // 记录仅在部分重复同名类存在的字段
    private void recordConflictField(int seq, WriteDbData4FieldInfo existsFieldInfo, WriteDbData4ClassInfo existsClassInfo, WriteDbData4ClassInfo notExistsClassInfo) throws IOException {
        WriteDbData4JarInfo existsJarInfo = jarInfoMap.get(existsFieldInfo.getJarNum());
        WriteDbData4JarInfo notExistsJarInfo = jarInfoMap.get(notExistsClassInfo.getJarNum());

        writer4ConflictPublicMethodField.writeDataInLine(
                String.valueOf(seq),
                JACGConstants.DESC_FIELD,
                existsFieldInfo.getClassName(),
                existsClassInfo.getClassPathInJar(),
                existsJarInfo.getJarFullPath(),
                existsJarInfo.getInnerJarPath(),
                existsFieldInfo.getFieldName(),
                existsFieldInfo.getFieldType(),
                notExistsClassInfo.getClassPathInJar(),
                notExistsJarInfo.getJarFullPath(),
                notExistsJarInfo.getInnerJarPath()
        );
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

    public void setCurrentOutputDirPath(String currentOutputDirPath) {
        this.currentOutputDirPath = currentOutputDirPath;
    }

    public void setJarInfoMap(Map<Integer, WriteDbData4JarInfo> jarInfoMap) {
        this.jarInfoMap = jarInfoMap;
    }
}
