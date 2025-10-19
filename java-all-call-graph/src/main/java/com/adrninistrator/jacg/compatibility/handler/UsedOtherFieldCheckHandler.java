package com.adrninistrator.jacg.compatibility.handler;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.compatibility.CompatibilityHandlerDto;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldUsageOther;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.handler.dto.classes.ClassNameAndType;
import com.adrninistrator.jacg.handler.field.FieldInfoHandler;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/8/6
 * @description: 检查被其他类使用的字段的处理类
 */
public class UsedOtherFieldCheckHandler extends BaseCompatibilityCheckHandler implements QueryByPageCallBack<String> {
    private static final Logger logger = LoggerFactory.getLogger(UsedOtherFieldCheckHandler.class);

    public static final String FILE_NAME = "c.被使用字段存在问题" + JavaCG2Constants.EXT_MD;
    public static final String[] FILE_HEADER_ARRAY = new String[]{
            "问题描述",
            "使用字段的方法",
            "使用字段的方法返回类型",
            "使用字段的类名",
            "使用字段代码行号",
            "使用字段的类所在jar文件路径",
            "使用字段的类所在jar文件内部路径",
            "被使用字段所在类",
            "被使用字段名称",
            "被使用字段类型",
            "被使用字段所在jar文件路径",
            "被使用字段所在jar文件内部路径"
    };
    public static final String FILE_HEADER = StringUtils.join(FILE_HEADER_ARRAY, JavaCG2Constants.FLAG_TAB);

    private WriterSupportHeader writer;

    // 代表当前查询的唯一类名
    private String currentFieldInSimpleClassName = null;

    public UsedOtherFieldCheckHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    // 检查被其他类使用的字段
    public boolean check() {
        String outputFilePath;
        try {
            outputFilePath = currentOutputDirPath + File.separator + FILE_NAME;
            writer = new WriterSupportHeader(outputFilePath, FILE_HEADER);
            if (!QueryByPageHandler.queryAndHandle(this, 0)) {
                return false;
            }
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        } finally {
            IOUtils.closeQuietly(writer);
        }
        return textFileToExcel(outputFilePath);
    }

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        // 当前方法的返回值不需要使用，返回任意值
        return 0;
    }

    @Override
    public List<String> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        List<String> list;
        if (currentFieldInSimpleClassName == null) {
            // 首次查询
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FUO_QUERY_SIMPLE_CLASS_NAME_FIRST;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "select distinct(" + DC.FUO_FIELD_IN_SIMPLE_CLASS_NAME + ")" +
                        " from " + DbTableInfoEnum.DTIE_FIELD_USAGE_OTHER.getTableName() +
                        " order by " + DC.FUO_FIELD_IN_SIMPLE_CLASS_NAME +
                        " limit ?";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }
            list = dbOperator.queryListOneColumn(sql, String.class, JACGConstants.DB_PAGE_HANDLE_SIZE);
        } else {
            // 非首次查询
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FUO_QUERY_SIMPLE_CLASS_NAME_BY_PAGE;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "select distinct(" + DC.FUO_FIELD_IN_SIMPLE_CLASS_NAME + ")" +
                        " from " + DbTableInfoEnum.DTIE_FIELD_USAGE_OTHER.getTableName() +
                        " where " + DC.FUO_FIELD_IN_SIMPLE_CLASS_NAME + " > ?" +
                        " order by " + DC.FUO_FIELD_IN_SIMPLE_CLASS_NAME +
                        " limit ?";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }
            list = dbOperator.queryListOneColumn(sql, String.class, currentFieldInSimpleClassName, JACGConstants.DB_PAGE_HANDLE_SIZE);
        }

        if (!JavaCG2Util.isCollectionEmpty(list)) {
            // 查询结果非空，修改当前查询的唯一类名
            currentFieldInSimpleClassName = list.get(list.size() - 1);
        }
        return list;
    }

    @Override
    public boolean handleDataList(List<String> dataList, Object... argsByPage) throws Exception {
        for (String fieldInSimpleClassName : dataList) {
            // 查询字段在其他类被使用的类完整类名
            String fieldInClassName = dbOperWrapper.queryClassNameBySimple(fieldInSimpleClassName);

            // 处理一个字段在其他类被使用的类
            handleOneClass(fieldInClassName, fieldInSimpleClassName);
        }
        return true;
    }

    // 查询结果为空时需要结束循环查询
    @Override
    public boolean exitWhenQueryEmpty() {
        return true;
    }

    // 处理一个字段被其他类使用的类
    private void handleOneClass(String fieldInClassName, String fieldInSimpleClassName) throws IOException {
        // 检查类是否存在
        boolean calleeClassExists = checkClassExists(fieldInClassName);
        Integer startRecordId = null;
        while (true) {
            // 分页查询被其他类使用的字段使用情况
            List<WriteDbData4FieldUsageOther> fieldUsageOtherList = fieldInfoHandler.queryFieldUsageOtherByPageSCN(fieldInSimpleClassName, startRecordId,
                    JACGConstants.DB_PAGE_HANDLE_SIZE);
            if (fieldUsageOtherList.isEmpty()) {
                break;
            }
            // 修改起始call_id为本次查询到的最后一条
            startRecordId = fieldUsageOtherList.get(fieldUsageOtherList.size() - 1).getRecordId();
            if (!calleeClassExists) {
                // 类不存在
                for (WriteDbData4FieldUsageOther fieldUsageOther : fieldUsageOtherList) {
                    // 记录存在问题的被使用字段
                    recordErrorUsedField(fieldUsageOther, "被使用字段所在的类未找到");
                }
            } else {
                // 类存在
                for (WriteDbData4FieldUsageOther fieldUsageOther : fieldUsageOtherList) {
                    // 检查被使用的字段是否存在
                    WriteDbData4FieldInfo fieldInfo = checkUsedFieldExists(fieldUsageOther);
                    if (fieldInfo == null) {
                        continue;
                    }

                    // 检查存在的字段
                    checkExistedField(fieldUsageOther, fieldInfo);
                }
            }

            if (fieldUsageOtherList.size() < JACGConstants.DB_PAGE_HANDLE_SIZE) {
                break;
            }
        }
    }

    /**
     * 检查被使用字段是否存在
     *
     * @param fieldUsageOther 使用其他类中字段的使用情况对象
     * @return
     */
    private WriteDbData4FieldInfo checkUsedFieldExists(WriteDbData4FieldUsageOther fieldUsageOther) throws IOException {
        List<String> classList = new ArrayList<>();
        // 从当前类到父类/接口开始尝试，判断对应字段是否存在
        classList.add(fieldUsageOther.getFieldInClassName());
        for (CompatibilityHandlerDto compatibilityHandlerDto : compatibilityHandlerDtoList) {
            FieldInfoHandler tmpFieldInfoHandler = compatibilityHandlerDto.getFieldInfoHandler();
            List<ClassNameAndType> superClassNameAndTypeList = new ArrayList<>();
            for (String className : classList) {
                // 查询字段，若在当前类中未查找到，则在父类与接口中查找
                WriteDbData4FieldInfo fieldInfo = tmpFieldInfoHandler.queryFieldExactSuperInterface(className, fieldUsageOther.getFieldName(), fieldUsageOther.getFieldType(),
                        superClassNameAndTypeList);
                if (fieldInfo != null) {
                    return fieldInfo;
                }
            }
            // 添加父类/接口
            for (ClassNameAndType classNameAndType : superClassNameAndTypeList) {
                if (!classList.contains(classNameAndType.getClassName())) {
                    classList.add(classNameAndType.getClassName());
                }
            }
        }
        // 记录存在问题的被使用字段
        recordErrorUsedField(fieldUsageOther, "未找到被使用的字段");
        return null;
    }

    // 检查存在的字段
    private void checkExistedField(WriteDbData4FieldUsageOther fieldUsageOther, WriteDbData4FieldInfo fieldInfo) throws IOException {
        String callerClassName = fieldUsageOther.getClassName();
        String calleeClassName = fieldUsageOther.getFieldInClassName();
        // 检查字段修饰符
        if (JavaCG2ByteCodeUtil.isPrivate(fieldInfo.getModifiers())) {
            // 检查是否为当前类调用，或内部类调用
            if (!callerClassName.equals(calleeClassName)
                    && !classInfoHandler.checkInnerClassAny(callerClassName, calleeClassName)) {
                // 记录存在问题的被使用字段
                recordErrorUsedField(fieldUsageOther, "被使用的字段为private字段，无法被其他类调用");
            }
        } else if (JavaCG2ByteCodeUtil.isProtected(fieldInfo.getModifiers())) {
            if (!callerClassName.equals(calleeClassName)
                    && !JavaCG2ClassMethodUtil.checkSamePackage(fieldInfo.getClassName(), callerClassName)
                    && !checkExtendsImplClass(calleeClassName, callerClassName)
                    && !checkExtendsImplClass(fieldInfo.getClassName(), callerClassName)) {
                // 记录存在问题的被使用字段
                recordErrorUsedField(fieldUsageOther, "被使用的字段为protected字段，调用字段不满足以下任意情况：1、相同的类，2、相同包中的类，3、子类或实现类");
            }
        } else if (!JavaCG2ByteCodeUtil.isPublic(fieldInfo.getModifiers())) {
            if (!callerClassName.equals(calleeClassName)
                    && !JavaCG2ClassMethodUtil.checkSamePackage(calleeClassName, callerClassName)) {
                // 记录存在问题的被使用字段
                recordErrorUsedField(fieldUsageOther, "被使用的字段修饰符为default，调用字段不满足以下任意情况：1、相同的类，2、相同包中的类");
            }
        }

        // 检查字段是否静态/非静态
        if (JavaCG2YesNoEnum.isYes(fieldUsageOther.getStaticFlag()) && !JavaCG2YesNoEnum.isYes(fieldInfo.getStaticFlag())) {
            // 记录存在问题的被使用字段
            recordErrorUsedField(fieldUsageOther, "被使用的字段编译时属于静态字段，但目前是非静态字段");
        } else if (!JavaCG2YesNoEnum.isYes(fieldUsageOther.getStaticFlag()) && JavaCG2YesNoEnum.isYes(fieldInfo.getStaticFlag())) {
            // 记录存在问题的被使用字段
            recordErrorUsedField(fieldUsageOther, "被使用的字段编译时属于非静态字段，但目前是静态字段");
        }
    }

    // 记录存在问题的被使用字段
    private void recordErrorUsedField(WriteDbData4FieldUsageOther fieldUsageOther, String reason) throws IOException {
        WriteDbData4JarInfo classJarInfo = jarInfoMap.get(fieldUsageOther.getClassJarNum());
        String fieldJarPath = "";
        String fieldJarInnerPath = "";
        if (fieldUsageOther.getFieldJarNum() != null) {
            WriteDbData4JarInfo calleeJarInfo = jarInfoMap.get(fieldUsageOther.getFieldJarNum());
            fieldJarPath = calleeJarInfo.getJarFullPath();
            fieldJarInnerPath = calleeJarInfo.getInnerJarPath();
        }
        writer.writeDataInLine(
                reason,
                fieldUsageOther.getFullMethod(),
                fieldUsageOther.getMethodReturnType(),
                fieldUsageOther.getClassName(),
                String.valueOf(fieldUsageOther.getLineNumber()),
                classJarInfo.getJarFullPath(),
                classJarInfo.getInnerJarPath(),
                fieldUsageOther.getFieldInClassName(),
                fieldUsageOther.getFieldName(),
                fieldUsageOther.getFieldType(),
                fieldJarPath,
                fieldJarInnerPath
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
}
