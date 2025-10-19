package com.adrninistrator.jacg.handler.field;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldUsageOther;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.classes.ClassNameAndType;
import com.adrninistrator.jacg.handler.dto.field.CommonFieldInfoInClass;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.dto.field.FieldTypeAndName;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.ArrayUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/1/11
 * @description: 字段信息处理类
 */
public class FieldInfoHandler extends BaseHandler {

    private final AnnotationHandler annotationHandler;
    private final JACGExtendsImplHandler jacgExtendsImplHandler;

    public FieldInfoHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
        jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    public FieldInfoHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
        jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    /**
     * 查询指定类中，字段类型属于指定的包的字段（支持排除特定的类型），且非public、非static、非final的字段信息
     *
     * @param className
     * @return
     */
    public List<WriteDbData4FieldInfo> queryClassFieldsByPackageExcludePSF(String className, String typePackage, String... excludedTypes) {
        int excludedTypeNum;
        if (ArrayUtils.isEmpty(excludedTypes)) {
            excludedTypeNum = 0;
        } else {
            excludedTypeNum = excludedTypes.length;
        }
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FI_QUERY_BY_CLASS_PACKAGE_EXCLUDE_PSF;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum, excludedTypeNum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_INFO.getTableName() +
                    " where " + DC.FI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FI_FIELD_TYPE_NAD + " like concat(?, '%')" +
                    " and " + DC.FI_MODIFIERS + " != ?" +
                    " and " + DC.FI_STATIC_FLAG + " = ?" +
                    " and " + DC.FI_FINAL_FLAG + " = ?";
            if (excludedTypeNum > 0) {
                sql = sql + " and " + DC.FI_FIELD_TYPE_NAD + " not in " + JACGSqlUtil.genQuestionString(excludedTypeNum);
            }
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql, excludedTypeNum);
        }
        List<Object> argList = new ArrayList<>(excludedTypeNum + 2);
        argList.add(dbOperWrapper.querySimpleClassName(className));
        argList.add(typePackage);
        argList.add(JavaCG2CommonNameConstants.MODIFIERS_PUBLIC);
        argList.add(JavaCG2YesNoEnum.NO.getIntValue());
        argList.add(JavaCG2YesNoEnum.NO.getIntValue());
        if (excludedTypeNum > 0) {
            argList.addAll(Arrays.asList(excludedTypes));
        }
        return dbOperator.queryList(sql, WriteDbData4FieldInfo.class, argList.toArray());
    }

    /**
     * 查询指定类中，类型属于指定的包的字段，排除特定的类型
     *
     * @param className
     * @return
     */
    public List<WriteDbData4FieldInfo> queryClassCustomTypeFields(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FI_QUERY_BY_CLASS_CUSTOM_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_INFO.getTableName() +
                    " where " + DC.FI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FI_FIELD_TYPE_NAD + " not like concat(?, '%')" +
                    " and " + DC.FI_PRIMITIVE_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4FieldInfo.class, dbOperWrapper.querySimpleClassName(className), JavaCG2CommonNameConstants.PACKAGE_JAVA,
                JavaCG2YesNoEnum.NO.getIntValue());
    }

    /**
     * 查询指定类指定字段中涉及的泛型类型
     *
     * @param className
     * @param fieldName
     * @return
     */
    public List<WriteDbData4FieldGenericsType> queryFieldGenericsTypeByClassFieldName(String className, String fieldName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FGT_QUERY_BY_CLASS_FIELD_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_GENERICS_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_GENERICS_TYPE.getTableName() +
                    " where " + DC.FGT_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FGT_FIELD_NAME + " = ?" +
                    " and " + DC.FGT_TYPE + " = ?" +
                    " order by " + DC.FGT_TYPE_SEQ;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4FieldGenericsType.class, dbOperWrapper.querySimpleClassName(className), fieldName, JavaCG2Constants.FILE_KEY_GENERICS_TYPE);
    }

    /**
     * 查询在字段的泛型类型中使用指定泛型类型的类
     *
     * @param fieldType
     * @return
     */
    public List<String> queryFieldGenericsTypeByType(String fieldType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FGT_QUERY_BY_FIELD_GENERICS_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + DC.FGT_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_FIELD_GENERICS_TYPE.getTableName() +
                    " where " + DC.FGT_SIMPLE_GENERICS_TYPE_NAD + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, dbOperWrapper.querySimpleClassName(fieldType));
    }

    // 查询结果按照主键进行排序，使查询结果与代码中字段定义顺序一致
    private List<WriteDbData4FieldInfo> sortFieldInfoListByPK(List<WriteDbData4FieldInfo> list) {
        list.sort(Comparator.comparingInt(WriteDbData4FieldInfo::getRecordId));
        return list;
    }

    /**
     * 查询类的字段信息，包含超类中的字段，根据类名查询
     *
     * @param className
     * @return
     */
    public List<WriteDbData4FieldInfo> queryFieldInfoByClassNameIncludeSuper(String className) {
        List<WriteDbData4FieldInfo> fieldInfoList = new ArrayList<>();

        String currentClassName = className;
        // 未查询到父类
        do {
            // 查询类的字段信息，根据类名查询
            List<WriteDbData4FieldInfo> tmpList = queryFieldInfoByClassName(currentClassName);
            if (!JavaCG2Util.isCollectionEmpty(tmpList)) {
                fieldInfoList.addAll(tmpList);
            }

            // 需要查询父类中的字段
            currentClassName = jacgExtendsImplHandler.querySuperClassNameByFull(currentClassName);
        } while (currentClassName != null);
        return fieldInfoList;
    }

    /**
     * 查询类的字段信息，根据类名查询
     *
     * @param className
     * @return
     */
    public List<WriteDbData4FieldInfo> queryFieldInfoByClassName(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FI_QUERY_BY_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_INFO.getTableName() +
                    " where " + DC.FI_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<WriteDbData4FieldInfo> list = dbOperator.queryList(sql, WriteDbData4FieldInfo.class, dbOperWrapper.querySimpleClassName(className));
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }
        return sortFieldInfoListByPK(list);
    }

    /**
     * 查询所有包含字段的简单类名
     *
     * @return
     */
    public List<String> queryAllSimpleClassName() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FI_QUERY_ALL_SIMPLE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + DC.FI_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_FIELD_INFO.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class);
    }

    /**
     * 根据类名查询DTO的字段信息
     * 查询非static、非final，且有get或set方法的字段
     *
     * @param className
     * @return
     */
    public List<WriteDbData4FieldInfo> queryFieldInfo4DtoByClassName(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FI_QUERY_4DTO_BY_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_INFO.getTableName() +
                    " where " + DC.FI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FI_STATIC_FLAG + " = ?" +
                    " and " + DC.FI_FINAL_FLAG + " = ?" +
                    " and (" + DC.FI_EXISTS_GET_METHOD + " = ? or " + DC.FI_EXISTS_SET_METHOD + " = ?)";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<WriteDbData4FieldInfo> list = dbOperator.queryList(sql, WriteDbData4FieldInfo.class, dbOperWrapper.querySimpleClassName(className),
                JavaCG2YesNoEnum.NO.getIntValue(), JavaCG2YesNoEnum.NO.getIntValue(), JavaCG2YesNoEnum.YES.getIntValue(), JavaCG2YesNoEnum.YES.getIntValue());
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }
        return sortFieldInfoListByPK(list);
    }

    /**
     * 查询指定类中的全部常用数据类型的字段信息
     * 只查询包含get或set方法的字段，不包含静态或final字段
     * 可以包含超类中的字段，或自定义类型字段中定义的字段，或字段的泛型类型中自定义类型中的字段
     *
     * @param className                          类名
     * @param includeSuperClass                  是否需要包含超类中的字段
     * @param includeCustomField                 是否需要包含自定义类型字段中定义的字段
     * @param includeCollectionFieldGenericsType 是否需要包含字段的泛型类型中自定义类型中的字段
     * @param foundCustomClasNameSet             记录查询过程中找到的自定义类型，可为空
     * @return
     */
    public List<CommonFieldInfoInClass> queryAllCommonFieldInfoInClass(String className, boolean includeSuperClass, boolean includeCustomField,
                                                                       boolean includeCollectionFieldGenericsType, Set<String> foundCustomClasNameSet) {
        List<CommonFieldInfoInClass> commonFieldInfoInClassList = new ArrayList<>();
        List<FieldTypeAndName> fieldTypeAndNameList = new ArrayList<>();
        List<String> recordedCustomFieldTypeList = new ArrayList<>();
        recordedCustomFieldTypeList.add(className);

        String currentClassName = className;
        while (true) {
            // 执行查询指定类中的全部常用数据类型的字段信息
            doQueryAllCommonFieldInfoInClass(currentClassName, currentClassName, commonFieldInfoInClassList, fieldTypeAndNameList, recordedCustomFieldTypeList, includeCustomField,
                    includeCollectionFieldGenericsType, foundCustomClasNameSet);

            if (!includeSuperClass) {
                // 不需要查询父类中的字段
                break;
            }
            // 需要查询父类中的字段
            currentClassName = jacgExtendsImplHandler.querySuperClassNameByFull(currentClassName);
            if (currentClassName == null) {
                // 未查询到父类
                break;
            }
        }
        return commonFieldInfoInClassList;
    }

    /**
     * 执行查询指定类中的全部常用数据类型的字段信息
     *
     * @param classNameOrSuper                   需要查询的类或父类类名
     * @param currentClassName                   当前查询类的类名（可能是需要查询的类或父类类名，或自定义类型字段对应的类，或字段的泛型类型中自定义类型中的字段对应的类）
     * @param commonFieldInfoInClassList         需要查询并返回的常用数据类型的字段信息
     * @param fieldTypeAndNameList               查询到当前类的字段类型与名称路径
     * @param recordedCustomFieldTypeList        当前路径上已经记录过的自定义字段类型列表
     * @param includeCustomField                 是否需要包含自定义类型字段中定义的字段
     * @param includeCollectionFieldGenericsType 是否需要包含字段的泛型类型中自定义类型中的字段
     * @param foundCustomClasNameSet             查询过程中找到的自定义类型，可为空
     */
    private void doQueryAllCommonFieldInfoInClass(String classNameOrSuper, String currentClassName, List<CommonFieldInfoInClass> commonFieldInfoInClassList,
                                                  List<FieldTypeAndName> fieldTypeAndNameList, List<String> recordedCustomFieldTypeList, boolean includeCustomField,
                                                  boolean includeCollectionFieldGenericsType, Set<String> foundCustomClasNameSet) {
        if (foundCustomClasNameSet != null) {
            foundCustomClasNameSet.add(currentClassName);
        }
        // 查询当前类的所有字段信息
        List<WriteDbData4FieldInfo> fieldInfoList = queryFieldInfo4DtoByClassName(currentClassName);
        for (WriteDbData4FieldInfo fieldInfo : fieldInfoList) {
            // 查询字段的@JsonProperty属性
            String jsonAlias = annotationHandler.queryFieldJsonPropertyValue(fieldInfo.getClassName(), fieldInfo.getFieldName());

            CommonFieldInfoInClass commonFieldInfoInClass = CommonFieldInfoInClass.genInstance(fieldInfo);
            commonFieldInfoInClassList.add(commonFieldInfoInClass);
            commonFieldInfoInClass.setClassNameOrSuper(classNameOrSuper);
            commonFieldInfoInClass.setFieldLevel(fieldTypeAndNameList.size());
            commonFieldInfoInClass.setFieldTypeAndNameList(fieldTypeAndNameList);
            commonFieldInfoInClass.setJsonAlias(jsonAlias);

            String fieldType = fieldInfo.getFieldTypeNad();
            /*
                处理自定义类型字段
                需要判断当前处理的类与字段的类型不同时才处理，避免死循环
             */
            if (includeCustomField && JavaCG2Constants.FILE_KEY_CATEGORY_CUSTOM.equals(fieldInfo.getFieldCategory())
                    && !currentClassName.equals(fieldType)) {
                List<String> recordedCustomFieldTypeList4Custom = new ArrayList<>(recordedCustomFieldTypeList);
                if (!recordedCustomFieldTypeList4Custom.contains(fieldType)) {
                    recordedCustomFieldTypeList4Custom.add(fieldType);
                    // 仅当当前处理的自定义字段类型在当前路径上未处理时才不处理，避免死循环
                    List<FieldTypeAndName> fieldTypeAndNameList4Custom = new ArrayList<>(fieldTypeAndNameList);
                    fieldTypeAndNameList4Custom.add(new FieldTypeAndName(fieldType, fieldInfo.getFieldName()));
                    // 继续处理自定义类型字段
                    doQueryAllCommonFieldInfoInClass(classNameOrSuper, fieldType, commonFieldInfoInClassList, fieldTypeAndNameList4Custom, recordedCustomFieldTypeList4Custom,
                            includeCustomField, includeCollectionFieldGenericsType, foundCustomClasNameSet);
                }
            }

            /*
                处理字段中的泛型类型
             */
            if (includeCollectionFieldGenericsType && JavaCG2YesNoEnum.isYes(fieldInfo.getExistsGenericsType())) {
                // 查询当前字段的泛型类型
                List<WriteDbData4FieldGenericsType> fieldGenericsTypeList = queryFieldGenericsTypeByClassFieldName(fieldInfo.getClassName(), fieldInfo.getFieldName());
                // 记录字段泛型类型中已处理过的类型
                Set<String> recordedGenericsTypeSet = new HashSet<>();

                for (WriteDbData4FieldGenericsType fieldGenericsType : fieldGenericsTypeList) {
                    String fieldGenericsTypeStr = fieldGenericsType.getGenericsTypeNad();
                    if (!JavaCG2Constants.FILE_KEY_CATEGORY_CUSTOM.equals(fieldGenericsType.getGenericsCategory())
                            || !recordedGenericsTypeSet.add(fieldGenericsTypeStr)) {
                        /*
                            若当前字段的泛型类型不是自定义类型则跳过
                            若当前字段的泛型类型已处理过则跳过
                         */
                        continue;
                    }
                    List<String> recordedCustomFieldTypeList4Generics = new ArrayList<>(recordedCustomFieldTypeList);
                    if (recordedCustomFieldTypeList4Generics.contains(fieldGenericsTypeStr)) {
                        // 若当前处理的字段的泛型类型在当前路径上有处理则不处理，避免死循环
                        continue;
                    }
                    recordedCustomFieldTypeList4Generics.add(fieldGenericsTypeStr);
                    List<FieldTypeAndName> fieldTypeAndNameList4Generics = new ArrayList<>(fieldTypeAndNameList);
                    fieldTypeAndNameList4Generics.add(new FieldTypeAndName(fieldGenericsTypeStr, fieldInfo.getFieldName()));
                    // 继续处理字段的泛型类型
                    doQueryAllCommonFieldInfoInClass(classNameOrSuper, fieldGenericsTypeStr, commonFieldInfoInClassList, fieldTypeAndNameList4Generics,
                            recordedCustomFieldTypeList4Generics, includeCustomField, includeCollectionFieldGenericsType, foundCustomClasNameSet);
                }
            }
        }
    }

    /**
     * 查询重复类的字段信息，根据类名查询
     *
     * @param jarNum
     * @param className
     * @return
     */
    public List<WriteDbData4FieldInfo> queryDupFieldInfoByClassName(int jarNum, String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.DFI_QUERY_BY_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_DUP_FIELD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_DUP_FIELD_INFO.getTableName() +
                    " where " + DC.FI_JAR_NUM + " = ?" +
                    " and " + DC.FI_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4FieldInfo.class, jarNum, dbOperWrapper.querySimpleClassName(className));
    }

    /**
     * 分页查询使用其他类中字段的使用情况，使用字段所在的唯一类名查询
     *
     * @param fieldInSimpleClassName 字段所在的唯一类名
     * @param startRecordId          起始的record_id
     * @param pageSize               分页查询数量
     * @return
     */
    public List<WriteDbData4FieldUsageOther> queryFieldUsageOtherByPageSCN(String fieldInSimpleClassName, Integer startRecordId, int pageSize) {
        String sql;
        List<Object> argList = new ArrayList<>();
        argList.add(fieldInSimpleClassName);
        SqlKeyEnum sqlKeyEnum;
        if (startRecordId == null) {
            // 首次查询
            sqlKeyEnum = SqlKeyEnum.FUO_QUERY_ALL_FIRST;
            sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_USAGE_OTHER) +
                        " from " + DbTableInfoEnum.DTIE_FIELD_USAGE_OTHER.getTableName() +
                        " where " + DC.FUO_FIELD_IN_SIMPLE_CLASS_NAME + " = ?" +
                        " limit ?";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }
        } else {
            // 非首次查询
            sqlKeyEnum = SqlKeyEnum.FUO_QUERY_ALL_BY_PAGE;
            sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_USAGE_OTHER) +
                        " from " + DbTableInfoEnum.DTIE_FIELD_USAGE_OTHER.getTableName() +
                        " where " + DC.FUO_FIELD_IN_SIMPLE_CLASS_NAME + " = ?" +
                        " and " + DC.FUO_RECORD_ID + " > ?" +
                        " limit ?";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }
            argList.add(startRecordId);
        }
        argList.add(pageSize);
        return dbOperator.queryList(sql, WriteDbData4FieldUsageOther.class, argList.toArray());
    }

    /**
     * 根据类名、字段名、字段类型查询字段信息
     *
     * @param className
     * @param fieldName
     * @param fieldType
     * @return
     */
    public WriteDbData4FieldInfo queryFieldExact(String className, String fieldName, String fieldType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FI_QUERY_EXACT;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_FIELD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_FIELD_INFO.getTableName() +
                    " where " + DC.FI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.FI_FIELD_NAME + " = ?" +
                    " and " + DC.FI_FIELD_TYPE + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4FieldInfo.class, dbOperWrapper.querySimpleClassName(className), fieldName, fieldType);
    }

    /**
     * 根据类名、字段名、字段类型查询字段信息，若在当前类中未查找到，则在父类与接口中查找
     *
     * @param className                 类名
     * @param fieldName                 字段名
     * @param fieldType                 字段类型
     * @param superClassNameAndTypeList 用于记录本次查询到的父类与接口类名，可为null
     * @return
     */
    public WriteDbData4FieldInfo queryFieldExactSuperInterface(String className, String fieldName, String fieldType, List<ClassNameAndType> superClassNameAndTypeList) {
        WriteDbData4FieldInfo fieldInfo = queryFieldExact(className, fieldName, fieldType);
        if (fieldInfo != null) {
            // 指定的字段在当前类中存在
            return fieldInfo;
        }
        // 指定的字段在当前类中不存在，从超类及实现的接口中查找
        // 根据类名向上查询对应的父类、实现的接口信息
        List<ClassNameAndType> tmpSuperClassNameAndTypeList = jacgExtendsImplHandler.queryAllSuperClassesAndInterfaces(className);
        if (JavaCG2Util.isCollectionEmpty(tmpSuperClassNameAndTypeList)) {
            return null;
        }
        if (superClassNameAndTypeList != null) {
            // 当参数指定的父类/接口列表非空时，记录本次查询到的父类/接口
            for (ClassNameAndType superClassNameAndType : tmpSuperClassNameAndTypeList) {
                if (!superClassNameAndTypeList.contains(superClassNameAndType)) {
                    superClassNameAndTypeList.add(superClassNameAndType);
                }
            }
        }
        for (ClassNameAndType superClassNameAndType : tmpSuperClassNameAndTypeList) {
            WriteDbData4FieldInfo superFieldInfo = queryFieldExact(superClassNameAndType.getClassName(), fieldName, fieldType);
            if (superFieldInfo != null) {
                // 在当前类的超类或实现的接口中找到了对应的字段，返回存在
                return superFieldInfo;
            }
        }
        return null;
    }
}
