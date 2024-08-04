package com.adrninistrator.jacg.handler.extendsimpl;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ExtendsImpl;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.common.enums.ClassInterfaceEnum;
import com.adrninistrator.jacg.handler.dto.classes.ClassNameAndAccessFlags;
import com.adrninistrator.jacg.handler.dto.classes.ClassNameAndType;
import com.adrninistrator.jacg.handler.dto.extendsimpl.ExtendsImplInfo;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.dto.accessflag.JavaCGAccessFlags;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author adrninistrator
 * @date 2022/11/19
 * @description: 继承与实际相关的处理类
 */
public class JACGExtendsImplHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(JACGExtendsImplHandler.class);

    // 保存已加载过对应的子类/子接口/实现类的父类/接口唯一类名
    private final Set<String> loadedDownwardSimpleClassNameSet = ConcurrentHashMap.newKeySet();

    /*
        保存的父类/接口对应的所有子类/子接口/实现类
        key
            父类/接口唯一类名
        value
            所有的子类/子接口/实现类名称及access_flags Set
     */
    private final Map<String, Set<ClassNameAndAccessFlags>> allDownwardClassInfoMap = new ConcurrentHashMap<>();

    protected ClassInfoHandler classInfoHandler;

    public JACGExtendsImplHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        if (!useNeo4j()) {
            classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        }
    }

    public JACGExtendsImplHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
    }

    /**
     * 向下加载父类/接口对应的子类/子接口/实现类
     *
     * @param upwardSimpleClassName 父类/接口的唯一类名
     */
    private void loadChildrenOrImplClassInfo(String upwardSimpleClassName) {
        logger.debug("向下加载父类/接口对应的子类/子接口/实现类 {}", upwardSimpleClassName);

        Set<ClassNameAndAccessFlags> allClassNameAndAccessFlagsSet = allDownwardClassInfoMap.computeIfAbsent(upwardSimpleClassName, k -> ConcurrentHashMap.newKeySet());

        // 查询当前类的子类/子接口/实现类
        List<String> downwardSimpleClassNameList = doLoadChildrenOrImplClassInfo(upwardSimpleClassName, allClassNameAndAccessFlagsSet);
        while (!downwardSimpleClassNameList.isEmpty()) {
            List<String> allDownwardSimpleClassNameList = new ArrayList<>();
            // 继续查询子类/子接口/实现类
            for (String downwardSimpleClassName : downwardSimpleClassNameList) {
                List<String> tmpDownwardSimpleClassNameList = doLoadChildrenOrImplClassInfo(downwardSimpleClassName, allClassNameAndAccessFlagsSet);
                allDownwardSimpleClassNameList.addAll(tmpDownwardSimpleClassNameList);
            }

            downwardSimpleClassNameList = allDownwardSimpleClassNameList;
        }

        loadedDownwardSimpleClassNameSet.add(upwardSimpleClassName);
    }

    /**
     * 执行向下加载父类/接口对应的子类/子接口/实现类
     *
     * @param simpleClassName
     * @param allClassNameAndAccessFlagsSet
     * @return 下一层子类/子接口/实现类名称列表
     */
    private List<String> doLoadChildrenOrImplClassInfo(String simpleClassName, Set<ClassNameAndAccessFlags> allClassNameAndAccessFlagsSet) {
        logger.debug("执行向下加载父类/接口对应的子类/子接口/实现类 {}", simpleClassName);
        List<WriteDbData4ExtendsImpl> list = queryDownloadBySimple(simpleClassName);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }

        List<String> downwardSimpleClassNameList = new ArrayList<>(list.size());
        for (WriteDbData4ExtendsImpl writeDbData4ExtendsImpl : list) {
            // 记录所有的子类/子接口/实现类名称及access_flags
            ClassNameAndAccessFlags classNameAndAccessFlags = new ClassNameAndAccessFlags(writeDbData4ExtendsImpl.getSimpleClassName(), writeDbData4ExtendsImpl.getClassName(),
                    writeDbData4ExtendsImpl.getAccessFlags());
            allClassNameAndAccessFlagsSet.add(classNameAndAccessFlags);

            if (JavaCGYesNoEnum.isYes(writeDbData4ExtendsImpl.getExistsDownwardClasses())) {
                // 当前类或接口还存在下一层的类或接口，记录需要返回的下一层子类/子接口/实现类名称
                downwardSimpleClassNameList.add(writeDbData4ExtendsImpl.getSimpleClassName());
            }
        }
        return downwardSimpleClassNameList;
    }

    /**
     * 判断父类/接口与子类/子接口/实现类之间是否存在继承或实现关系，使用完整类名
     *
     * @param upwardClassName   父类/接口类名
     * @param downwardClassName 子类/子接口/实现类类名
     * @return false: 不存在继承或实现关系 true: 存在继承或实现关系
     */
    public boolean checkExtendsOrImplFull(String upwardClassName, String downwardClassName) {
        String upwardSimpleClassName = dbOperWrapper.getSimpleClassName(upwardClassName);
        String downwardSimpleClassName = dbOperWrapper.getSimpleClassName(downwardClassName);
        return checkExtendsOrImplBySimple(upwardSimpleClassName, downwardSimpleClassName);
    }

    /**
     * 判断父类/接口与子类/子接口/实现类之间是否存在继承或实现关系，使用唯一类名
     *
     * @param upwardSimpleClassName   父类/接口唯一类名
     * @param downwardSimpleClassName 子类/子接口/实现类唯一类名
     * @return false: 不存在继承或实现关系 true: 存在继承或实现关系
     */
    public boolean checkExtendsOrImplBySimple(String upwardSimpleClassName, String downwardSimpleClassName) {
        if (!loadedDownwardSimpleClassNameSet.contains(upwardSimpleClassName)) {
            // 向下加载父类/接口对应的子类/子接口/实现类
            loadChildrenOrImplClassInfo(upwardSimpleClassName);
        }

        // 获取父类/接口对应的所有子类/子接口/实现类
        Set<ClassNameAndAccessFlags> allClassNameAndAccessFlagsSet = allDownwardClassInfoMap.get(upwardSimpleClassName);
        if (allClassNameAndAccessFlagsSet.isEmpty()) {
            return false;
        }

        for (ClassNameAndAccessFlags classNameAndAccessFlags : allClassNameAndAccessFlagsSet) {
            if (classNameAndAccessFlags.getSimpleClassName().equals(downwardSimpleClassName)) {
                return true;
            }
        }

        return false;
    }

    /**
     * 查询指定父类/接口的子类接口完整类名，使用父类的完整类名
     * 可根据参数查询指定的结果，例如查询所有的类、接口；仅查询类；仅查询接口；仅查询抽象类；仅查询非抽象类
     *
     * @param superClassName          父类/接口完整类名
     * @param includeInterface        查询结果是否需要包含接口
     * @param includeClass            查询结果是否需要包含类
     * @param includeAbstractClass    查询结果是否需要包含抽象类
     * @param includeNonAbstractClass 查询结果是否需要包含非抽象类
     * @return
     */
    public List<String> queryChildClassListByFull(String superClassName,
                                                  boolean includeInterface,
                                                  boolean includeClass,
                                                  boolean includeAbstractClass,
                                                  boolean includeNonAbstractClass) {
        String superSimpleClassName = dbOperWrapper.getSimpleClassName(superClassName);
        return queryChildClassListBySimple(superSimpleClassName, includeInterface, includeClass, includeAbstractClass, includeNonAbstractClass);
    }

    /**
     * 查询指定父类/接口的子类接口完整类名，使用父类的唯一类名
     * 可根据参数查询指定的结果，例如查询所有的类、接口；仅查询类；仅查询接口；仅查询抽象类；仅查询非抽象类
     *
     * @param superSimpleClassName    父类/接口唯一类名
     * @param includeInterface        查询结果是否需要包含接口
     * @param includeClass            查询结果是否需要包含类
     * @param includeAbstractClass    查询结果是否需要包含抽象类
     * @param includeNonAbstractClass 查询结果是否需要包含非抽象类
     * @return
     */
    public List<String> queryChildClassListBySimple(String superSimpleClassName,
                                                    boolean includeInterface,
                                                    boolean includeClass,
                                                    boolean includeAbstractClass,
                                                    boolean includeNonAbstractClass) {
        if (!includeInterface && !includeClass) {
            throw new JavaCGRuntimeException("类和接口至少需要包含一种");
        }

        if (includeClass && !includeAbstractClass && !includeNonAbstractClass) {
            throw new JavaCGRuntimeException("抽象类与非抽象类至少需要包含一种");
        }

        if (includeAbstractClass && !includeClass) {
            throw new JavaCGRuntimeException("参数指定包含抽象类时，需要指定包含类");
        }

        if (includeNonAbstractClass && !includeClass) {
            throw new JavaCGRuntimeException("参数指定包含非抽象类时，需要指定包含类");
        }

        if (!loadedDownwardSimpleClassNameSet.contains(superSimpleClassName)) {
            // 向下加载父类/接口对应的子类/子接口/实现类
            loadChildrenOrImplClassInfo(superSimpleClassName);
        }

        // 获取父类/接口对应的所有子类/子接口/实现类
        Set<ClassNameAndAccessFlags> allClassNameAndAccessFlagsSet = allDownwardClassInfoMap.get(superSimpleClassName);
        if (allClassNameAndAccessFlagsSet.isEmpty()) {
            return Collections.emptyList();
        }

        List<String> list = new ArrayList<>();
        for (ClassNameAndAccessFlags classNameAndAccessFlags : allClassNameAndAccessFlagsSet) {
            // 判断指定类的是否需要包含在结果中
            if (checkIncludeClass(classNameAndAccessFlags.getAccessFlags(), includeInterface, includeClass, includeAbstractClass, includeNonAbstractClass)) {
                list.add(classNameAndAccessFlags.getClassName());
            }
        }
        Collections.sort(list);
        return list;
    }

    // 根据类的access_flags判断指定类的是否需要包含在结果中
    private boolean checkIncludeClass(int accessFlags, boolean includeInterface, boolean includeClass, boolean includeAbstractClass, boolean includeNonAbstractClass) {
        JavaCGAccessFlags javaCGAccessFlags = new JavaCGAccessFlags(accessFlags);

        // 先判断是否不满足
        if (!includeInterface && javaCGAccessFlags.isInterface()) {
            return false;
        }
        if (!includeClass && !javaCGAccessFlags.isInterface()) {
            return false;
        }
        if (includeClass) {
            if (!includeAbstractClass && javaCGAccessFlags.isAbstract()) {
                return false;
            }
            if (!includeNonAbstractClass && !javaCGAccessFlags.isAbstract()) {
                return false;
            }
        }

        // 再判断是否满足
        if (includeInterface && javaCGAccessFlags.isInterface()) {
            return true;
        }
        // 以下代码虽然会提示不需要，但为了保持逻辑清晰，还是保留
        if (includeClass) {
            if (includeAbstractClass && javaCGAccessFlags.isAbstract()) {
                return true;
            }
            if (includeNonAbstractClass && !javaCGAccessFlags.isAbstract()) {
                return true;
            }
            if (!javaCGAccessFlags.isInterface()) {
                return true;
            }
        }
        return false;
    }

    /**
     * 查询指定类的所有父类
     *
     * @param className
     * @return
     */
    public List<String> queryAllSuperClassName(String className) {
        List<String> superCLassNameList = new ArrayList<>(1);
        String currentClassName = className;
        while (true) {
            String superClassName = querySuperClassNameByFull(currentClassName);
            if (superClassName == null) {
                break;
            }
            superCLassNameList.add(superClassName);
            currentClassName = superClassName;
        }
        return superCLassNameList;
    }

    /**
     * 获取父类名称，使用完整类名
     *
     * @param className
     * @return
     */
    public String querySuperClassNameByFull(String className) {
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        return querySuperClassNameBySimple(simpleClassName);
    }

    /**
     * 获取父类名称，使用唯一类名
     *
     * @param simpleClassName
     * @return
     */
    public String querySuperClassNameBySimple(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.EI_QUERY_UPWARD_EXTENDS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.EI_UPWARD_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_EXTENDS_IMPL.getTableName() +
                    " where " + DC.EI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.EI_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        String upwardClassName = dbOperator.queryObjectOneColumn(sql, String.class, simpleClassName, JavaCGConstants.FILE_KEY_EXTENDS);
        if (upwardClassName == null) {
            logger.debug("未查询到指定类的父类 {}", simpleClassName);
        }
        return upwardClassName;
    }

    /**
     * 根据类名获取实现的接口名称
     *
     * @param className
     * @return
     */
    public List<String> queryImplInterfaceNameByClassName(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.EI_QUERY_UPWARD_IMPL;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.EI_UPWARD_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_EXTENDS_IMPL.getTableName() +
                    " where " + DC.EI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.EI_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        return dbOperator.queryListOneColumn(sql, String.class, simpleClassName, JavaCGConstants.FILE_KEY_IMPLEMENTS);
    }

    /**
     * 根据类名向上查询对应的父类、实现的接口信息
     *
     * @param className
     * @return
     */
    public List<ClassNameAndType> queryUpwardByClassName(String className) {
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.EI_QUERY_UPWARD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.EI_UPWARD_CLASS_NAME, DC.EI_UPWARD_SIMPLE_CLASS_NAME, DC.EI_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_EXTENDS_IMPL.getTableName() +
                    " where " + DC.EI_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<WriteDbData4ExtendsImpl> writeDbData4ExtendsList = dbOperator.queryList(sql, WriteDbData4ExtendsImpl.class, simpleClassName);
        if (JavaCGUtil.isCollectionEmpty(writeDbData4ExtendsList)) {
            return Collections.emptyList();
        }
        List<ClassNameAndType> classNameAndTypesList = new ArrayList<>(writeDbData4ExtendsList.size());
        for (WriteDbData4ExtendsImpl writeDbData4ExtendsImpl : writeDbData4ExtendsList) {
            ClassInterfaceEnum classInterfaceEnum;
            if (JavaCGConstants.FILE_KEY_IMPLEMENTS.equals(writeDbData4ExtendsImpl.getType())) {
                classInterfaceEnum = ClassInterfaceEnum.CIE_INTERFACE;
            } else {
                // 查询父类的access_flags
                classInterfaceEnum = classInfoHandler.queryClassInterfaceEnumBySimple(writeDbData4ExtendsImpl.getUpwardSimpleClassName());
            }
            ClassNameAndType classNameAndType = new ClassNameAndType(writeDbData4ExtendsImpl.getUpwardClassName(), classInterfaceEnum);
            classNameAndTypesList.add(classNameAndType);
        }
        return classNameAndTypesList;
    }

    /**
     * 向下查询继承/实现信息，使用完整类名
     *
     * @param className
     * @return
     */
    private List<WriteDbData4ExtendsImpl> queryDownload(String className) {
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        return queryDownloadBySimple(simpleClassName);
    }

    /**
     * 向下查询继承/实现信息，使用唯一类名
     *
     * @param simpleClassName
     * @return
     */
    protected List<WriteDbData4ExtendsImpl> queryDownloadBySimple(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.EI_QUERY_DOWNWARD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.EI_SIMPLE_CLASS_NAME, DC.EI_CLASS_NAME, DC.EI_ACCESS_FLAGS, DC.EI_EXISTS_DOWNWARD_CLASSES) +
                    " from " + DbTableInfoEnum.DTIE_EXTENDS_IMPL.getTableName() +
                    " where " + DC.EI_UPWARD_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4ExtendsImpl.class, simpleClassName);
    }

    /**
     * 根据类名向下查询对应的子类、实现类信息
     *
     * @param className
     * @return
     */
    public List<ClassNameAndType> queryDownwardByClassName(String className) {
        List<WriteDbData4ExtendsImpl> writeDbData4ExtendsList = queryDownload(className);
        if (JavaCGUtil.isCollectionEmpty(writeDbData4ExtendsList)) {
            return Collections.emptyList();
        }
        List<ClassNameAndType> classNameAndTypesList = new ArrayList<>(writeDbData4ExtendsList.size());
        for (WriteDbData4ExtendsImpl writeDbData4ExtendsImpl : writeDbData4ExtendsList) {
            ClassInterfaceEnum classInterfaceEnum = JACGClassMethodUtil.getClassInterfaceEnum(writeDbData4ExtendsImpl.getAccessFlags());
            ClassNameAndType classNameAndType = new ClassNameAndType(writeDbData4ExtendsImpl.getClassName(), classInterfaceEnum);
            classNameAndTypesList.add(classNameAndType);
        }
        return classNameAndTypesList;
    }

    /**
     * 从指定的类开始向上查询对应的继承与实现信息
     *
     * @param startClassName 指定的类名
     * @return key 子类类名 value 当前类/接口的类型，及父类与实现接口的信息
     */
    public Map<String, ExtendsImplInfo> queryExtendsImplInfoUpward(String startClassName) {
        return queryExtendsImplInfoUpDownward(startClassName, null, true);
    }

    // 从指定的类开始向上或向下查询对应的继承与实现信息
    private Map<String, ExtendsImplInfo> queryExtendsImplInfoUpDownward(String startClassName, List<ClassNameAndType> startClassNameAndTypeList, boolean upward) {
        Map<String, ExtendsImplInfo> resultMap = new HashMap<>();
        Set<ClassNameAndType> classNameAndTypeSet = new HashSet<>();
        if (startClassName != null) {
            classNameAndTypeSet.add(new ClassNameAndType(startClassName, null));
        } else {
            classNameAndTypeSet.addAll(startClassNameAndTypeList);
        }
        while (true) {
            Set<ClassNameAndType> tmpClassNameAndTypeSet = new HashSet<>();
            for (ClassNameAndType classNameAndType : classNameAndTypeSet) {
                Set<ClassNameAndType> returnClassNameAndTypeSet = queryExtendsImplInfoUpDownward(resultMap, classNameAndType, upward);
                tmpClassNameAndTypeSet.addAll(returnClassNameAndTypeSet);
            }
            if (JavaCGUtil.isCollectionEmpty(tmpClassNameAndTypeSet)) {
                break;
            }
            classNameAndTypeSet = tmpClassNameAndTypeSet;
        }
        return resultMap;
    }

    /**
     * 执行从指定的类开始向上查询对应的继承与实现信息
     *
     * @param resultMap
     * @param classNameAndType
     * @return 下一次需要查询的类名列表，若为空则不需要再查询
     */
    private Set<ClassNameAndType> queryExtendsImplInfoUpDownward(Map<String, ExtendsImplInfo> resultMap, ClassNameAndType classNameAndType, boolean upward) {
        Set<ClassNameAndType> newClassNameAndTypeSet = new HashSet<>();
        String className = classNameAndType.getClassName();
        if (resultMap.containsKey(className)) {
            // 已经处理过的类不再处理
            return newClassNameAndTypeSet;
        }
        // 根据类名查询对应的父类、实现的接口信息
        List<ClassNameAndType> upDownwardClassInfoList;
        if (upward) {
            upDownwardClassInfoList = queryUpwardByClassName(className);
        } else {
            upDownwardClassInfoList = queryDownwardByClassName(className);
        }
        if (JavaCGUtil.isCollectionEmpty(upDownwardClassInfoList)) {
            return newClassNameAndTypeSet;
        }
        ClassInterfaceEnum classInterfaceEnum = classInfoHandler.queryClassInterfaceEnum(className);
        ExtendsImplInfo extendsImplInfo = new ExtendsImplInfo();
        extendsImplInfo.setClassType(classInterfaceEnum);
        extendsImplInfo.setExtendsImplClassInfoList(upDownwardClassInfoList);
        resultMap.put(className, extendsImplInfo);

        newClassNameAndTypeSet.addAll(upDownwardClassInfoList);
        return newClassNameAndTypeSet;
    }

    /**
     * 从指定的类开始向下查询对应的继承与实现信息
     *
     * @param startClassName 指定的类名
     * @return key 父类/接口名称 value 当前类/接口的类型，及子类与实现类信息
     */
    public Map<String, ExtendsImplInfo> queryExtendsImplInfoDownward(String startClassName) {
        return queryExtendsImplInfoUpDownward(startClassName, null, false);
    }

    /**
     * 查询某个类的顶层父类或接口
     *
     * @param className
     * @return
     */
    public List<ClassNameAndType> queryTopCLassList(String className) {
        List<ClassNameAndType> classNameAndTypeList = new ArrayList<>();
        Map<String, ExtendsImplInfo> resultMap = new HashMap<>();
        Set<ClassNameAndType> classNameAndTypeSet = new HashSet<>();
        classNameAndTypeSet.add(new ClassNameAndType(className, null));
        while (true) {
            Set<ClassNameAndType> tmpClassNameAndTypeSet = new HashSet<>();
            for (ClassNameAndType classNameAndType : classNameAndTypeSet) {
                Set<ClassNameAndType> returnClassNameAndTypeSet = queryExtendsImplInfoUpDownward(resultMap, classNameAndType, true);
                tmpClassNameAndTypeSet.addAll(returnClassNameAndTypeSet);
                if (JavaCGUtil.isCollectionEmpty(returnClassNameAndTypeSet)) {
                    // 当前类向上未查询到父类或实现接口
                    classNameAndTypeList.add(classNameAndType);
                }
            }
            if (JavaCGUtil.isCollectionEmpty(tmpClassNameAndTypeSet)) {
                break;
            }
            classNameAndTypeSet = tmpClassNameAndTypeSet;
        }
        return classNameAndTypeList;
    }

    /**
     * 从指定的类的所有顶层父类/接口开始向下查询对应的继承与实现信息
     *
     * @param startClassName 指定的类名
     * @return key 父类/接口名称 value 当前类/接口的类型，及子类与实现类信息
     */
    public Map<String, ExtendsImplInfo> queryExtendsImplInfoDownwardFromTop(String startClassName) {
        List<ClassNameAndType> topClassNameAndTypeList = queryTopCLassList(startClassName);
        if (JavaCGUtil.isCollectionEmpty(topClassNameAndTypeList)) {
            return Collections.emptyMap();
        }
        return queryExtendsImplInfoUpDownward(null, topClassNameAndTypeList, false);
    }
}
