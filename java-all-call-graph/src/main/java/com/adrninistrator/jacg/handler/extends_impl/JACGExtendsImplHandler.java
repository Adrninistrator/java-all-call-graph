package com.adrninistrator.jacg.handler.extends_impl;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.access_flag.JACGAccessFlags;
import com.adrninistrator.jacg.dto.classes.ClassNameAndAccessFlags;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
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

    public JACGExtendsImplHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public JACGExtendsImplHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 向下加载父类/接口对应的子类/子接口/实现类
     *
     * @param upwardSimpleClassName 父类/接口的唯一类名
     * @return
     */
    private boolean loadChildrenOrImplClassInfo(String upwardSimpleClassName) {
        if (loadedDownwardSimpleClassNameSet.contains(upwardSimpleClassName)) {
            return true;
        }

        logger.debug("向下加载父类/接口对应的子类/子接口/实现类 {}", upwardSimpleClassName);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.EI_QUERY_DOWNWARD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.EI_SIMPLE_CLASS_NAME, DC.EI_CLASS_NAME, DC.EI_ACCESS_FLAGS, DC.EI_EXISTS_DOWNWARD_CLASSES) +
                    " from " + DbTableInfoEnum.DTIE_EXTENDS_IMPL.getTableName() +
                    " where " + DC.EI_UPWARD_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Set<ClassNameAndAccessFlags> allClassNameAndAccessFlagsSet = allDownwardClassInfoMap.computeIfAbsent(upwardSimpleClassName, k -> ConcurrentHashMap.newKeySet());

        boolean success = true;
        // 查询当前类的子类/子接口/实现类
        List<String> downwardSimpleClassNameList = doLoadChildrenOrImplClassInfo(upwardSimpleClassName, sql, allClassNameAndAccessFlagsSet);
        if (downwardSimpleClassNameList == null) {
            return false;
        }

        while (true) {
            if (downwardSimpleClassNameList.isEmpty()) {
                break;
            }

            List<String> allDownwardSimpleClassNameList = new ArrayList<>();
            // 继续查询子类/子接口/实现类
            for (String downwardSimpleClassName : downwardSimpleClassNameList) {
                List<String> tmpDownwardSimpleClassNameList = doLoadChildrenOrImplClassInfo(downwardSimpleClassName, sql, allClassNameAndAccessFlagsSet);
                if (tmpDownwardSimpleClassNameList == null) {
                    success = false;
                    break;
                }
                allDownwardSimpleClassNameList.addAll(tmpDownwardSimpleClassNameList);
            }

            if (!success) {
                break;
            }
            downwardSimpleClassNameList = allDownwardSimpleClassNameList;
        }

        if (!success) {
            return false;
        }
        loadedDownwardSimpleClassNameSet.add(upwardSimpleClassName);
        return true;
    }

    /**
     * 执行向下加载父类/接口对应的子类/子接口/实现类
     *
     * @param simpleClassName
     * @param sql
     * @param allClassNameAndAccessFlagsSet
     * @return 下一层子类/子接口/实现类名称列表
     */
    private List<String> doLoadChildrenOrImplClassInfo(String simpleClassName, String sql, Set<ClassNameAndAccessFlags> allClassNameAndAccessFlagsSet) {
        logger.debug("执行向下加载父类/接口对应的子类/子接口/实现类 {}", simpleClassName);
        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{simpleClassName});
        if (list == null) {
            return null;
        }

        if (list.isEmpty()) {
            return Collections.emptyList();
        }

        List<String> downwardSimpleClassNameList = new ArrayList<>(list.size());
        for (Map<String, Object> map : list) {
            String downwardSimpleClassName = (String) map.get(DC.EI_SIMPLE_CLASS_NAME);
            String downwardClassName = (String) map.get(DC.EI_CLASS_NAME);
            int accessFlags = (int) map.get(DC.EI_ACCESS_FLAGS);
            int existsDownwardClasses = (int) map.get(DC.EI_EXISTS_DOWNWARD_CLASSES);

            ClassNameAndAccessFlags classNameAndAccessFlags = new ClassNameAndAccessFlags(downwardSimpleClassName, downwardClassName, accessFlags);
            // 记录所有的子类/子接口/实现类名称及access_flags
            allClassNameAndAccessFlagsSet.add(classNameAndAccessFlags);

            if (JavaCGYesNoEnum.isYes(existsDownwardClasses)) {
                // 当前类或接口还存在下一层的类或接口，记录需要返回的下一层子类/子接口/实现类名称
                downwardSimpleClassNameList.add(downwardSimpleClassName);
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
        // 向下加载父类/接口对应的子类/子接口/实现类
        if (!loadChildrenOrImplClassInfo(upwardSimpleClassName)) {
            throw new JavaCGRuntimeException("加载对应的类失败");
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

        // 向下加载父类/接口对应的子类/子接口/实现类
        if (!loadChildrenOrImplClassInfo(superSimpleClassName)) {
            throw new JavaCGRuntimeException("加载对应的类失败");
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
        JACGAccessFlags jacgAccessFlags = new JACGAccessFlags(accessFlags);

        // 先判断是否不满足
        if (!includeInterface && jacgAccessFlags.isInterface()) {
            return false;
        }
        if (!includeClass && !jacgAccessFlags.isInterface()) {
            return false;
        }
        if (includeClass) {
            if (!includeAbstractClass && jacgAccessFlags.isAbstract()) {
                return false;
            }
            if (!includeNonAbstractClass && !jacgAccessFlags.isAbstract()) {
                return false;
            }
        }

        // 再判断是否满足
        if (includeInterface && jacgAccessFlags.isInterface()) {
            return true;
        }
        // 以下代码虽然会提示不需要，但为了保持逻辑清晰，还是保留
        if (includeClass) {
            if (includeAbstractClass && jacgAccessFlags.isAbstract()) {
                return true;
            }
            if (includeNonAbstractClass && !jacgAccessFlags.isAbstract()) {
                return true;
            }
            if (!jacgAccessFlags.isInterface()) {
                return true;
            }
        }
        return false;
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
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.EI_QUERY_UPWARD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.EI_UPWARD_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_EXTENDS_IMPL.getTableName() +
                    " where " + DC.EI_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{simpleClassName});
        if (JavaCGUtil.isCollectionEmpty(list)) {
            logger.debug("未查询到指定类的父类 {}", simpleClassName);
            return null;
        }

        return (String) list.get(0);
    }
}
