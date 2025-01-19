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
import com.adrninistrator.jacg.handler.dto.extendsimpl.ExtendsImplNode;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.dto.accessflag.JavaCG2AccessFlags;
import com.adrninistrator.javacg2.dto.stack.ListAsStack;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/19
 * @description: 继承与实际相关的处理类
 */
public class JACGExtendsImplHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(JACGExtendsImplHandler.class);

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
     * 获取 fromClassName 逐层向上继承或实现到 toClassName 中间经过的父类或接口类名，使用完整类名
     *
     * @param fromClassName
     * @param toClassName
     * @return
     */
    public List<String> getPathToSuperImplClassByFull(String fromClassName, String toClassName) {
        return getPathToSuperImplClassBySimple(dbOperWrapper.querySimpleClassName(fromClassName), dbOperWrapper.querySimpleClassName(toClassName));
    }

    /**
     * 获取 fromSimpleClassName 逐层向上继承或实现到 toSimpleClassName 中间经过的父类或接口类名，使用唯一类名
     * 若 fromSimpleClassName 没有直接或间接继承或实现 toSimpleClassName ，则返回列表为空
     * 若 fromSimpleClassName 有直接或间接继承或实现 toSimpleClassName ，则返回列表中序号最大的元素为toClassName，列表中不会包括fromClassName
     *
     * @param fromSimpleClassName
     * @param toSimpleClassName
     * @return
     */
    public List<String> getPathToSuperImplClassBySimple(String fromSimpleClassName, String toSimpleClassName) {
        String upperClassName = querySuperClassNameBySimple(fromSimpleClassName);
        if (upperClassName != null) {
            String upperSimpleClassName = dbOperWrapper.querySimpleClassName(upperClassName);
            if (upperSimpleClassName.equals(toSimpleClassName)) {
                // toSimpleClassName 是 fromSimpleClassName 的父类或实现的接口
                return Collections.singletonList(upperClassName);
            }
        }
        // toSimpleClassName 不是 fromSimpleClassName 的父类或实现的接口，逐层向上找
        // 查询 toSimpleClassName 父类及实现的接口类名列表
        List<String> initUpwardClassNameList = queryUpwardClassNameBySimple(fromSimpleClassName);
        if (JavaCG2Util.isCollectionEmpty(initUpwardClassNameList)) {
            // 未查询到 toSimpleClassName 父类及实现的接口类名列表
            return Collections.emptyList();
        }

        ListAsStack<ExtendsImplNode> stack = new ListAsStack<>();
        // 栈初始加入 toSimpleClassName 父类及实现的接口类名
        ExtendsImplNode initExtendsImplNode = new ExtendsImplNode();
        initExtendsImplNode.setExtendsImplClassNameList(initUpwardClassNameList);
        initExtendsImplNode.setListIndex(0);
        stack.push(initExtendsImplNode);

        // 通过栈进行循环处理
        while (!stack.isEmpty()) {
            ExtendsImplNode currentExtendsImplNode = stack.peek();
            // 获取当前栈顶元素的当前父类或实现的接口类名
            String currentUpwardClassName = currentExtendsImplNode.getCurrentClassName();
            if (currentUpwardClassName == null) {
                // 当前父类或实现的接口类名列表处理完毕，出栈
                stack.pop();
                if (!stack.isEmpty()) {
                    // 出栈后若栈非空，则将栈顶元素的列表下标加1
                    stack.peek().addListIndex();
                }
                continue;
            }

            // 当前父类或实现的接口类名列表未处理完毕
            String currentUpwardSimpleClassName = dbOperWrapper.querySimpleClassName(currentUpwardClassName);
            if (toSimpleClassName.equals(currentUpwardSimpleClassName)) {
                // fromSimpleClassName 有直接或间接继承或实现 toSimpleClassName
                List<String> classNameList = new ArrayList<>(stack.getHead() + 1);
                for (int i = 0; i <= stack.getHead(); i++) {
                    ExtendsImplNode extendsImplNode = stack.getElementAt(i);
                    classNameList.add(extendsImplNode.getCurrentClassName());
                }
                return classNameList;
            }

            // 查询当前类的父类及实现的接口类名列表
            List<String> currentUpwardClassNameList = queryUpwardClassNameBySimple(currentUpwardSimpleClassName);
            if (JavaCG2Util.isCollectionEmpty(currentUpwardClassNameList)) {
                // 未查询到当前类的父类及实现的接口类名列表，将列表的序号加1
                currentExtendsImplNode.addListIndex();
                continue;
            }

            // 向栈中加入当前类的父类及实现的接口类名
            ExtendsImplNode nextExtendsImplNode = new ExtendsImplNode();
            nextExtendsImplNode.setExtendsImplClassNameList(currentUpwardClassNameList);
            nextExtendsImplNode.setListIndex(0);
            stack.push(nextExtendsImplNode);
        }
        return Collections.emptyList();
    }

    /**
     * 向下获取父类/接口对应的子类/子接口/实现类
     *
     * @param upwardSimpleClassName 父类/接口的唯一类名
     * @return
     */
    private Set<ClassNameAndAccessFlags> getChildrenOrImplClassInfo(String upwardSimpleClassName) {
        Set<ClassNameAndAccessFlags> allClassNameAndAccessFlagsSet = new HashSet<>();

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

        return allClassNameAndAccessFlagsSet;
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
        if (JavaCG2Util.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }

        List<String> downwardSimpleClassNameList = new ArrayList<>(list.size());
        for (WriteDbData4ExtendsImpl writeDbData4ExtendsImpl : list) {
            // 记录所有的子类/子接口/实现类名称及access_flags
            ClassNameAndAccessFlags classNameAndAccessFlags = new ClassNameAndAccessFlags(writeDbData4ExtendsImpl.getSimpleClassName(), writeDbData4ExtendsImpl.getClassName(),
                    writeDbData4ExtendsImpl.getAccessFlags());
            allClassNameAndAccessFlagsSet.add(classNameAndAccessFlags);

            if (JavaCG2YesNoEnum.isYes(writeDbData4ExtendsImpl.getExistsDownwardClasses())) {
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
        String upwardSimpleClassName = dbOperWrapper.querySimpleClassName(upwardClassName);
        String downwardSimpleClassName = dbOperWrapper.querySimpleClassName(downwardClassName);
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
        List<String> classNameList = getPathToSuperImplClassBySimple(downwardSimpleClassName, upwardSimpleClassName);
        return !JavaCG2Util.isCollectionEmpty(classNameList);
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
        String superSimpleClassName = dbOperWrapper.querySimpleClassName(superClassName);
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
            throw new JavaCG2RuntimeException("类和接口至少需要包含一种");
        }

        if (includeClass && !includeAbstractClass && !includeNonAbstractClass) {
            throw new JavaCG2RuntimeException("抽象类与非抽象类至少需要包含一种");
        }

        if (includeAbstractClass && !includeClass) {
            throw new JavaCG2RuntimeException("参数指定包含抽象类时，需要指定包含类");
        }

        if (includeNonAbstractClass && !includeClass) {
            throw new JavaCG2RuntimeException("参数指定包含非抽象类时，需要指定包含类");
        }

        // 向下加载父类/接口对应的子类/子接口/实现类
        Set<ClassNameAndAccessFlags> allClassNameAndAccessFlagsSet = getChildrenOrImplClassInfo(superSimpleClassName);

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
        JavaCG2AccessFlags javaCG2AccessFlags = new JavaCG2AccessFlags(accessFlags);

        // 先判断是否不满足
        if (!includeInterface && javaCG2AccessFlags.isInterface()) {
            return false;
        }
        if (!includeClass && !javaCG2AccessFlags.isInterface()) {
            return false;
        }
        if (includeClass) {
            if (!includeAbstractClass && javaCG2AccessFlags.isAbstract()) {
                return false;
            }
            if (!includeNonAbstractClass && !javaCG2AccessFlags.isAbstract()) {
                return false;
            }
        }

        // 再判断是否满足
        if (includeInterface && javaCG2AccessFlags.isInterface()) {
            return true;
        }
        // 以下代码虽然会提示不需要，但为了保持逻辑清晰，还是保留
        if (includeClass) {
            if (includeAbstractClass && javaCG2AccessFlags.isAbstract()) {
                return true;
            }
            if (includeNonAbstractClass && !javaCG2AccessFlags.isAbstract()) {
                return true;
            }
            if (!javaCG2AccessFlags.isInterface()) {
                return true;
            }
        }
        return false;
    }

    /**
     * 查询指定类的所有父类类名
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
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
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

        String upwardClassName = dbOperator.queryObjectOneColumn(sql, String.class, simpleClassName, JavaCG2Constants.FILE_KEY_EXTENDS);
        if (upwardClassName == null) {
            logger.debug("未查询到指定类的父类 {}", simpleClassName);
        }
        return upwardClassName;
    }

    /**
     * 查询指定类的父类及实现的接口类名列表，使用唯一类名查询
     *
     * @param simpleClassName
     * @return
     */
    public List<String> queryUpwardClassNameBySimple(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.EI_QUERY_UPWARD_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.EI_UPWARD_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_EXTENDS_IMPL.getTableName() +
                    " where " + DC.EI_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryListOneColumn(sql, String.class, simpleClassName);
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
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        return dbOperator.queryListOneColumn(sql, String.class, simpleClassName, JavaCG2Constants.FILE_KEY_IMPLEMENTS);
    }

    /**
     * 根据类名向上查询父类、实现的接口信息
     *
     * @param className
     * @return
     */
    public List<WriteDbData4ExtendsImpl> queryUpwardInfoByClassName(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.EI_QUERY_UPWARD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.EI_UPWARD_CLASS_NAME, DC.EI_UPWARD_SIMPLE_CLASS_NAME, DC.EI_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_EXTENDS_IMPL.getTableName() +
                    " where " + DC.EI_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4ExtendsImpl.class, dbOperWrapper.querySimpleClassName(className));
    }

    /**
     * 根据类名向上查询父类、实现的接口的类名及类型
     *
     * @param className
     * @return
     */
    public List<ClassNameAndType> queryUpwardClassNameAndTypeByClassName(String className) {
        // 根据类名向上查询父类、实现的接口信息
        List<WriteDbData4ExtendsImpl> writeDbData4ExtendsList = queryUpwardInfoByClassName(className);
        if (JavaCG2Util.isCollectionEmpty(writeDbData4ExtendsList)) {
            return Collections.emptyList();
        }
        List<ClassNameAndType> classNameAndTypesList = new ArrayList<>(writeDbData4ExtendsList.size());
        for (WriteDbData4ExtendsImpl writeDbData4ExtendsImpl : writeDbData4ExtendsList) {
            ClassInterfaceEnum classInterfaceEnum;
            if (JavaCG2Constants.FILE_KEY_IMPLEMENTS.equals(writeDbData4ExtendsImpl.getType())) {
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
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
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
        if (JavaCG2Util.isCollectionEmpty(writeDbData4ExtendsList)) {
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
     * 从指定的类开始向上查询所有对应的继承与实现信息
     *
     * @param startClassName 指定的类名
     * @return key 子类类名 value 当前类/接口的类型，及父类与实现接口的信息
     */
    public LinkedHashMap<String, ExtendsImplInfo> queryAllExtendsImplInfoUpward(String startClassName) {
        return queryAllExtendsImplInfoUpDownward(startClassName, null, true);
    }

    /**
     * 从指定的类开始向上查询所有对应的超类与实现的接口
     *
     * @param startClassName 指定的类名
     * @return key 子类类名 value 当前类/接口的类型，及父类与实现接口的信息
     */
    public List<ClassNameAndType> queryAllSuperClassesAndInterfaces(String startClassName) {
        List<ClassNameAndType> superClassAndInterfaceList = new ArrayList<>();
        LinkedHashMap<String, ExtendsImplInfo> resultMap = queryAllExtendsImplInfoUpDownward(startClassName, null, true);
        for (Map.Entry<String, ExtendsImplInfo> entry : resultMap.entrySet()) {
            ExtendsImplInfo extendsImplInfo = entry.getValue();
            for (ClassNameAndType superClassOrInterface : extendsImplInfo.getExtendsImplClassInfoList()) {
                if (!superClassAndInterfaceList.contains(superClassOrInterface)) {
                    superClassAndInterfaceList.add(superClassOrInterface);
                }
            }
        }
        return superClassAndInterfaceList;
    }

    // 从指定的类开始向上或向下查询所有对应的继承与实现信息
    private LinkedHashMap<String, ExtendsImplInfo> queryAllExtendsImplInfoUpDownward(String startClassName, List<ClassNameAndType> startClassNameAndTypeList, boolean upward) {
        LinkedHashMap<String, ExtendsImplInfo> resultMap = new LinkedHashMap<>();
        Set<ClassNameAndType> classNameAndTypeSet = new HashSet<>();
        if (startClassName != null) {
            classNameAndTypeSet.add(new ClassNameAndType(startClassName, null));
        } else {
            classNameAndTypeSet.addAll(startClassNameAndTypeList);
        }
        while (true) {
            Set<ClassNameAndType> tmpClassNameAndTypeSet = new HashSet<>();
            for (ClassNameAndType classNameAndType : classNameAndTypeSet) {
                Set<ClassNameAndType> returnClassNameAndTypeSet = doQueryAllExtendsImplInfoUpDownward(resultMap, classNameAndType, upward);
                tmpClassNameAndTypeSet.addAll(returnClassNameAndTypeSet);
            }
            if (JavaCG2Util.isCollectionEmpty(tmpClassNameAndTypeSet)) {
                break;
            }
            classNameAndTypeSet = tmpClassNameAndTypeSet;
        }
        return resultMap;
    }

    /**
     * 从指定的类开始向上或向下查询对应的继承与实现信息
     *
     * @param resultMap
     * @param classNameAndType
     * @param upward
     * @return 下一次需要查询的类名列表，若为空则不需要再查询
     */
    private Set<ClassNameAndType> doQueryAllExtendsImplInfoUpDownward(LinkedHashMap<String, ExtendsImplInfo> resultMap, ClassNameAndType classNameAndType, boolean upward) {
        Set<ClassNameAndType> newClassNameAndTypeSet = new HashSet<>();
        String className = classNameAndType.getClassName();
        if (resultMap.containsKey(className)) {
            // 已经处理过的类不再处理
            return newClassNameAndTypeSet;
        }
        // 根据类名查询对应的父类、实现的接口信息
        List<ClassNameAndType> upDownwardClassInfoList;
        if (upward) {
            upDownwardClassInfoList = queryUpwardClassNameAndTypeByClassName(className);
        } else {
            upDownwardClassInfoList = queryDownwardByClassName(className);
        }
        if (JavaCG2Util.isCollectionEmpty(upDownwardClassInfoList)) {
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
     * 从指定的类开始向下查询所有对应的继承与实现信息
     *
     * @param startClassName 指定的类名
     * @return key 父类/接口名称 value 当前类/接口的类型，及子类与实现类信息
     */
    public LinkedHashMap<String, ExtendsImplInfo> queryAllExtendsImplInfoDownward(String startClassName) {
        return queryAllExtendsImplInfoUpDownward(startClassName, null, false);
    }

    /**
     * 查询某个类的顶层父类或接口
     *
     * @param className
     * @return
     */
    public List<ClassNameAndType> queryTopCLassList(String className) {
        List<ClassNameAndType> classNameAndTypeList = new ArrayList<>();
        LinkedHashMap<String, ExtendsImplInfo> resultMap = new LinkedHashMap<>();
        Set<ClassNameAndType> classNameAndTypeSet = new HashSet<>();
        classNameAndTypeSet.add(new ClassNameAndType(className, null));
        while (true) {
            Set<ClassNameAndType> tmpClassNameAndTypeSet = new HashSet<>();
            for (ClassNameAndType classNameAndType : classNameAndTypeSet) {
                Set<ClassNameAndType> returnClassNameAndTypeSet = doQueryAllExtendsImplInfoUpDownward(resultMap, classNameAndType, true);
                tmpClassNameAndTypeSet.addAll(returnClassNameAndTypeSet);
                if (JavaCG2Util.isCollectionEmpty(returnClassNameAndTypeSet)) {
                    // 当前类向上未查询到父类或实现接口
                    classNameAndTypeList.add(classNameAndType);
                }
            }
            if (JavaCG2Util.isCollectionEmpty(tmpClassNameAndTypeSet)) {
                break;
            }
            classNameAndTypeSet = tmpClassNameAndTypeSet;
        }
        return classNameAndTypeList;
    }

    /**
     * 从指定的类的所有顶层父类/接口开始向下查询所有对应的继承与实现信息
     *
     * @param startClassName 指定的类名
     * @return key 父类/接口名称 value 当前类/接口的类型，及子类与实现类信息
     */
    public LinkedHashMap<String, ExtendsImplInfo> queryAllExtendsImplInfoDownwardFromTop(String startClassName) {
        List<ClassNameAndType> topClassNameAndTypeList = queryTopCLassList(startClassName);
        if (JavaCG2Util.isCollectionEmpty(topClassNameAndTypeList)) {
            return new LinkedHashMap<>();
        }
        return queryAllExtendsImplInfoUpDownward(null, topClassNameAndTypeList, false);
    }
}
