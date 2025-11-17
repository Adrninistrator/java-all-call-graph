package com.adrninistrator.jacg.compatibility.handler;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.ListStringAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.OuterClassWithAnnotation;
import com.adrninistrator.jacg.dto.compatibility.CompatibilityHandlerDto;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.handler.field.FieldInfoHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/8/5
 * @description: 检查被调用类、方法、字段的处理类基类
 */
public abstract class BaseCompatibilityCheckHandler extends BaseHandler {

    protected final ClassInfoHandler classInfoHandler;
    protected final MethodInfoHandler methodInfoHandler;
    protected final MethodCallHandler methodCallHandler;
    protected final JACGExtendsImplHandler extendsImplHandler;
    protected final FieldInfoHandler fieldInfoHandler;
    protected final AnnotationHandler annotationHandler;

    protected List<DbOperWrapper> otherDbOperWrapperList;
    protected List<CompatibilityHandlerDto> compatibilityHandlerDtoList;
    protected Map<String, Boolean> classNameExistsMap;

    protected String currentOutputDirPath;

    protected Map<Integer, WriteDbData4JarInfo> jarInfoMap;

    public BaseCompatibilityCheckHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);

        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        extendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
        fieldInfoHandler = new FieldInfoHandler(dbOperWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
    }

    // 初始化
    public void init() {
        compatibilityHandlerDtoList = new ArrayList<>(otherDbOperWrapperList.size() + 1);
        compatibilityHandlerDtoList.add(new CompatibilityHandlerDto(classInfoHandler, methodInfoHandler, extendsImplHandler, fieldInfoHandler));
        if (!otherDbOperWrapperList.isEmpty()) {
            for (DbOperWrapper otherDbOperWrapper : otherDbOperWrapperList) {
                ClassInfoHandler otherClassInfoHandler = new ClassInfoHandler(otherDbOperWrapper);
                MethodInfoHandler otherMethodInfoHandler = new MethodInfoHandler(otherDbOperWrapper);
                JACGExtendsImplHandler otherExtendsImplHandler = new JACGExtendsImplHandler(otherDbOperWrapper);
                FieldInfoHandler otherFieldInfoHandler = new FieldInfoHandler(otherDbOperWrapper);
                compatibilityHandlerDtoList.add(new CompatibilityHandlerDto(otherClassInfoHandler, otherMethodInfoHandler, otherExtendsImplHandler, otherFieldInfoHandler));
            }
        }
    }

    // 检查指定的类是否存在
    protected boolean checkClassExists(String className) {
        Boolean exists = classNameExistsMap.get(className);
        if (exists != null) {
            return exists;
        }

        for (CompatibilityHandlerDto compatibilityHandlerDto : compatibilityHandlerDtoList) {
            ClassInfoHandler tmpClassInfoHandler = compatibilityHandlerDto.getClassInfoHandler();
            // 需要通过完整类名查询，因为不同的数据库中唯一类名可能不同
            WriteDbData4ClassInfo classInfo = tmpClassInfoHandler.queryClassInfoByClassName(className);
            if (classInfo != null) {
                classNameExistsMap.put(className, Boolean.TRUE);
                return true;
            }
        }
        classNameExistsMap.put(className, Boolean.FALSE);
        return false;
    }

    /**
     * 检查类是否间接或直接继承或实现了另一个类
     *
     * @param upwardClassName   父类或接口
     * @param downwardClassName 子类或实现类
     * @return
     */
    protected boolean checkExtendsImplClass(String upwardClassName, String downwardClassName) {
        if (JavaCG2CommonNameConstants.CLASS_NAME_OBJECT.equals(upwardClassName)) {
            // 判断是否为Object的子类时，默认为是
            return true;
        }

        // 记录子类或实现类，及间接或直接继承或实现的父类或接口类名
        List<String> allExtendsImplClassNameList = new ArrayList<>();
        // 首先添加子类或实现类类名开始处理
        allExtendsImplClassNameList.add(downwardClassName);

        // 从当前数据库开始，到其他数据库，查询指定类的所有父类与实现接口
        for (CompatibilityHandlerDto compatibilityHandlerDto : compatibilityHandlerDtoList) {
            // 记录需要处理的类的队列，假如同时对allExtendsImplClassNameList进行增加和遍历会有并发问题
            ArrayDeque<String> deque = new ArrayDeque<>(allExtendsImplClassNameList);

            JACGExtendsImplHandler tmpExtendsImplHandler = compatibilityHandlerDto.getExtendsImplHandler();
            while (!deque.isEmpty()) {
                String className = deque.removeFirst();
                List<String> allSuperClassesAndInterfacesNameList = tmpExtendsImplHandler.queryAllSuperClassesAndInterfacesName(className);
                if (JavaCG2Util.isCollectionEmpty(allSuperClassesAndInterfacesNameList)) {
                    continue;
                }
                for (String superClassesAndInterfacesName : allSuperClassesAndInterfacesNameList) {
                    if (superClassesAndInterfacesName.equals(upwardClassName)) {
                        // 子类或实现类有继承或实现父类或接口
                        return true;
                    }
                    if (!allExtendsImplClassNameList.contains(superClassesAndInterfacesName)) {
                        // 记录继承或实现的父类或接口类名，继续处理
                        allExtendsImplClassNameList.add(superClassesAndInterfacesName);
                        deque.add(superClassesAndInterfacesName);
                    }
                }
            }
        }
        return false;
    }

    /**
     * 查询指定的类上@ConditionalOnClass注解的属性值用于显示的字符串
     *
     * @param className
     * @return
     */
    protected String queryConditionalOnClassValueStr(String className) {
        List<String> valueList = queryConditionalOnClassValueList(className);
        if (JavaCG2Util.isCollectionEmpty(valueList)) {
            return "";
        }
        return StringUtils.join(valueList, JavaCG2Constants.FLAG_COMMA);
    }

    /**
     * 查询指定的类上@ConditionalOnClass注解的属性值列表
     *
     * @param className
     * @return
     */
    protected List<String> queryConditionalOnClassValueList(String className) {
        List<String> valueList = new ArrayList<>();
        // 查询指定类上的注解
        Map<String, BaseAnnotationAttribute> currentAnnotationAttributeMap = annotationHandler.queryAnnotationAttributes4Class(className,
                JACGCommonNameConstants.SPRING_BOOT_CONDITIONAL_ON_CLASS);
        handleConditionalOnClassValue(valueList, currentAnnotationAttributeMap);

        // 查询指定类的外部类上的注解
        List<OuterClassWithAnnotation> annotationAttributeList = annotationHandler.queryOuterClassesInfo(className, JACGCommonNameConstants.SPRING_BOOT_CONDITIONAL_ON_CLASS);
        for (OuterClassWithAnnotation outerClassWithAnnotation : annotationAttributeList) {
            handleConditionalOnClassValue(valueList, outerClassWithAnnotation.getAnnotationAttributeMap());
        }
        return valueList;
    }

    private void handleConditionalOnClassValue(List<String> valueList, Map<String, BaseAnnotationAttribute> annotationAttributeMap) {
        if (annotationAttributeMap == null) {
            return;
        }
        BaseAnnotationAttribute annotationAttributeValue = annotationAttributeMap.get(JACGCommonNameConstants.ANNOTATION_ATTRIBUTE_NAME_VALUE);
        if (annotationAttributeValue instanceof ListStringAnnotationAttribute) {
            ListStringAnnotationAttribute listStringAnnotationAttributeValue = (ListStringAnnotationAttribute) annotationAttributeValue;
            valueList.addAll(listStringAnnotationAttributeValue.getAttributeList());
        }
        BaseAnnotationAttribute annotationAttributeName = annotationAttributeMap.get(JACGCommonNameConstants.ANNOTATION_ATTRIBUTE_NAME_NAME);
        if (annotationAttributeName instanceof ListStringAnnotationAttribute) {
            ListStringAnnotationAttribute listStringAnnotationAttributeName = (ListStringAnnotationAttribute) annotationAttributeName;
            valueList.addAll(listStringAnnotationAttributeName.getAttributeList());
        }
    }

    /**
     * 使用H2数据库时使用只读模式
     *
     * @return
     */
    @Override
    protected boolean useReadOnlyMode() {
        return true;
    }

    public void setOtherDbOperWrapperList(List<DbOperWrapper> otherDbOperWrapperList) {
        this.otherDbOperWrapperList = otherDbOperWrapperList;
    }

    public void setClassNameExistsMap(Map<String, Boolean> classNameExistsMap) {
        this.classNameExistsMap = classNameExistsMap;
    }

    public void setCurrentOutputDirPath(String currentOutputDirPath) {
        this.currentOutputDirPath = currentOutputDirPath;
    }

    public void setJarInfoMap(Map<Integer, WriteDbData4JarInfo> jarInfoMap) {
        this.jarInfoMap = jarInfoMap;
    }
}
