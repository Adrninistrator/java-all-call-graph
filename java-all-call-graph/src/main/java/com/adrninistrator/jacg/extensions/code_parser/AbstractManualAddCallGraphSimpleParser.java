package com.adrninistrator.jacg.extensions.code_parser;

import com.adrninistrator.javacg.dto.classes.ClassInterfaceMethodInfo;
import com.adrninistrator.javacg.dto.classes.ExtendsClassMethodInfo;
import com.adrninistrator.javacg.dto.counter.CallIdCounter;
import com.adrninistrator.javacg.dto.method.MethodCallDto;
import com.adrninistrator.javacg.enums.CallTypeEnum;
import com.adrninistrator.javacg.extensions.code_parser.CustomCodeParserInterface;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;
import org.apache.commons.lang3.StringUtils;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/4/15
 * @description: 人工添加缺失的方法调用简单基类
 */
public abstract class AbstractManualAddCallGraphSimpleParser implements CustomCodeParserInterface {

    private Map<String, ExtendsClassMethodInfo> extendsClassMethodInfoMap;
    private Map<String, ClassInterfaceMethodInfo> classInterfaceMethodInfoMap;

    private Set<String> handledClassNameSet;

    @Override
    public void init() {
        handledClassNameSet = new HashSet<>();
    }

    @Override
    public void setExtendsClassMethodInfoMap(Map<String, ExtendsClassMethodInfo> extendsClassMethodInfoMap) {
        this.extendsClassMethodInfoMap = extendsClassMethodInfoMap;
    }

    @Override
    public void setClassInterfaceMethodInfoMap(Map<String, ClassInterfaceMethodInfo> classInterfaceMethodInfoMap) {
        this.classInterfaceMethodInfoMap = classInterfaceMethodInfoMap;
    }

    @Override
    public void handleMethodCall(CallIdCounter callIdCounter, String calleeClassName, String calleeMethodName, Type[] arguments, InstructionHandle mcIh, MethodGen methodGen,
                                 List<MethodCallDto> methodCalls) {
        if (!StringUtils.equals(chooseCalleeMethodName(), calleeMethodName)) {
            // 判断被调用方法，不需要处理时跳过
            return;
        }

        // 判断被调用类的父类或接口是否为需要处理的类
        if (!checkSuperOrItfClass(calleeClassName)) {
            return;
        }

        if (handledClassNameSet.contains(calleeClassName)) {
            // 确保每个类仅处理一次
            return;
        }

        // 补充缺失的调用关系，调用者方法为当前被调用的类及方法
        String callerFullMethod = JavaCGUtil.formatFullMethod(calleeClassName, calleeMethodName, JavaCGUtil.getArgListStr(arguments));
        // 被调用的方法为当前类的指定方法
        // 以下需要使调用ID加1
        String methodCall = JavaCGUtil.formatMethodCall(callIdCounter.addAndGet(), callerFullMethod, CallTypeEnum.CTE_MA.getType(), calleeClassName,
                chooseAddCalleeMethodName(), chooseAddCalleeMethodNameArgs());

        methodCalls.add(MethodCallDto.genInstance(methodCall, 0));

        handledClassNameSet.add(calleeClassName);
    }

    /**
     * 判断被调用类的父类或接口是否为需要处理的类
     *
     * @param calleeClassName
     * @return true: 需要处理 false: 不需要处理
     */
    private boolean checkSuperOrItfClass(String calleeClassName) {
        if (chooseExtendsOrImpl()) {
            // 继承父类
            return JavaCGUtil.isChildOf(calleeClassName, chooseTopSuperOrItfClassFullName(), extendsClassMethodInfoMap);
        }
        // 实现接口
        return JavaCGUtil.isImplementationOf(calleeClassName, chooseTopSuperOrItfClassFullName(), extendsClassMethodInfoMap, classInterfaceMethodInfoMap);
    }

    /**
     * 指定是继承类还是实现接口的方式
     *
     * @return true: 继承类 false: 实现接口
     */
    protected abstract boolean chooseExtendsOrImpl();

    /**
     * 指定父类或接口完整类名
     *
     * @return
     */
    protected abstract String chooseTopSuperOrItfClassFullName();

    /**
     * 指定识别到什么名称的方法被调用时，需要补充缺失的调用关系，不需要指定括号或参数
     *
     * @return
     */
    protected abstract String chooseCalleeMethodName();

    /**
     * 指定需要补充的调用关系的被调用方法名，不需要指定括号或参数
     *
     * @return
     */
    protected abstract String chooseAddCalleeMethodName();

    /**
     * 指定需要补充的调用关系的被调用方法参数，需要包含括号，各参数也需要为完整类名形式，使用半角逗号,分隔，中间不能出现空格
     *
     * @return
     */
    protected abstract String chooseAddCalleeMethodNameArgs();

    @Override
    public String chooseSkipTopSuperClassFullName() {
        if (chooseExtendsOrImpl()) {
            // 指定不需要补充子类调用父类方法/父类调用子类方法的顶层父类完整类名，仅当通过继承时需要指定
            return chooseTopSuperOrItfClassFullName();
        }

        return null;
    }
}
