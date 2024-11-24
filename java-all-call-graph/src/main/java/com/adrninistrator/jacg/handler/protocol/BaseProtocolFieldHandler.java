package com.adrninistrator.jacg.handler.protocol;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodArgument;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringController;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.dto.field.CommonFieldInfoInClass;
import com.adrninistrator.jacg.handler.field.FieldInfoHandler;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/10/27
 * @description: 查询传输协议字段处理类，基类
 */
public abstract class BaseProtocolFieldHandler extends BaseHandler {

    protected final ClassInfoHandler classInfoHandler;
    protected final MethodArgReturnHandler methodArgReturnHandler;
    protected final MethodInfoHandler methodInfoHandler;
    protected final FieldInfoHandler fieldInfoHandler;
    protected final SpringHandler springHandler;

    public BaseProtocolFieldHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        fieldInfoHandler = new FieldInfoHandler(dbOperWrapper);
        springHandler = new SpringHandler(dbOperWrapper);
    }

    public BaseProtocolFieldHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        fieldInfoHandler = new FieldInfoHandler(dbOperWrapper);
        springHandler = new SpringHandler(dbOperWrapper);
    }

    /**
     * 处理Spring Controller某个方法的某个参数
     *
     * @param springController Spring Controller信息
     * @param methodArgument   Spring Controller方法参数
     */
    protected void doHandleSPCMethodArg(WriteDbData4SpringController springController, WriteDbData4MethodArgument methodArgument) {
    }

    /**
     * 处理Spring Controller某个方法的某个自定义类型参数中的常用数据类型的字段信息
     *
     * @param springController       Spring Controller信息
     * @param methodArgument         Spring Controller方法参数
     * @param commonFieldInfoInClass Spring Controller某个方法的某个自定义类型参数中的常用数据类型的字段信息
     */
    protected void doHandleCommonFieldInfoInSPCCustomArg(WriteDbData4SpringController springController, WriteDbData4MethodArgument methodArgument,
                                                         CommonFieldInfoInClass commonFieldInfoInClass) {
    }

    /**
     * 处理Spring Controller某个方法的某个参数的泛型类型中的常用数据类型的字段信息
     *
     * @param springController       Spring Controller信息
     * @param methodArgument         Spring Controller方法参数
     * @param methodArgGenericsType  Spring Controller方法参数泛型类型
     * @param commonFieldInfoInClass Spring Controller某个方法的某个自定义类型参数中的常用数据类型的字段信息
     */
    protected void doHandleCommonFieldInfoInSPCArgGT(WriteDbData4SpringController springController, WriteDbData4MethodArgument methodArgument,
                                                     String methodArgGenericsType, CommonFieldInfoInClass commonFieldInfoInClass) {
    }

    /**
     * 处理Spring Controller某个方法的返回类型中的常用数据类型的字段信息
     *
     * @param springController       Spring Controller信息
     * @param methodInfo             Spring Controller方法信息
     * @param commonFieldInfoInClass Spring Controller某个方法的返回类型中的常用数据类型的字段信息
     */
    protected void doHandleCommonFieldInfoInSPCReturnType(WriteDbData4SpringController springController, WriteDbData4MethodInfo methodInfo,
                                                          CommonFieldInfoInClass commonFieldInfoInClass) {
    }

    /**
     * 处理Spring Controller某个方法的返回类型的泛型类型中的常用数据类型的字段信息
     *
     * @param springController         Spring Controller信息
     * @param methodInfo               Spring Controller方法信息
     * @param methodReturnGenericsType Spring Controller某个方法的返回类型中的泛型类型
     * @param commonFieldInfoInClass   Spring Controller某个方法的返回类型中的泛型类型中的常用数据类型的字段信息
     */
    protected void doHandleCommonFieldInfoInSPCReturnTypeGT(WriteDbData4SpringController springController, WriteDbData4MethodInfo methodInfo, String methodReturnGenericsType,
                                                            CommonFieldInfoInClass commonFieldInfoInClass) {
    }

    /**
     * 处理其他类中的常用数据类型的字段信息
     *
     * @param className              类名
     * @param commonFieldInfoInClass 当前类中的常用数据类型的字段信息
     */
    protected void doHandleCommonFieldInfoInOtherClass(String className, CommonFieldInfoInClass commonFieldInfoInClass) {
    }

    /**
     * 处理其他传输协议字段
     *
     * @param customTypeSet 有处理过的自定义类型Set
     */
    protected void handleOtherProtocolField(Set<String> customTypeSet) {
    }

    // 处理传输协议字段
    public void handle() {
        // 保存有处理过的自定义类型Set
        Set<String> customTypeSet = new HashSet<>();
        // 处理Spring Controller传输协议字段
        handleSpringControllerField(customTypeSet);
        // 处理其他传输协议字段
        handleOtherProtocolField(customTypeSet);
        // 处理未在以上步骤中处理过的包含字段的类
        handleOtherClassContainsField(customTypeSet);
    }

    /**
     * 处理Spring Controller传输协议字段
     *
     * @param customTypeSet
     */
    private void handleSpringControllerField(Set<String> customTypeSet) {
        // 查询Spring Controller对应的全部简单类名
        List<String> simpleClassNameList = springHandler.queryAllControllerSCN();
        for (String simpleClassName : simpleClassNameList) {
            // 根据简单类名查询对应的Spring Controller
            List<WriteDbData4SpringController> springControllerList = springHandler.queryControllerBySCN(simpleClassName);
            for (WriteDbData4SpringController springController : springControllerList) {
                // 处理Spring Controller方法参数
                handleSpringControllerMethodArg(springController, customTypeSet);
                // 处理Spring Controller方法返回类型
                handleSpringControllerReturnType(springController, customTypeSet);
            }
        }
    }

    // 处理Spring Controller方法参数
    private void handleSpringControllerMethodArg(WriteDbData4SpringController springController, Set<String> customTypeSet) {
        // 查询Spring Controller方法参数
        List<WriteDbData4MethodArgument> methodArgumentList = methodArgReturnHandler.queryMethodArgumentByMethod(springController.getFullMethod());
        if (JavaCG2Util.isCollectionEmpty(methodArgumentList)) {
            // 当前Spring Controller方法没有参数
            return;
        }
        for (WriteDbData4MethodArgument methodArgument : methodArgumentList) {
            // 处理Spring Controller某个方法的某个参数
            doHandleSPCMethodArg(springController, methodArgument);

            if (JavaCG2Constants.FILE_KEY_CATEGORY_CUSTOM.equals(methodArgument.getArgCategory())) {
                // 记录有处理过的自定义类型
                customTypeSet.add(methodArgument.getArgType());
                // 查询Spring Controller某个方法的某个自定义类型参数中的全部常用数据类型的字段信息
                List<CommonFieldInfoInClass> commonFieldInfoInClassList = fieldInfoHandler.queryAllCommonFieldInfoInClass(methodArgument.getArgType(), true, true, true,
                        customTypeSet);
                for (CommonFieldInfoInClass commonFieldInfoInClass : commonFieldInfoInClassList) {
                    // 处理Spring Controller某个方法的某个自定义类型参数中的常用数据类型的字段信息
                    doHandleCommonFieldInfoInSPCCustomArg(springController, methodArgument, commonFieldInfoInClass);
                }
            }

            if (JavaCG2YesNoEnum.isYes(methodArgument.getExistsGenericsType())) {
                // 查询指定方法指定的参数中的泛型类型
                List<String> methodArgGenericsTypeList =
                        methodArgReturnHandler.queryCustomTypeInMethodArgGenerics(springController.getFullMethod(), methodArgument.getArgSeq());
                // 记录有处理过的自定义类型
                customTypeSet.addAll(methodArgGenericsTypeList);
                for (String methodArgGenericsType : methodArgGenericsTypeList) {
                    // 查询Spring Controller某个方法的某个自定义类型参数中的全部常用数据类型的字段信息
                    List<CommonFieldInfoInClass> commonFieldInfoInClassList = fieldInfoHandler.queryAllCommonFieldInfoInClass(methodArgGenericsType, true, true, true,
                            customTypeSet);
                    for (CommonFieldInfoInClass commonFieldInfoInClass : commonFieldInfoInClassList) {
                        // 处理Spring Controller某个方法的某个参数的泛型类型中的常用数据类型的字段信息
                        doHandleCommonFieldInfoInSPCArgGT(springController, methodArgument, methodArgGenericsType, commonFieldInfoInClass);
                    }
                }
            }
        }
    }

    // 处理Spring Controller方法返回类型
    private void handleSpringControllerReturnType(WriteDbData4SpringController springController, Set<String> customTypeSet) {
        // 查询Spring Controller方法信息
        WriteDbData4MethodInfo methodInfo = methodInfoHandler.queryMethodInfoByFullMethod(springController.getFullMethod());
        if (JavaCG2CommonNameConstants.RETURN_TYPE_VOID.equals(methodInfo.getReturnType())) {
            // 当前Spring Controller方法返回void
            return;
        }

        if (JavaCG2Constants.FILE_KEY_CATEGORY_CUSTOM.equals(methodInfo.getReturnCategory())) {
            // 记录有处理过的自定义类型
            customTypeSet.add(methodInfo.getReturnType());
            // 查询Spring Controller某个方法的某个自定义类型参数中的全部常用数据类型的字段信息
            List<CommonFieldInfoInClass> commonFieldInfoInClassList = fieldInfoHandler.queryAllCommonFieldInfoInClass(methodInfo.getReturnType(), true, true, true,
                    customTypeSet);
            for (CommonFieldInfoInClass commonFieldInfoInClass : commonFieldInfoInClassList) {
                // 处理Spring Controller某个方法的返回类型中的常用数据类型的字段信息
                doHandleCommonFieldInfoInSPCReturnType(springController, methodInfo, commonFieldInfoInClass);
            }
        }

        if (JavaCG2YesNoEnum.isYes(methodInfo.getReturnExistsGenericsType())) {
            // 查询指定方法返回类型中泛型类型中出现的自定义类型
            List<String> methodReturnGenericsCustomTypeList =
                    methodArgReturnHandler.queryCustomTypeInMethodReturnGenerics(springController.getFullMethod());
            // 记录有处理过的自定义类型
            customTypeSet.addAll(methodReturnGenericsCustomTypeList);
            for (String methodReturnGenericsType : methodReturnGenericsCustomTypeList) {
                // 查询Spring Controller某个方法的某个自定义类型参数中的全部常用数据类型的字段信息
                List<CommonFieldInfoInClass> commonFieldInfoInClassList = fieldInfoHandler.queryAllCommonFieldInfoInClass(methodReturnGenericsType, true, true, true,
                        customTypeSet);
                for (CommonFieldInfoInClass commonFieldInfoInClass : commonFieldInfoInClassList) {
                    // 处理Spring Controller某个方法的返回类型的泛型类型中的常用数据类型的字段信息
                    doHandleCommonFieldInfoInSPCReturnTypeGT(springController, methodInfo, methodReturnGenericsType, commonFieldInfoInClass);
                }
            }
        }
    }

    // 处理未在以上步骤中处理过的包含字段的类
    private void handleOtherClassContainsField(Set<String> customTypeSet) {
        // 查询所有包含字段的简单类名
        List<String> simpleClassNameList = fieldInfoHandler.queryAllSimpleClassName();
        for (String simpleClassName : simpleClassNameList) {
            // 查询对应的完整类名
            String className = classInfoHandler.queryClassNameBySimple(simpleClassName);
            if (!JavaCG2ClassMethodUtil.isCustomType(className) || customTypeSet.contains(className)) {
                // 跳过非自定义类型，或在之前步骤处理过的自定义类型
                continue;
            }
            // 查询当前类中的全部常用数据类型的字段信息
            List<CommonFieldInfoInClass> commonFieldInfoInClassList = fieldInfoHandler.queryAllCommonFieldInfoInClass(className, true, true, true, null);
            for (CommonFieldInfoInClass commonFieldInfoInClass : commonFieldInfoInClassList) {
                // 处理当前类中的常用数据类型的字段信息
                doHandleCommonFieldInfoInOtherClass(className, commonFieldInfoInClass);
            }
        }
    }
}
