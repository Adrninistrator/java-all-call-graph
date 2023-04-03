package com.adrninistrator.jacg.extensions.manual_add_method_call;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.method_call.MethodCallPair;
import com.adrninistrator.jacg.handler.extends_impl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.handler.method.MethodCallHandler;
import com.adrninistrator.javacg.util.JavaCGMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/24
 * @description: 人工添加方法调用关系，处理特定的子类与实现类，当前类仅判断了方法名称，未判断方法参数
 */
public abstract class AbstractManualAddMethodCall1 {
    private static final Logger logger = LoggerFactory.getLogger(AbstractManualAddMethodCall1.class);

    protected DbOperator dbOperator;

    protected DbOperWrapper dbOperWrapper;

    protected JACGExtendsImplHandler jacgExtendsImplHandler;

    protected MethodCallHandler methodCallHandler;

    /**
     * 选择需要处理的父类或接口的完整类名
     *
     * @return
     */
    protected abstract String chooseSuperOrItfClassName();

    /**
     * 选择需要添加的调用方法+参数，以及被调用方法+参数对应的列表
     * MethodCallPair中只需要设置callerFullMethod、calleeFullMethod
     * callerFullMethod：需要添加的调用方法+参数，例如ActionListener实现类的构造函数<init>
     * calleeFullMethod：需要添加的被调用方法+参数
     * 若以上方法没有参数，则不需要返回参数，直接返回()
     * 若以上方法有参数，则需要返回参数，如(java.lang.String,int)
     *
     * @param className 当前的类名
     * @return
     */
    protected abstract List<MethodCallPair> chooseAddMethodCallPairList(String className);

    /**
     * 添加方法调用
     *
     * @return
     */
    public boolean addMethodCall() {
        // 获取父类或接口的完整类名
        String superOrItfClassName = chooseSuperOrItfClassName();

        // 查询父类或接口的子类或实现类，仅查询非抽象类
        List<String> childOrImplClassList = jacgExtendsImplHandler.queryChildClassListByFull(superOrItfClassName, false, true, false, true);
        if (JavaCGUtil.isCollectionEmpty(childOrImplClassList)) {
            logger.error("未查询到指定的子类或实现类 {}", superOrItfClassName);
            return true;
        }

        for (String childOrImplClass : childOrImplClassList) {
            // 不需要判断对应方法是否有被调用，因为可能通过获取Spring Bean的方式被调用
            List<MethodCallPair> methodCallPairList = chooseAddMethodCallPairList(childOrImplClass);
            if (JavaCGUtil.isCollectionEmpty(methodCallPairList)) {
                continue;
            }

            for (MethodCallPair methodCallPair : methodCallPairList) {
                String calleeFullMethod = JavaCGMethodUtil.formatFullMethodWithArgs(childOrImplClass, methodCallPair.getCallerFullMethod());
                // 生成需要添加的方法调用关系的被调用完整方法
                String addCalleeFullMethod = JavaCGMethodUtil.formatFullMethodWithArgs(childOrImplClass, methodCallPair.getCalleeFullMethod());
                // 添加方法调用
                if (!methodCallHandler.manualAddMethodCall(calleeFullMethod, addCalleeFullMethod)) {
                    return false;
                }
            }
        }
        return true;
    }

    public void setDbOperator(DbOperator dbOperator) {
        this.dbOperator = dbOperator;
    }

    public void setDbOperWrapper(DbOperWrapper dbOperWrapper) {
        this.dbOperWrapper = dbOperWrapper;
    }

    public void setJacgExtendsImplHandler(JACGExtendsImplHandler jacgExtendsImplHandler) {
        this.jacgExtendsImplHandler = jacgExtendsImplHandler;
    }

    public void setMethodCallHandler(MethodCallHandler methodCallHandler) {
        this.methodCallHandler = methodCallHandler;
    }
}
