package com.adrninistrator.jacg.extensions.method_call_add;

import com.adrninistrator.jacg.dto.method.MethodCallFullMethod;
import com.adrninistrator.jacg.handler.extends_impl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.util.JavaCGUtil;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/19
 * @description: 人工添加方法调用关系，处理继承与实现相关的抽象父类，当前类仅判断了方法名称，未判断方法参数
 */
public abstract class AbstractMethodCallAdd4ExtendsImpl implements MethodCallAddInterface {
    private final Set<String> handledClassMethodNameSet = new HashSet<>();

    // 继承与实际相关的处理类
    private JACGExtendsImplHandler jacgExtendsImplHandler;

    /**
     * 选择需要处理的被调用类的父类或接口类名
     *
     * @return
     */
    protected abstract String chooseCalleeSuperClassOrInterfaceName();

    /**
     * 选择需要处理的被调用类方法
     *
     * @return
     */
    protected abstract String chooseCalleeMethodName();

    /**
     * 选择需要添加的被调用方法名称
     *
     * @return
     */
    protected abstract String chooseAddCalleeMethodName();

    /**
     * 选择需要添加的被调用方法参数
     * 示例如下
     * ()
     * (java.lang.String)
     * (java.lang.String,int)
     *
     * @return
     */
    protected abstract String chooseAddCalleeMethodNameArgs();

    @Override
    public MethodCallFullMethod handleMethodCall(String callerFullMethod, String calleeFullMethod, String calleeClassName) {
        String calleeSuperClassOrInterfaceName = chooseCalleeSuperClassOrInterfaceName();
        // 判断当前被调用类是否为指定父类/接口的子类/实现类
        if (!jacgExtendsImplHandler.checkExtendsOrImplFull(calleeSuperClassOrInterfaceName, calleeClassName)) {
            // 当前被调用类不是指定父类/接口的子类/实现类，当前方法调用关系不需要处理
            return null;
        }

        String calleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
        if (!calleeMethodName.equals(chooseCalleeMethodName())) {
            // 被调用方法不是指定方法，不处理
            return null;
        }

        String calleeClassMethodName = calleeClassName + JavaCGConstants.FLAG_COLON + calleeMethodName;
        if (!handledClassMethodNameSet.add(calleeClassMethodName)) {
            // 已经处理过的方法，不再处理
            return null;
        }

        // 生成需要添加的方法调用关系的被调用完整方法
        String addCalleeFullMethod = JavaCGUtil.formatFullMethod(calleeClassName, chooseAddCalleeMethodName(), chooseAddCalleeMethodNameArgs());

        // 添加的方法调用关系，使用当前被调用的方法作为调用方法
        return new MethodCallFullMethod(calleeFullMethod, addCalleeFullMethod);
    }

    //
    public void setJacgExtendsImplHandler(JACGExtendsImplHandler jacgExtendsImplHandler) {
        this.jacgExtendsImplHandler = jacgExtendsImplHandler;
    }
}
