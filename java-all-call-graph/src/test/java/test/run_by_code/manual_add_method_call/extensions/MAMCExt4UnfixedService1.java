package test.run_by_code.manual_add_method_call.extensions;

import com.adrninistrator.jacg.dto.method_call.MethodCallPair;
import com.adrninistrator.jacg.extensions.manual_add_method_call.AbstractManualAddMethodCall1;
import com.adrninistrator.javacg.util.JavaCGMethodUtil;
import test.call_graph.other_jar.AbstractUnFixedOtherJarService1;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/20
 * @description: 用于添加方法调用关系的扩展类示例，处理 AbstractUnFixedOtherJarService1 相关的类
 */
public class MAMCExt4UnfixedService1 extends AbstractManualAddMethodCall1 {

    @Override
    protected String chooseSuperOrItfClassName() {
        // AbstractUnFixedOtherJarService1类的完整类名
        return AbstractUnFixedOtherJarService1.class.getName();
    }

    @Override
    protected List<MethodCallPair> chooseAddMethodCallPairList(String className) {
        // 当AbstractUnFixedOtherJarService1子类的invoke()方法被调用时，添加调用execute()方法
        return Collections.singletonList(new MethodCallPair(JavaCGMethodUtil.formatMethodWithArgs("invoke", Object.class, Collection.class),
                JavaCGMethodUtil.formatMethodWithArgs("execute", Object.class, Collection.class)));
    }
}
