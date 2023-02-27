package test.extensions.method_call_add;

import com.adrninistrator.jacg.extensions.method_call_add.AbstractMethodCallAdd4ExtendsImpl;
import com.adrninistrator.javacg.common.JavaCGConstants;
import test.call_graph.manual_add_callgraph.fixed.AbstractFixedService1;

/**
 * @author adrninistrator
 * @date 2022/11/20
 * @description: 用于添加方法调用关系的扩展类示例，处理AbstractFixedService1类
 */
public class MCAExt4FixedService1 extends AbstractMethodCallAdd4ExtendsImpl {
    @Override
    protected String chooseCalleeSuperClassOrInterfaceName() {
        // AbstractFixedService1类的完整类名
        return AbstractFixedService1.class.getName();
    }

    @Override
    protected String chooseCalleeMethodName() {
        // 当AbstractFixedService1子类的invoke()方法被调用时，添加缺失的方法调用关系，该方法名作为调用方法
        return "invoke";
    }

    @Override
    protected String chooseAddCalleeMethodName() {
        // 添加缺失的方法调用关系，被调用方法为execute
        return "execute";
    }

    @Override
    protected String chooseAddCalleeMethodNameArgs() {
        // 添加缺失的方法调用关系，被调用方法参数为空
        return JavaCGConstants.EMPTY_METHOD_ARGS;
    }
}
