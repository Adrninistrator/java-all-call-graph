package test.extensions.method_call_add;

import com.adrninistrator.jacg.extensions.method_call_add.AbstractMethodCallAdd4ExtendsImpl;
import test.call_graph.manual_add_callgraph.unfixed.AbstractUnFixedService1;

/**
 * @author adrninistrator
 * @date 2022/11/20
 * @description: 用于添加方法调用关系的扩展类示例，处理AbstractUnFixedService1类
 */
public class MCAExt4UnfixedService1 extends AbstractMethodCallAdd4ExtendsImpl {
    @Override
    protected String chooseCalleeSuperClassOrInterfaceName() {
        // AbstractUnFixedService1类的完整类名
        return AbstractUnFixedService1.class.getName();
    }

    @Override
    protected String chooseCalleeMethodName() {
        // 当AbstractUnFixedService1子类的invoke()方法被调用时，添加缺失的方法调用关系，该方法名作为调用方法
        return "invoke";
    }

    @Override
    protected String chooseAddCalleeMethodName() {
        // 添加缺失的方法调用关系，被调用方法为execute
        return "execute";
    }

    @Override
    protected String chooseAddCalleeMethodNameArgs() {
        // 添加缺失的方法调用关系，被调用方法参数类型为java.lang.Object,java.util.Collection
        return "(java.lang.Object,java.util.Collection)";
    }
}
