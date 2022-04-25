package test.extensions.code_parser;

import com.adrninistrator.jacg.extensions.code_parser.AbstractManualAddCallGraphSimpleParser;

/**
 * @author adrninistrator
 * @date 2022/4/21
 * @description:
 */
public class MACGUnfixedService1Parser extends AbstractManualAddCallGraphSimpleParser {
    @Override
    public String chooseTopSuperOrItfClassFullName() {
        // AbstractUnFixedService1类的完整类名
        return "test.call_graph.manual_add_callgraph.unfixed.AbstractUnFixedService1";
    }

    @Override
    protected boolean chooseExtendsOrImpl() {
        // 在使用AbstractUnFixedService1时，是继承父类的方式，返回true
        return true;
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
