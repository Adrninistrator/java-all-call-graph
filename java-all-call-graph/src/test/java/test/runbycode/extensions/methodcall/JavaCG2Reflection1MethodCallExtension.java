package test.runbycode.extensions.methodcall;

import com.adrninistrator.jacg.extensions.methodcall.JavaCG2ObjInstanceMethodNameMCE;
import test.callgraph.reflection1.util.TestReflectionUtil1;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description: java-callgraph2 组件方法调用处理扩展类
 * 通过方法调用参数添加方法调用，使用被调用对象，及被调用方法名称
 * 对应的反射工具类方法为 test.callgraph.reflection1.util.TestReflectionUtil1:runByReflection(java.lang.Object, java.lang.String, java.lang.Object...)
 */
public class JavaCG2Reflection1MethodCallExtension extends JavaCG2ObjInstanceMethodNameMCE {

    public static final String CALL_TYPE = "test_runByReflection1";

    @Override
    protected String chooseCalleeClassName() {
        return TestReflectionUtil1.class.getName();
    }

    @Override
    protected String chooseCalleeMethodName() {
        return "runByReflection";
    }

    @Override
    protected String chooseMethodCallType() {
        return CALL_TYPE;
    }
}
