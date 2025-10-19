package test.runbycode.extensions.methodcall;

import com.adrninistrator.jacg.extensions.methodcall.JavaCG2ObjInstanceMethodNameMCE;
import test.callgraph.reflection1.util.TestReflectionUtil1;

/**
 * @author adrninistrator
 * @date 2025/2/17
 * @description: java-callgraph2 组件方法调用处理扩展类
 * 通过方法调用参数添加方法调用，使用被调用对象，及被调用方法名
 * 对应的反射工具类方法为 test.callgraph.reflection1.util.TestReflectionUtil1#runByReflection(java.lang.String, java.lang.Object, java.lang.String, java.lang.Object...)
 */
public class JavaCG2Reflection2MethodCallExtension extends JavaCG2ObjInstanceMethodNameMCE {

    public static final String CALL_TYPE = "test_runByReflection2";

    @Override
    protected boolean checkHandleCalleeMethod(String calleeClassName, String calleeMethodName, String calleeMethodArgTypes) {
        return "runByReflection".equals(calleeMethodName) &&
                TestReflectionUtil1.class.getName().equals(calleeClassName) &&
                "(java.lang.String,java.lang.Object,java.lang.String,java.lang.Object[])".equals(calleeMethodArgTypes);
    }

    @Override
    protected String chooseMethodCallType() {
        return CALL_TYPE;
    }
}
