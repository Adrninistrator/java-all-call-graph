package test.runbycode.extensions.methodcall.commonslang3methodutils;

import com.adrninistrator.jacg.extensions.methodcall.JavaCG2ObjInstanceMethodNameMCE;
import org.apache.commons.lang3.reflect.MethodUtils;

/**
 * @author adrninistrator
 * @date 2025/11/17
 * @description: java-callgraph2 组件方法调用处理扩展类
 * 通过指定方法的调用参数添加方法调用，使用被调用对象，及被调用方法名
 * 对应的反射工具类方法为 org.apache.commons.lang3.reflect.MethodUtils:invokeMethod(java.lang.Object, java.lang.String)
 */
public class JavaCG2CommonsLang3MethodUtilsMethodCallExtension extends JavaCG2ObjInstanceMethodNameMCE {

    public static final String CALL_TYPE = "commons_lang3_method_utils";

    @Override
    protected boolean checkHandleCalleeMethod(String calleeClassName, String calleeMethodName, String calleeMethodArgTypes) {
        return "invokeMethod".equals(calleeMethodName) &&
                MethodUtils.class.getName().equals(calleeClassName) &&
                "(java.lang.Object,java.lang.String)".equals(calleeMethodArgTypes);
    }

    @Override
    protected String chooseMethodCallType() {
        return CALL_TYPE;
    }
}
