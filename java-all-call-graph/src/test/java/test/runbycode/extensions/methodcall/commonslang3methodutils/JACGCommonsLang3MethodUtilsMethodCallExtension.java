package test.runbycode.extensions.methodcall.commonslang3methodutils;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.extensions.methodcall.JACGObjInstanceMethodNameMCE;

/**
 * @author adrninistrator
 * @date 2025/11/17
 * @description: java-all-call-graph 组件方法调用处理扩展类
 * 通过指定方法的调用参数添加方法调用，使用被调用对象，及被调用方法名
 * 对应的反射工具类方法为 org.apache.commons.lang3.reflect.MethodUtils:invokeMethod(java.lang.Object, java.lang.String)
 */
public class JACGCommonsLang3MethodUtilsMethodCallExtension extends JACGObjInstanceMethodNameMCE {

    public JACGCommonsLang3MethodUtilsMethodCallExtension(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected int chooseCalleeObjArgSeq() {
        return 1;
    }

    @Override
    protected int chooseCalleeMethodNameArgSeq() {
        return 2;
    }

    @Override
    protected String chooseMethodCallType() {
        return JavaCG2CommonsLang3MethodUtilsMethodCallExtension.CALL_TYPE;
    }
}
