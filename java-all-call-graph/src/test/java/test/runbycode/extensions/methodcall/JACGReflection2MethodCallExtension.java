package test.runbycode.extensions.methodcall;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.extensions.methodcall.JACGObjInstanceMethodNameMCE;

/**
 * @author adrninistrator
 * @date 2025/2/17
 * @description: java-all-call-graph 组件方法调用处理扩展类
 * 通过方法调用参数添加方法调用，使用被调用对象，及被调用方法名称
 * 对应的反射工具类方法为 test.callgraph.reflection1.util.TestReflectionUtil1#runByReflection(java.lang.String, java.lang.Object, java.lang.String, java.lang.Object...)
 */
public class JACGReflection2MethodCallExtension extends JACGObjInstanceMethodNameMCE {

    public JACGReflection2MethodCallExtension(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected int chooseCalleeObjArgSeq() {
        return 2;
    }

    @Override
    protected int chooseCalleeMethodNameArgSeq() {
        return 3;
    }

    @Override
    protected String chooseMethodCallType() {
        return JavaCG2Reflection2MethodCallExtension.CALL_TYPE;
    }
}
