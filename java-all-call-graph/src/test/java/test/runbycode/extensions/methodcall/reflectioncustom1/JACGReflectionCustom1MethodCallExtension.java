package test.runbycode.extensions.methodcall.reflectioncustom1;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.extensions.methodcall.JACGObjInstanceMethodNameMCE;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description: java-all-call-graph 组件方法调用处理扩展类
 * 通过指定方法的调用参数添加方法调用，使用被调用对象，及被调用方法名
 * 对应的反射工具类方法为 test.callgraph.reflectioncustom.util.TestReflectionCustomUtil1:runByReflection(java.lang.Object, java.lang.String, java.lang.Object...)
 */
public class JACGReflectionCustom1MethodCallExtension extends JACGObjInstanceMethodNameMCE {

    public JACGReflectionCustom1MethodCallExtension(DbOperWrapper dbOperWrapper) {
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
        return JavaCG2ReflectionCustom1MethodCallExtension.CALL_TYPE;
    }
}
