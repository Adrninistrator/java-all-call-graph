package test.runbycode.manualaddmethodcall.extensions;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.methodcall.MethodCallPair;
import com.adrninistrator.jacg.extensions.manualaddmethodcall.AbstractManualAddMethodCall1;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import test.callgraph.otherjar.AbstractFixedOtherJarService1;

import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/20
 * @description: 用于添加方法调用关系的扩展类示例，处理 AbstractFixedOtherJarService1 相关的类
 */
public class MAMCExt4FixedService1 extends AbstractManualAddMethodCall1 {

    public MAMCExt4FixedService1(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected String chooseSuperOrItfClassName() {
        // AbstractFixedOtherJarService1类的完整类名
        return AbstractFixedOtherJarService1.class.getName();
    }

    @Override
    protected List<MethodCallPair> chooseAddMethodCallPairList(String className) {
        // 当AbstractFixedOtherJarService1子类的invoke()方法被调用时，添加调用execute()方法
        return Collections.singletonList(new MethodCallPair(JavaCG2ClassMethodUtil.formatMethodWithArgTypesStr("invoke"),
                JavaCG2ClassMethodUtil.formatMethodWithArgTypesStr("execute")));
    }
}
