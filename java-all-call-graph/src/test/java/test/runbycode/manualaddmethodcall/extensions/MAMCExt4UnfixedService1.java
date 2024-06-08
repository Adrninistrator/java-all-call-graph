package test.runbycode.manualaddmethodcall.extensions;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.methodcall.MethodCallPair;
import com.adrninistrator.jacg.extensions.manualaddmethodcall.AbstractManualAddMethodCall1;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import test.callgraph.otherjar.AbstractUnFixedOtherJarService1;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/20
 * @description: 用于添加方法调用关系的扩展类示例，处理 AbstractUnFixedOtherJarService1 相关的类
 */
public class MAMCExt4UnfixedService1 extends AbstractManualAddMethodCall1 {

    public MAMCExt4UnfixedService1(DbOperator dbOperator, DbOperWrapper dbOperWrapper) {
        super(dbOperator, dbOperWrapper);
    }

    @Override
    protected String chooseSuperOrItfClassName() {
        // AbstractUnFixedOtherJarService1类的完整类名
        return AbstractUnFixedOtherJarService1.class.getName();
    }

    @Override
    protected List<MethodCallPair> chooseAddMethodCallPairList(String className) {
        // 当AbstractUnFixedOtherJarService1子类的invoke()方法被调用时，添加调用execute()方法
        return Collections.singletonList(new MethodCallPair(JavaCGClassMethodUtil.formatMethodWithArgTypes("invoke", Object.class, Collection.class),
                JavaCGClassMethodUtil.formatMethodWithArgTypes("execute", Object.class, Collection.class)));
    }
}
