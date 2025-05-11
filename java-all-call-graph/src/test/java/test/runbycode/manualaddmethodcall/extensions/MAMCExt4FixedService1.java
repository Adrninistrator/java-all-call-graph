package test.runbycode.manualaddmethodcall.extensions;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.methodcall.MethodCallPair;
import com.adrninistrator.jacg.extensions.manualaddmethodcall.AbstractManualAddMethodCall1;
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
        // 当 AbstractFixedOtherJarService1 子类的 invoke() 方法被调用时，添加调用 execute() 方法
        return Collections.singletonList(new MethodCallPair("invoke()", String.class.getName(),
                "execute()", String.class.getName()));
    }
}
