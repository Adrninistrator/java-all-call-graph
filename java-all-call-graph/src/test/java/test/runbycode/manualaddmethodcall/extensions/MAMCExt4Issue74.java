package test.runbycode.manualaddmethodcall.extensions;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.methodcall.MethodCallPair;
import com.adrninistrator.jacg.extensions.manualaddmethodcall.AbstractManualAddMethodCall1;
import test.callgraph.manualaddmethodcall.issue74.NotFoundCallBack;

import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/4/23
 * @description: 用于添加方法调用关系的扩展类示例，处理 test.callgraph.manualaddmethodcall.issue74.NotFoundCallBack 相关的类
 */
public class MAMCExt4Issue74 extends AbstractManualAddMethodCall1 {

    public MAMCExt4Issue74(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected String chooseSuperOrItfClassName() {
        // test.callgraph.manualaddmethodcall.issue74.NotFoundCallBack 类的完整类名
        return NotFoundCallBack.class.getName();
    }

    @Override
    protected List<MethodCallPair> chooseAddMethodCallPairList(String className) {
        // 当 test.callgraph.manualaddmethodcall.issue74.NotFoundCallBack 子类的 <init>() 方法被调用时，添加调用 execute() 方法
        return Collections.singletonList(new MethodCallPair("<init>()", "execute()"));
    }
}
