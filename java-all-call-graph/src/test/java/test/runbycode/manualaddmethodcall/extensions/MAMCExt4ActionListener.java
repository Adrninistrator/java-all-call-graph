package test.runbycode.manualaddmethodcall.extensions;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.methodcall.MethodCallPair;
import com.adrninistrator.jacg.extensions.manualaddmethodcall.AbstractManualAddMethodCall1;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/20
 * @description: 用于添加方法调用关系的扩展类示例，处理ActionListener
 */
public class MAMCExt4ActionListener extends AbstractManualAddMethodCall1 {

    private final MethodInfoHandler methodInfoHandler;

    public MAMCExt4ActionListener(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
    }

    @Override
    protected String chooseSuperOrItfClassName() {
        // ActionListener接口的完整类名
        return ActionListener.class.getName();
    }

    @Override
    protected List<MethodCallPair> chooseAddMethodCallPairList(String className) {
        // ActionListener实现类使用匿名内部类形式时，构造函数参数类型不固定，需要查询当前类的所有构造函数并处理
        List<FullMethodWithReturnType> methodList = methodInfoHandler.queryMethodByClassMethod(className, JavaCG2CommonNameConstants.METHOD_NAME_INIT);
        if (JavaCG2Util.isCollectionEmpty(methodList)) {
            return null;
        }

        List<MethodCallPair> methodCallPairList = new ArrayList<>(methodList.size());
        for (FullMethodWithReturnType fullMethodWithReturnType : methodList) {
            // 当 ActionListener 接口实现类的构造函数被调用时，添加调用 actionPerformed()方法
            String methodAndArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(fullMethodWithReturnType.getFullMethod());
            return Collections.singletonList(new MethodCallPair(methodAndArgs,
                    fullMethodWithReturnType.getReturnType(),
                    JavaCG2ClassMethodUtil.formatMethodWithArgTypes("actionPerformed", ActionEvent.class),
                    "void"));
        }
        return methodCallPairList;
    }
}
