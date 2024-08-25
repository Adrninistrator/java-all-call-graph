package test.runbycode.manualaddmethodcall.extensions;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.methodcall.MethodCallPair;
import com.adrninistrator.jacg.extensions.manualaddmethodcall.AbstractManualAddMethodCall1;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;

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

    public MAMCExt4ActionListener(DbOperator dbOperator, DbOperWrapper dbOperWrapper) {
        super(dbOperator, dbOperWrapper);
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
        List<String> fullMethodList = methodInfoHandler.queryMethodByClassMethod(className, JavaCGCommonNameConstants.METHOD_NAME_INIT);
        if (JavaCGUtil.isCollectionEmpty(fullMethodList)) {
            return null;
        }

        List<MethodCallPair> methodCallPairList = new ArrayList<>(fullMethodList.size());
        for (String fullMethod : fullMethodList) {
            // 当ActionListener接口实现类的构造函数被调用时，添加调用actionPerformed()方法
            String methodAndArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(fullMethod);
            return Collections.singletonList(new MethodCallPair(methodAndArgs, JavaCGClassMethodUtil.formatMethodWithArgTypes("actionPerformed", ActionEvent.class)));
        }
        return methodCallPairList;
    }
}
