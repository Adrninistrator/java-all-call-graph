package test.runbycode.handler.methodcall.handler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.methodcall.BaseMethodCallByEEDetailHandler;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/7/4
 * @description:
 */
public class RecordCallerMethodCallByEEDetailHandler extends BaseMethodCallByEEDetailHandler {

    private final Set<FullMethodWithReturnType> indexOfCallerMethodSet = new HashSet<>();

    private final Set<FullMethodWithReturnType> substringCallerMethodSet = new HashSet<>();

    public RecordCallerMethodCallByEEDetailHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public RecordCallerMethodCallByEEDetailHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    protected boolean chooseQueryObjArgsInfoInMethodCall() {
        // 不需要查询方法调用中被调用对象与参数使用的信息
        return false;
    }

    @Override
    protected void handleMethodWithArgs(WriteDbData4MethodCall methodCall, MethodDetailNoReturnType callerMethodDetailNoReturnType,
                                        MethodDetailNoReturnType calleeMethodDetailNoReturnType, ObjArgsInfoInMethodCall objArgsInfoInMethodCall, Object... args) {
        String calleeMethodName = methodCall.getCalleeMethodName();
        if (calleeMethodName.contains("indexOf") || calleeMethodName.contains("IndexOf")) {
            indexOfCallerMethodSet.add(new FullMethodWithReturnType(methodCall.getCallerFullMethod(), methodCall.getCallerReturnType()));
        } else if (calleeMethodName.contains("substring")) {
            substringCallerMethodSet.add(new FullMethodWithReturnType(methodCall.getCallerFullMethod(), methodCall.getCallerReturnType()));
        }
    }

    public Set<FullMethodWithReturnType> getIndexOfCallerMethodSet() {
        return indexOfCallerMethodSet;
    }

    public Set<FullMethodWithReturnType> getSubstringCallerMethodSet() {
        return substringCallerMethodSet;
    }
}
