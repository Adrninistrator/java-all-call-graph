package test.runbycode.customflow.methodcallargs.handler;

import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.methodcall.MethodCallInfo;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import test.callgraph.customflow.methodcallargs.dto.base.TestBaseCFMCARequestDto;
import test.callgraph.customflow.methodcallargs.flow.TestCFMCAFlow;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description:
 */
public class CFMCAMethodCallHandler extends BaseHandler {

    private final MethodCallHandler methodCallHandler;
    private final MethodCallInfoHandler methodCallInfoHandler;

    public CFMCAMethodCallHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    public CFMCAMethodCallHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    public void addMethodCall() {
        // 根据被调用类名与方法名查询调用方信息
        List<WriteDbData4MethodCall> allerInfoList = methodCallHandler.queryCallerInfoByCallee(TestCFMCAFlow.class.getName(), "handle");
        if (JavaCGUtil.isCollectionEmpty(allerInfoList)) {
            return;
        }

        for (WriteDbData4MethodCall callerInfo : allerInfoList) {
            if (!MethodCallFlagsEnum.MCFE_METHOD_CALL_INFO.checkFlag(callerInfo.getCallFlags())) {
                // 当前方法调用没有方法调用信息，跳过
                continue;
            }
            // 查询方法调用中被调用对象与参数使用的信息
            ObjArgsInfoInMethodCall objArgsInfoInMethodCall = methodCallInfoHandler.queryObjArgsInfoInMethodCall(callerInfo.getCallId());
            // 获取第1个参数对应的参数信息
            List<MethodCallInfo> methodCallInfoList = objArgsInfoInMethodCall.getArgMethodCallInfo(1);
            if (JavaCGUtil.isCollectionEmpty(methodCallInfoList)) {
                continue;
            }
            // 遍历数组类型参数的各个元素
            for (MethodCallInfo methodCallInfo : methodCallInfoList) {
                String argType = methodCallInfo.getType();
                if (StringUtils.isBlank(argType)) {
                    // 参数类型为空，跳过
                    continue;
                }
                // 参数类型非空，添加方法调用关系
                String calleeFullMethod = JavaCGClassMethodUtil.formatFullMethod(argType, "handle", TestBaseCFMCARequestDto.class);
                methodCallHandler.manualAddMethodCall(callerInfo.getCallerFullMethod(), calleeFullMethod);
            }
        }
    }
}
