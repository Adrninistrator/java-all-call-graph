package test.run_by_code.custom_flow.method_call_args.handler;

import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method_call.MethodCallInfo;
import com.adrninistrator.jacg.dto.method_call.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.method.MethodCallHandler;
import com.adrninistrator.jacg.handler.method.MethodCallInfoHandler;
import com.adrninistrator.javacg.util.JavaCGMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import test.call_graph.custom_flow.method_call_args.dto.base.TestBaseCFMCARequestDto;
import test.call_graph.custom_flow.method_call_args.flow.TestCFMCAFlow;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description:
 */
public class CFMCAMethodCallHandler extends BaseHandler {
    private final MethodCallInfoHandler methodCallInfoHandler;

    private final MethodCallHandler methodCallHandler;

    public CFMCAMethodCallHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
    }

    public CFMCAMethodCallHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
    }

    public void addMethodCall() {
        // 根据被调用类名与方法名查询调用方信息
        List<WriteDbData4MethodCall> allerInfoList = dbOperWrapper.getCallerInfoByCallee(TestCFMCAFlow.class.getName(), "handle");
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
                String calleeFullMethod = JavaCGMethodUtil.formatFullMethod(argType, "handle", TestBaseCFMCARequestDto.class);
                methodCallHandler.manualAddMethodCall(callerInfo.getCallerFullMethod(), calleeFullMethod);
            }
        }
    }
}
