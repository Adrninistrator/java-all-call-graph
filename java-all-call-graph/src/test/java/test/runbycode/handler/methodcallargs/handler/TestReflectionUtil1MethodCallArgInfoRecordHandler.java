package test.runbycode.handler.methodcallargs.handler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.methodcallargs.BaseMethodCallArgInfoRecordHandler;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import test.callgraph.reflection1.util.TestReflectionUtil1;
import test.callgraph.reflection1.util.TestReflectionUtilWrapper1;

import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/10/11
 * @description:
 */
public class TestReflectionUtil1MethodCallArgInfoRecordHandler extends BaseMethodCallArgInfoRecordHandler {

    public static final String[] FILE_HEADERS = new String[]{
            "调用方法",
            "调用类名",
            "调用方法代码行号",
            "被调用方法",
            "参数序号",
            "序号",
            "参数类型"
    };

    public TestReflectionUtil1MethodCallArgInfoRecordHandler(ConfigureWrapper configureWrapper, List<String> calleeClassNameList, String outputTextFilePath, boolean genExcel) {
        super(configureWrapper, calleeClassNameList, outputTextFilePath, genExcel);
    }

    @Override
    protected String[] chooseFileHeaders() {
        return FILE_HEADERS;
    }

    @Override
    protected boolean needHandleMethodCall(WriteDbData4MethodCall methodCall, MethodDetailNoReturnType callerMethodDetailNoReturnType,
                                           MethodDetailNoReturnType calleeMethodDetailNoReturnType) {
        return checkTestReflectionUtil1Method(calleeMethodDetailNoReturnType) || checkTestReflectionUtilWrapper1Method(calleeMethodDetailNoReturnType);
    }

    @Override
    protected void handleMethodCallInfo(WriteDbData4MethodCall methodCall, MethodDetailNoReturnType callerMethodDetailNoReturnType,
                                        MethodDetailNoReturnType calleeMethodDetailNoReturnType, Map<Integer, Map<String, List<String>>> methodCallInfoMap) {
        List<String> arg1TypeList = null;
        Map<String, List<String>> innerMap = methodCallInfoMap.get(1);
        if (innerMap != null) {
            arg1TypeList = innerMap.get(JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE.getType());
        }
        if (JavaCG2Util.isCollectionEmpty(arg1TypeList)) {
            arg1TypeList = Collections.singletonList("未获取到");
        }

        for (int i = 0; i < arg1TypeList.size(); i++) {
            String arg1Type = arg1TypeList.get(i);
            writeData(callerMethodDetailNoReturnType.getFullMethod(),
                    callerMethodDetailNoReturnType.getClassName(),
                    String.valueOf(methodCall.getCallerLineNumber()),
                    calleeMethodDetailNoReturnType.getFullMethod(),
                    "1",
                    String.valueOf(i),
                    arg1Type
            );
        }
    }

    private boolean checkTestReflectionUtil1Method(MethodDetailNoReturnType calleeMethodDetailNoReturnType) {
        return TestReflectionUtil1.class.getName().equals(calleeMethodDetailNoReturnType.getClassName()) &&
                "runByReflection".equals(calleeMethodDetailNoReturnType.getMethodName()) &&
                calleeMethodDetailNoReturnType.getArgTypeList().size() == 3;
    }

    private boolean checkTestReflectionUtilWrapper1Method(MethodDetailNoReturnType calleeMethodDetailNoReturnType) {
        return TestReflectionUtilWrapper1.class.getName().equals(calleeMethodDetailNoReturnType.getClassName()) &&
                "runByReflection".equals(calleeMethodDetailNoReturnType.getMethodName());
    }
}
