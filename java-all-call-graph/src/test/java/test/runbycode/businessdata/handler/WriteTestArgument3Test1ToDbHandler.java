package test.runbycode.businessdata.handler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.methodcall.MethodCallInfo;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.handler.businessdata.AbstractWriteBusinessData2DbHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.methodargument.TestArgument3;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/7
 * @description:
 */
public class WriteTestArgument3Test1ToDbHandler extends AbstractWriteBusinessData2DbHandler {
    private static final Logger logger = LoggerFactory.getLogger(WriteTestArgument3Test1ToDbHandler.class);

    public static final String[] CALLEE_METHOD_INFO_ARRAY = new String[]{
            JACGClassMethodUtil.genClassAndMethodName(TestArgument3.class.getName(), "test1")
    };

    public WriteTestArgument3Test1ToDbHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public WriteTestArgument3Test1ToDbHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected String[] chooseCalleeMethodInfoArray() {
        return CALLEE_METHOD_INFO_ARRAY;
    }

    @Override
    public String chooseBusinessDataType() {
        return TestArgument3.class.getSimpleName() + "@test1";
    }

    @Override
    protected String handleMethodCall(int methodCallId, String calleeClassName, String calleeMethodName, ObjArgsInfoInMethodCall objArgsInfoInMethodCall,
                                      List<String> currentCalleeMethodArgTypeList) {
        if (objArgsInfoInMethodCall == null) {
            return null;
        }

        List<Integer> argSeqList = objArgsInfoInMethodCall.getArgSeqList();
        if (JavaCG2Util.isCollectionEmpty(argSeqList)) {
            return null;
        }

        List<String> typeList = new ArrayList<>();
        for (Integer argSeq : argSeqList) {
            List<MethodCallInfo> methodCallInfoList = objArgsInfoInMethodCall.getArgMethodCallInfo(argSeq);
            if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
                continue;
            }

            for (MethodCallInfo methodCallInfo : methodCallInfoList) {
                String type;
                if (methodCallInfo.getType() != null) {
                    type = methodCallInfo.getType();
                } else {
                    type = methodCallInfo.getValueType();
                }
                logger.info("methodCallId: {} argSeq: {} actual type: {}", methodCallId, argSeq, type);
                typeList.add(type);
            }
        }
        return JACGJsonUtil.getJsonStr(typeList);
    }
}
