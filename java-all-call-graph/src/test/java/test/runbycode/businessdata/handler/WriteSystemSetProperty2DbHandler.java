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

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/7
 * @description:
 */
public class WriteSystemSetProperty2DbHandler extends AbstractWriteBusinessData2DbHandler {
    private static final Logger logger = LoggerFactory.getLogger(WriteSystemSetProperty2DbHandler.class);

    public static final String[] CALLEE_METHOD_INFO_ARRAY = new String[]{
            JACGClassMethodUtil.genClassAndMethodName(System.class.getName(), "setProperty")
    };

    public WriteSystemSetProperty2DbHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public WriteSystemSetProperty2DbHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected String[] chooseCalleeMethodInfoArray() {
        return CALLEE_METHOD_INFO_ARRAY;
    }

    @Override
    public String chooseBusinessDataType() {
        return System.class.getSimpleName();
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

        List<String> valueList = new ArrayList<>();
        for (Integer argSeq : argSeqList) {
            List<MethodCallInfo> methodCallInfoList = objArgsInfoInMethodCall.getArgMethodCallInfo(argSeq);
            if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
                continue;
            }

            for (MethodCallInfo methodCallInfo : methodCallInfoList) {
                logger.info("methodCallId: {} argSeq: {} type: {} value: {}", methodCallId, argSeq, currentCalleeMethodArgTypeList.get(argSeq - 1), methodCallInfo.getValue());
                valueList.add(methodCallInfo.getValue());
            }
        }
        return JACGJsonUtil.getJsonStr(valueList);
    }
}
