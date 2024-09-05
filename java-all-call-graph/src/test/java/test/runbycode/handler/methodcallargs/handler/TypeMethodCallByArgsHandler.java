package test.runbycode.handler.methodcallargs.handler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.handler.methodcallargs.BaseMethodCallByArgsHandler;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2023/6/28
 * @description:
 */
public class TypeMethodCallByArgsHandler extends BaseMethodCallByArgsHandler {
    private static final Logger logger = LoggerFactory.getLogger(TypeMethodCallByArgsHandler.class);

    public TypeMethodCallByArgsHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public TypeMethodCallByArgsHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected JavaCG2MethodCallInfoTypeEnum[] chooseMethodCallInfoTypes() {
        return new JavaCG2MethodCallInfoTypeEnum[]{JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE};
    }

    @Override
    protected boolean needHandleMethodCallInfo(WriteDbData4MethodCallInfo methodCallInfo) {
        return true;
    }

    @Override
    protected void handleMethodCallWithInfo(WriteDbData4MethodCall methodCall, MethodDetail callerMethodDetail, MethodDetail calleeMethodDetail,
                                            WriteDbData4MethodCallInfo methodCallInfo) {
        logger.info("### {} {} {} {} {}", methodCall.getCallerFullMethod(), methodCall.getCalleeFullMethod(), methodCallInfo.getType(), methodCallInfo.getValueType(),
                methodCallInfo.getTheValue());
    }
}
