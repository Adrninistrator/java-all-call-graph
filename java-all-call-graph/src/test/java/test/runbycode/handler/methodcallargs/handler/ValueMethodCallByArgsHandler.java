package test.runbycode.handler.methodcallargs.handler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.handler.methodcallargs.BaseMethodCallByArgsHandler;
import com.adrninistrator.javacg.common.enums.JavaCGMethodCallInfoTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2023/8/11
 * @description:
 */
public class ValueMethodCallByArgsHandler extends BaseMethodCallByArgsHandler {
    private static final Logger logger = LoggerFactory.getLogger(ValueMethodCallByArgsHandler.class);

    public ValueMethodCallByArgsHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public ValueMethodCallByArgsHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected JavaCGMethodCallInfoTypeEnum[] chooseMethodCallInfoTypes() {
        return new JavaCGMethodCallInfoTypeEnum[]{JavaCGMethodCallInfoTypeEnum.MCIT_VALUE};
    }

    @Override
    protected boolean needHandleMethodCallInfo(WriteDbData4MethodCallInfo methodCallInfo) {
        return true;
    }

    @Override
    protected void handleMethodCallWithInfo(WriteDbData4MethodCall methodCall, MethodDetail callerMethodDetail, MethodDetail calleeMethodDetail,
                                            WriteDbData4MethodCallInfo methodCallInfo) {
        logger.info("### {} {} {} {} {} {}", methodCall.getCallerFullMethod(), methodCall.getCalleeFullMethod(), methodCallInfo.getType(), methodCallInfo.getValueType(),
                methodCallInfo.getTheValue(), methodCallInfo.getOrigValue());
    }
}
