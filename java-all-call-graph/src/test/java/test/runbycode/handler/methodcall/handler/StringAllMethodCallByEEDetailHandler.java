package test.runbycode.handler.methodcall.handler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.methodcall.BaseMethodCallByEEDetailHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2024/2/2
 * @description:
 */
public class StringAllMethodCallByEEDetailHandler extends BaseMethodCallByEEDetailHandler {
    private static final Logger logger = LoggerFactory.getLogger(StringAllMethodCallByEEDetailHandler.class);

    public StringAllMethodCallByEEDetailHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public StringAllMethodCallByEEDetailHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected boolean chooseQueryObjArgsInfoInMethodCall() {
        return false;
    }

    @Override
    protected void handleMethodWithArgs(WriteDbData4MethodCall methodCall, MethodDetail callerMethodDetail, MethodDetail calleeMethodDetail,
                                        ObjArgsInfoInMethodCall objArgsInfoInMethodCall, Object... args) {
        logger.info("### {} {} {}", methodCall.getCallerFullMethod(), methodCall.getCalleeFullMethod(), methodCall.getCallerLineNumber());
    }
}
