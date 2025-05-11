package test.runbycode.handler.methodcall.handler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.methodcall.BaseMethodCallByERDetailHandler;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2023/7/4
 * @description:
 */
public class ShowAllMethodCallByERDetailHandler extends BaseMethodCallByERDetailHandler {
    private static final Logger logger = LoggerFactory.getLogger(ShowAllMethodCallByERDetailHandler.class);

    public ShowAllMethodCallByERDetailHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public ShowAllMethodCallByERDetailHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected void handleMethodWithArgs(WriteDbData4MethodCall methodCall, MethodDetailNoReturnType callerMethodDetailNoReturnType,
                                        MethodDetailNoReturnType calleeMethodDetailNoReturnType, ObjArgsInfoInMethodCall objArgsInfoInMethodCall) {
        logger.info("###\n{} {} {} {}\n{}", methodCall.getCallId(), methodCall.getCallerFullMethod(), methodCall.getCalleeFullMethod(), methodCall.getCallerLineNumber(),
                JACGJsonUtil.getJsonStr(objArgsInfoInMethodCall));
    }
}
