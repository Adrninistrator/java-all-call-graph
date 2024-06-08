package test.runbycode.handler.methodcall.handler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.methodcall.MethodCallInfo4Read;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.methodcall.BaseMethodCallByEEDetailHandler;
import com.adrninistrator.jacg.util.JACGMethodCallInfoUtil;
import com.adrninistrator.javacg.common.enums.JavaCGConstantTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/7/3
 * @description:
 */
public class StringAppendCharMethodCallByEEDetailHandler extends BaseMethodCallByEEDetailHandler {
    private static final Logger logger = LoggerFactory.getLogger(StringAppendCharMethodCallByEEDetailHandler.class);

    public StringAppendCharMethodCallByEEDetailHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public StringAppendCharMethodCallByEEDetailHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected boolean chooseHandleMethod(WriteDbData4MethodCall methodCall, MethodDetail callerMethodDetail, MethodDetail calleeMethodDetail) {
        if (StringBuilder.class.getName().equals(calleeMethodDetail.getClassName())
                && "append".equals(calleeMethodDetail.getMethodName())
                && !JavaCGConstantTypeEnum.CONSTTE_CHAR.getType().equals(calleeMethodDetail.getArgTypeStr())) {
            return false;
        }
        return true;
    }

    @Override
    protected void handleMethodWithArgs(WriteDbData4MethodCall methodCall, MethodDetail callerMethodDetail, MethodDetail calleeMethodDetail,
                                        ObjArgsInfoInMethodCall objArgsInfoInMethodCall, Object... args) {
        List<String> argTypeList = Arrays.asList(
                String.class.getName(),
                CharSequence.class.getName(),
                JavaCGConstantTypeEnum.CONSTTE_CHAR.getType(),
                StringBuilder.class.getName()
        );

        // 获取类型匹配的被调用对象及参数的用于人工查看的方法调用中使用的相关信息
        Map<Integer, List<MethodCallInfo4Read>> methodCallInfo4ReadMap = JACGMethodCallInfoUtil.genMethodCallInfo4ReadMapByArgType(calleeMethodDetail, objArgsInfoInMethodCall,
                argTypeList);
        if (!methodCallInfo4ReadMap.isEmpty()) {
            List<Integer> seqList = new ArrayList<>(methodCallInfo4ReadMap.keySet());
            Collections.sort(seqList);
            for (Integer seq : seqList) {
                List<MethodCallInfo4Read> methodCallInfo4ReadList = methodCallInfo4ReadMap.get(seq);
                for (MethodCallInfo4Read methodCallInfo4Read : methodCallInfo4ReadList) {
                    logger.info("###\n{} {} {}\n{} {} {}", methodCall.getCallerFullMethod(), methodCall.getCalleeFullMethod(), methodCall.getCallerLineNumber(),
                            JACGMethodCallInfoUtil.genObjArgDesc(seq), methodCallInfo4Read.getMethodCallInfoTypeEnum().getDesc(), methodCallInfo4Read.getInfo());
                }
            }
        }
    }
}
