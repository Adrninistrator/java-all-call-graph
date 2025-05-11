package test.runbycode.handler.methodcall.handler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.methodcall.MethodCallInfo4Read;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.methodcall.BaseMethodCallByEEDetailHandler;
import com.adrninistrator.jacg.util.JACGMethodCallInfoUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
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
    protected boolean chooseHandleMethod(WriteDbData4MethodCall methodCall, MethodDetailNoReturnType callerMethodDetailNoReturnType,
                                         MethodDetailNoReturnType calleeMethodDetailNoReturnType) {
        if (StringBuilder.class.getName().equals(calleeMethodDetailNoReturnType.getClassName())
                && "append".equals(calleeMethodDetailNoReturnType.getMethodName())
                && !JavaCG2ConstantTypeEnum.CONSTTE_CHAR.getType().equals(calleeMethodDetailNoReturnType.getArgTypeStr())) {
            return false;
        }
        return true;
    }

    @Override
    protected void handleMethodWithArgs(WriteDbData4MethodCall methodCall, MethodDetailNoReturnType callerMethodDetailNoReturnType, MethodDetailNoReturnType calleeMethodDetailNoReturnType,
                                        ObjArgsInfoInMethodCall objArgsInfoInMethodCall, Object... args) {
        List<String> argTypeList = Arrays.asList(
                String.class.getName(),
                CharSequence.class.getName(),
                JavaCG2ConstantTypeEnum.CONSTTE_CHAR.getType(),
                StringBuilder.class.getName()
        );

        // 获取类型匹配的被调用对象及参数的用于人工查看的方法调用中使用的相关信息
        Map<Integer, List<MethodCallInfo4Read>> methodCallInfo4ReadMap = JACGMethodCallInfoUtil.genMethodCallInfo4ReadMapByArgType(calleeMethodDetailNoReturnType, objArgsInfoInMethodCall,
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
