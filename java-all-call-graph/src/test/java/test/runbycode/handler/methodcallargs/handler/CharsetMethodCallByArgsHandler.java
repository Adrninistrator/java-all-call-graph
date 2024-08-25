package test.runbycode.handler.methodcallargs.handler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.handler.methodcallargs.BaseMethodCallByArgsHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg.common.enums.JavaCGMethodCallInfoTypeEnum;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * @author adrninistrator
 * @date 2023/6/28
 * @description:
 */
public class CharsetMethodCallByArgsHandler extends BaseMethodCallByArgsHandler {
    private static final Logger logger = LoggerFactory.getLogger(CharsetMethodCallByArgsHandler.class);

    public static final String CLASS_NAME_STANDARD_CHARSETS = StandardCharsets.class.getName();
    public static final String CLASS_NAME_CHARSET = Charset.class.getName();

    public static final String METHOD_NAME_FOR_NAME = "forName";

    public static final String[] STANDARD_CHARSETS_ALL = new String[]{
            JavaCGClassMethodUtil.genClassAndField(CLASS_NAME_STANDARD_CHARSETS, "US_ASCII"),
            JavaCGClassMethodUtil.genClassAndField(CLASS_NAME_STANDARD_CHARSETS, "ISO_8859_1"),
            JavaCGClassMethodUtil.genClassAndField(CLASS_NAME_STANDARD_CHARSETS, "UTF_8"),
            JavaCGClassMethodUtil.genClassAndField(CLASS_NAME_STANDARD_CHARSETS, "UTF_16BE"),
            JavaCGClassMethodUtil.genClassAndField(CLASS_NAME_STANDARD_CHARSETS, "UTF_16LE"),
            JavaCGClassMethodUtil.genClassAndField(CLASS_NAME_STANDARD_CHARSETS, "UTF_16")
    };

    public CharsetMethodCallByArgsHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public CharsetMethodCallByArgsHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    @Override
    protected JavaCGMethodCallInfoTypeEnum[] chooseMethodCallInfoTypes() {
        return new JavaCGMethodCallInfoTypeEnum[]{JavaCGMethodCallInfoTypeEnum.MCIT_VALUE, JavaCGMethodCallInfoTypeEnum.MCIT_STATIC_FIELD};
    }

    @Override
    protected boolean needHandleMethodCallInfo(WriteDbData4MethodCallInfo methodCallInfo) {
        if (JavaCGMethodCallInfoTypeEnum.MCIT_VALUE.getType().equals(methodCallInfo.getType())) {
            return StringUtils.equalsAnyIgnoreCase(methodCallInfo.getTheValue(),
                    "US-ASCII", "ISO-8859-1", "UTF-8", "UTF-16BE", "UTF-16LE", "UTF-16", "ASCII", "BIG5", "UTF-32", "GBK", "GB2312");
        }
        return StringUtils.equalsAnyIgnoreCase(methodCallInfo.getTheValue(), STANDARD_CHARSETS_ALL);
    }

    @Override
    protected void handleMethodCallWithInfo(WriteDbData4MethodCall methodCall, MethodDetail callerMethodDetail, MethodDetail calleeMethodDetail,
                                            WriteDbData4MethodCallInfo methodCallInfo) {
        logger.info("### {} {} {} {}", methodCall.getCallerFullMethod(), methodCall.getCalleeFullMethod(), methodCallInfo.getType(), methodCallInfo.getTheValue());
        String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(methodCall.getCalleeFullMethod());
        if (CLASS_NAME_CHARSET.equals(calleeClassName) && METHOD_NAME_FOR_NAME.equals(methodCall.getCalleeMethodName())) {
            // 被调用方法为Charset.forName()，查询下一个被调用的方法
            WriteDbData4MethodCall nextMethodCall = methodCallHandler.queryMethodCallByCallId(methodCall.getCallId() + 1);
            if (nextMethodCall != null) {
                logger.info("### next {}", nextMethodCall.getCalleeFullMethod());
            }
        }
    }
}
