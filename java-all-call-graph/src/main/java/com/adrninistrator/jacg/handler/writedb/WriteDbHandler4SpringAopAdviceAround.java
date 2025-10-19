package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.WriteDbHandlerWriteFileEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAdvice;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAdviceAround;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/7/2
 * @description: 写入数据库，Spring AOP advice Around信息
 */
@JACGWriteDbHandler(
        readFile = false,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_AOP_ADVICE_AROUND,
        writeFileEnum = WriteDbHandlerWriteFileEnum.WDHWFE_SPRING_AOP_ADVICE_AROUND,
        dependsWriteDbTableEnums = {DbTableInfoEnum.DTIE_SPRING_AOP_ADVICE,
                DbTableInfoEnum.DTIE_METHOD_CALL,
                DbTableInfoEnum.DTIE_METHOD_CALL_INFO,
                // 当前类的后续处理Spring AOP advice影响的方法时需要使用以下表
                DbTableInfoEnum.DTIE_SPRING_BEAN,
                DbTableInfoEnum.DTIE_METHOD_INFO,
        }
)
public class WriteDbHandler4SpringAopAdviceAround extends AbstractWriteDbHandler<WriteDbData4SpringAopAdviceAround> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4SpringAopAdviceAround.class);

    public WriteDbHandler4SpringAopAdviceAround(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    private SpringHandler springHandler;
    private MethodCallHandler methodCallHandler;
    private MethodCallInfoHandler methodCallInfoHandler;

    public boolean handle(String javaCG2OutputPath) {
        try {
            beforeHandle(javaCG2OutputPath);
            // 查询Spring AOP advice Around信息
            List<WriteDbData4SpringAopAdvice> springAopAdviceList = springHandler.querySpringAopAroundInAdvice();
            if (JavaCG2Util.isCollectionEmpty(springAopAdviceList)) {
                return true;
            }

            for (WriteDbData4SpringAopAdvice springAopAdvice : springAopAdviceList) {
                handleSpringAopAdvice(springAopAdvice);
            }
            return true;
        } catch (Exception e) {
            logger.error("处理Spring AOP advice Around信息出现异常 ", e);
            return false;
        } finally {
            afterHandle();
        }
    }

    private boolean handleSpringAopAdvice(WriteDbData4SpringAopAdvice springAopAdvice) {
        // 对于每个Spring AOP advice Around，查询advice方法调用ProceedingJoinPoint.proceed方法的方法调用
        List<WriteDbData4MethodCall> methodCallList = methodCallHandler.queryByErHashEeClassesMethod(springAopAdvice.getAdviceMethodHash(),
                JACGCommonNameConstants.METHOD_NAME_PROCEED, JACGCommonNameConstants.SPRING_AOP_PROCEEDING_JOIN_POINT_NAMES);
        if (JavaCG2Util.isCollectionEmpty(methodCallList)) {
            logger.error("Around类型的Spring AOP advice方法未查询到调用ProceedingJoinPoint.proceed方法 {}", springAopAdvice.getAdviceFullMethod());
            return false;
        }

        List<Integer> proceedCallIdList = new ArrayList<>();
        for (WriteDbData4MethodCall methodCall : methodCallList) {
            // 处理查询到的每个方法调用，查询方法调用信息
            List<String> argSeqStrList = methodCallInfoHandler.queryMethodCallInfoByCallIdType(methodCall.getCallId(), 0,
                    JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ.getType());
            for (String argSeqStr : argSeqStrList) {
                int methodArgSeq = Integer.parseInt(argSeqStr);
                if (methodArgSeq > 0) {
                    logger.info("找到Around类型的Spring AOP advice方法调用ProceedingJoinPoint.proceed方法 {}", methodCall.genPrintInfo());
                    proceedCallIdList.add(methodCall.getCallId());
                    break;
                }
            }
        }
        if (proceedCallIdList.isEmpty()) {
            logger.error("Around类型的Spring AOP advice方法未查询到调用ProceedingJoinPoint参数的proceed方法 {}", springAopAdvice.getAdviceFullMethod());
            return false;
        }
        if (proceedCallIdList.size() > 1) {
            logger.error("Around类型的Spring AOP advice方法查询到调用ProceedingJoinPoint参数的proceed方法多次，只支持处理调用一次的写法 {}", springAopAdvice.getAdviceFullMethod());
            return false;
        }

        WriteDbData4SpringAopAdviceAround springAopAdviceAround = new WriteDbData4SpringAopAdviceAround();
        springAopAdviceAround.setRecordId(genNextRecordId());
        springAopAdviceAround.setAdviceFullMethod(springAopAdvice.getAdviceFullMethod());
        springAopAdviceAround.setAdviceMethodReturnType(springAopAdvice.getAdviceMethodReturnType());
        springAopAdviceAround.setAdviceMethodHash(springAopAdvice.getAdviceMethodHash());
        springAopAdviceAround.setProceedCallId(proceedCallIdList.get(0));
        dataList.add(springAopAdviceAround);
        return true;
    }

    @Override
    public void init(WriteDbResult writeDbResult) {
        super.init(writeDbResult);
        springHandler = new SpringHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "记录id，从1开始",
                "advice的完整方法",
                "advice方法的返回类型",
                "advice方法hash+字节数",
                "调用ProceedingJoinPoint.proceed()方法调用序号，从1开始"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring AOP advice Around信息";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{"包括advice Around方法，及调用ProceedingJoinPoint.proceed()方法调用序号"};
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringAopAdviceAround data) {
        return new Object[]{
                data.getRecordId(),
                data.getAdviceFullMethod(),
                data.getAdviceMethodReturnType(),
                data.getAdviceMethodHash(),
                data.getProceedCallId()
        };
    }
}
