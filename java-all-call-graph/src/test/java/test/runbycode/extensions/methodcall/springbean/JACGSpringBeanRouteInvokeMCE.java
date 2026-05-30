package test.runbycode.extensions.methodcall.springbean;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.extensions.methodcall.JACGObjInstanceMethodNameMCE;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/5/25
 * @description: JACG 方法调用处理扩展类 - Spring Bean 方法调用跳转
 * 当找到 RouteClazz.invoke() 方法调用时，获得对应被调用对象的构造函数中的参数1的值（Spring Bean名称），
 * 通过该值获得对应的Spring Bean对应的类名进行替换，得到调用对应类的.execute()方法的调用关系
 */
public class JACGSpringBeanRouteInvokeMCE extends JACGObjInstanceMethodNameMCE {

    private static final Logger logger = LoggerFactory.getLogger(JACGSpringBeanRouteInvokeMCE.class);

    private final MethodCallInfoHandler methodCallInfoHandler;
    private final MethodInfoHandler methodInfoHandler;
    private final SpringHandler springHandler;

    public JACGSpringBeanRouteInvokeMCE(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        springHandler = new SpringHandler(dbOperWrapper);
    }

    @Override
    public String getCallType() {
        return JavaCG2SpringBeanRouteInvokeMCE.CALL_TYPE;
    }

    @Override
    public boolean handle(WriteDbData4MethodCall methodCall) {
        // 获取对应的原始方法调用ID
        int rawMethodCallId = Integer.parseInt(methodCall.getDescription());

        // 1. 从 RouteClazz.invoke() 调用的被调用对象追溯构造函数调用
        WriteDbData4MethodCallInfo constructorCallInfo = methodCallInfoHandler.queryMethodCallInfoByCallIdSeqType(
                rawMethodCallId, 0, 0, JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID.getType());

        if (constructorCallInfo == null) {
            logger.warn("未找到RouteClazz.invoke()被调用对象的构造函数调用 callId: {}", rawMethodCallId);
            methodCall.setCallType(JavaCG2CallTypeEnum.CTE_MANUAL_ADDED.getType());
            methodCall.setDescription("Spring Bean方法调用（未找到构造函数） " + rawMethodCallId);
            return true;
        }

        int constructorCallId;
        try {
            constructorCallId = Integer.parseInt(constructorCallInfo.getTheValue());
        } catch (NumberFormatException e) {
            logger.error("解析构造函数调用callId失败 value: {}", constructorCallInfo.getTheValue());
            methodCall.setCallType(JavaCG2CallTypeEnum.CTE_MANUAL_ADDED.getType());
            methodCall.setDescription("Spring Bean方法调用（callId解析失败） " + rawMethodCallId);
            return true;
        }

        // 2. 从构造函数调用获取参数1的值（Spring Bean名称）
        List<String> beanNameList = methodCallInfoHandler.queryMethodCallObjArgValues(constructorCallId, 1);
        if (JavaCG2Util.isCollectionEmpty(beanNameList)) {
            logger.warn("未找到构造函数参数1的值 callId: {}", constructorCallId);
            methodCall.setCallType(JavaCG2CallTypeEnum.CTE_MANUAL_ADDED.getType());
            methodCall.setDescription("Spring Bean方法调用（未找到Bean名称） " + rawMethodCallId);
            return true;
        }

        String beanName = beanNameList.get(0);

        // 3. 通过Spring Bean名称查询对应的类名
        String className = springHandler.queryClassNameBySpringBeanName(beanName);
        if (className == null) {
            logger.warn("未找到Spring Bean名称对应的类名 beanName: {}", beanName);
            methodCall.setCallType(JavaCG2CallTypeEnum.CTE_MANUAL_ADDED.getType());
            methodCall.setDescription("Spring Bean方法调用（未找到类名） " + rawMethodCallId);
            return true;
        }

        // 4. 查找execute()方法（在当前类及父类中查找）
        List<WriteDbData4MethodInfo> methodInfoList = methodInfoHandler.queryMethodByClassMethodUpper(className, "execute");
        if (JavaCG2Util.isCollectionEmpty(methodInfoList)) {
            logger.warn("未找到execute()方法 className: {}", className);
            methodCall.setCallType(JavaCG2CallTypeEnum.CTE_MANUAL_ADDED.getType());
            methodCall.setDescription("Spring Bean方法调用（未找到execute方法） " + rawMethodCallId);
            return true;
        }

        WriteDbData4MethodInfo methodInfo = methodInfoList.get(0);

        // 5. 修改被调用方法信息，使用Spring Bean对应的类名
        String calleeSimpleClassName = dbOperWrapper.querySimpleClassName(className);
        String calleeFullMethod = methodInfo.getFullMethod();

        methodCall.setCalleeSimpleClassName(calleeSimpleClassName);
        methodCall.setCalleeMethodName("execute");
        methodCall.setCalleeFullMethod(calleeFullMethod);
        methodCall.setCalleeMethodHash(JACGClassMethodUtil.genMethodHashWithLen(calleeFullMethod, methodInfo.getReturnType()));
        methodCall.setRawReturnType(methodInfo.getReturnType());
        methodCall.setCalleeJarNum(0);

        // 修改方法调用类型及描述
        methodCall.setCallType(JavaCG2CallTypeEnum.CTE_MANUAL_ADDED.getType());
        methodCall.setDescription("Spring Bean方法调用 " + rawMethodCallId + " beanName: " + beanName);
        return true;
    }

    @Override
    protected int chooseCalleeObjArgSeq() {
        // 不使用父类的默认处理逻辑，此处返回0
        return 0;
    }

    @Override
    protected int chooseCalleeMethodNameArgSeq() {
        // 不使用父类的默认处理逻辑，此处返回0
        return 0;
    }

    @Override
    protected String chooseMethodCallType() {
        return JavaCG2SpringBeanRouteInvokeMCE.CALL_TYPE;
    }
}
