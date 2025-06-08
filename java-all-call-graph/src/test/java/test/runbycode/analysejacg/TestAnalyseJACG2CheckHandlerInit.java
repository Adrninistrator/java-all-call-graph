package test.runbycode.analysejacg;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/5/29
 * @description: 检查BaseHandler子类构造函数中调用其他BaseHandler子类构造函数时，使用参数类型为DbOperWrapper的构造函数
 */
public class TestAnalyseJACG2CheckHandlerInit {

    private static final Logger logger = LoggerFactory.getLogger(TestAnalyseJACG2CheckHandlerInit.class);

    @Test
    public void test() {
        List<String> illegalMethodCallList = new ArrayList<>();
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper);
             MethodInfoHandler methodInfoHandler = new MethodInfoHandler(configureWrapper);
             MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {

            // 查询BaseHandler子类
            List<String> handlerClassList = jacgExtendsImplHandler.queryChildClassListByFull(BaseHandler.class.getName(), false, true, true, true);
            logger.info("查询到BaseHandler子类 {}", StringUtils.join(handlerClassList, " "));

            for (String handlerClass : handlerClassList) {
                logger.info("开始处理类 {}", handlerClass);
                // 查询BaseHandler子类的构造函数
                List<FullMethodWithReturnType> methodList = methodInfoHandler.queryMethodByClassMethod(handlerClass, JavaCG2CommonNameConstants.METHOD_NAME_INIT);
                if (JavaCG2Util.isCollectionEmpty(methodList)) {
                    continue;
                }
                for (FullMethodWithReturnType method : methodList) {
                    logger.info("开始处理方法 {}", method.genFullMethodWithReturnType());
                    // 查找BaseHandler子类的构造函数中的方法调用
                    List<WriteDbData4MethodCall> methodCallList = methodCallHandler.queryMethodCallByCallerHash(method.genMethodHash());
                    if (JavaCG2Util.isCollectionEmpty(methodCallList)) {
                        continue;
                    }
                    for (WriteDbData4MethodCall methodCall : methodCallList) {
                        String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCallerFullMethod());
                        MethodDetailNoReturnType methodDetailNoReturnType = JACGClassMethodUtil.genMethodDetailNoReturnType(methodCall.getCalleeFullMethod());
                        // 判断是否调用了BaseHandler子类，且不是当前类父类的构造函数
                        if (!JavaCG2CommonNameConstants.METHOD_NAME_INIT.equals(methodCall.getCalleeMethodName()) ||
                                !jacgExtendsImplHandler.checkExtendsOrImplFull(BaseHandler.class.getName(), methodDetailNoReturnType.getClassName()) ||
                                jacgExtendsImplHandler.checkExtendsOrImplFull(methodDetailNoReturnType.getClassName(), callerClassName)
                        ) {
                            continue;
                        }

                        // 检查被调用的BaseHandler子类的构造函数的参数是否为 DbOperWrapper
                        if (methodDetailNoReturnType.getArgTypeList().size() != 1 || !DbOperWrapper.class.getName().equals(methodDetailNoReturnType.getArgTypeList().get(0))) {
                            illegalMethodCallList.add(methodCall.getCallerFullMethod());
                        }
                    }
                }
            }
        }
        if (illegalMethodCallList.isEmpty()) {
            logger.info("检查通过");
            return;
        }
        logger.error("检查不通过 {}", StringUtils.join(illegalMethodCallList, " "));
        Assert.fail("检查不通过");
    }
}
