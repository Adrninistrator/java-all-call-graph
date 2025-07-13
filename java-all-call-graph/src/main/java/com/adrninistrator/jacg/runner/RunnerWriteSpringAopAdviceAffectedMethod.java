package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.spring.SpringAopAdviceAndPointcut;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAdvice;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAdviceAffectedMethod;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringAopAdviceAffectedMethod;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.accessflag.JavaCG2AccessFlags;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.weaver.internal.tools.PointcutExpressionImpl;
import org.aspectj.weaver.patterns.Pointcut;
import org.aspectj.weaver.tools.PointcutExpression;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.aspectj.AspectJExpressionPointcut;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/7/6
 * @description:
 */
public class RunnerWriteSpringAopAdviceAffectedMethod extends RunnerWriteDb {

    private static final Logger logger = LoggerFactory.getLogger(RunnerWriteSpringAopAdviceAffectedMethod.class);

    private String inputRootPath;

    public static void main(String[] args) {
        RunnerWriteSpringAopAdviceAffectedMethod runnerWriteSpringAopAdviceAffectedMethod = new RunnerWriteSpringAopAdviceAffectedMethod();
        if (runnerWriteSpringAopAdviceAffectedMethod.run()) {
            System.exit(0);
            return;
        }
        System.exit(1);
    }

    @Override
    protected boolean preHandle() {
        inputRootPath = JavaCG2Util.getDirPathInJvmOptions(JavaCG2Constants.JVM_PROP_KEY_INPUT_ROOT_PATH);
        if (StringUtils.isBlank(inputRootPath)) {
            logger.error("未通过JVM参数 {} 指定当前使用的输入目录", JavaCG2Constants.JVM_PROP_KEY_INPUT_ROOT_PATH);
            return false;
        }
        logger.info("当前使用的输入目录 {}", inputRootPath);
        return true;
    }

    @Override
    protected void handle() {
        // 创建线程池，参数固定指定为1，即使用1个线程
        createThreadPoolExecutor(1);

        // 处理Spring AOP advice影响的方法
        if (!handleSpringAopAdviceAffectedMethod()) {
            recordTaskFail();
        }

        // 等待线程池执行完毕
        wait4TPEDone();

        // 检查执行结果
        if (!checkResult()) {
            recordTaskFail();
        }

        // 打印使用的配置参数
        currentOutputDirPath = inputRootPath;
        printAllConfigInfo();
        printUsedConfigInfo();
    }

    @Override
    protected boolean checkH2DbFile() {
        // 检查H2数据库文件是否可写，不允许文件不存在
        return checkH2DbFileWritable(false);
    }

    // 处理Spring AOP advice影响的方法
    private boolean handleSpringAopAdviceAffectedMethod() {
        SpringHandler springHandler = new SpringHandler(dbOperWrapper);
        MethodInfoHandler methodHandler = new MethodInfoHandler(dbOperWrapper);
        // 查询所有的Spring Bean类名
        List<String> springBeanClassNameList = springHandler.queryDistinctBeanClassName();
        if (springBeanClassNameList.isEmpty()) {
            logger.info("未查询到Spring Bean类名");
            return true;
        }

        // 查询所有的Spring AOP advice
        List<WriteDbData4SpringAopAdvice> springAopAdviceList = springHandler.querySpringAopAdvice();
        if (springAopAdviceList.isEmpty()) {
            logger.info("未查询到Spring AOP advice");
            return true;
        }

        WriteDbHandler4SpringAopAdviceAffectedMethod writeDbHandler4SpringAopAdviceAffectedMethod = new WriteDbHandler4SpringAopAdviceAffectedMethod(writeDbResult);
        try {
            initWriteDbHandler(writeDbHandler4SpringAopAdviceAffectedMethod);
            writeDbHandler4SpringAopAdviceAffectedMethod.beforeHandle(inputRootPath);

            List<SpringAopAdviceAndPointcut> springAopAdviceAndPointcutList = new ArrayList<>(springAopAdviceList.size());
            for (WriteDbData4SpringAopAdvice springAopAdvice : springAopAdviceList) {
                AspectJExpressionPointcut aspectJExpressionPointcut = new AspectJExpressionPointcut();
                aspectJExpressionPointcut.setExpression(springAopAdvice.getExpression());
                Class<?> aspectClass = Class.forName(springAopAdvice.getAspectClassName(), false, this.getClass().getClassLoader());
                aspectJExpressionPointcut.setPointcutDeclarationScope(aspectClass);

                SpringAopAdviceAndPointcut springAopAdviceAndPointcut = new SpringAopAdviceAndPointcut();
                springAopAdviceAndPointcut.setSpringAopAdvice(springAopAdvice);
                springAopAdviceAndPointcut.setAspectJExpressionPointcut(aspectJExpressionPointcut);
                springAopAdviceAndPointcutList.add(springAopAdviceAndPointcut);
            }

            // 获取需要忽略的Spring Bean类名前缀
            Set<String> ignoreSpringBeanClassPrefixSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_PARSE_SPRING_AOP_IGNORE_CLASS_PREFIX);

            // 遍历Spring Bean类名
            for (String springBeanClassName : springBeanClassNameList) {
                // 根据Spring Bean类名前缀判断是否需要忽略
                if (checkIgnoreBySpringBeanClassName(ignoreSpringBeanClassPrefixSet, springBeanClassName)) {
                    continue;
                }

                /*
                    需要检查的方法Map
                    key 方法HASH
                    value 方法信息
                 */
                Map<String, WriteDbData4MethodInfo> checkMethodInfoMap = new HashMap<>();
                // 查询当前类的所有方法
                List<WriteDbData4MethodInfo> methodInfoList = methodHandler.queryMethodInfoByClass(springBeanClassName);
                for (WriteDbData4MethodInfo methodInfo : methodInfoList) {
                    JavaCG2AccessFlags accessFlags = new JavaCG2AccessFlags(methodInfo.getAccessFlags());
                    if (accessFlags.isPublic() && !accessFlags.isStatic() && !accessFlags.isFinal() && !accessFlags.isAbstract() && !JavaCG2CommonNameConstants.METHOD_NAME_INIT.equals(methodInfo.getMethodName())) {
                        // 仅处理public、非构造函数、非static、非final、非abstract方法
                        checkMethodInfoMap.put(methodInfo.getMethodHash(), methodInfo);
                    }
                }
                if (checkMethodInfoMap.isEmpty()) {
                    continue;
                }

                logger.info("尝试加载Spring Bean类 {}", springBeanClassName);
                Class<?> springBeanClass = Class.forName(springBeanClassName, false, this.getClass().getClassLoader());

                for (Method springBeanMethod : springBeanClass.getMethods()) {
                    String fullMethod = JACGClassMethodUtil.genJavaFullMethod(springBeanClassName, springBeanMethod);
                    String methodReturnType = springBeanMethod.getGenericReturnType().getTypeName();
                    logger.debug("当前处理的方法 {} {} {}", springBeanClassName, fullMethod, methodReturnType);
                    String methodHash = JACGClassMethodUtil.genMethodHashWithLen(fullMethod, methodReturnType);
                    WriteDbData4MethodInfo methodInfo = checkMethodInfoMap.get(methodHash);
                    if (methodInfo == null) {
                        // 不需要处理的方法跳过
                        continue;
                    }

                    // 记录每个Spring Aop advice匹配的方法
                    recordSpringAopAdviceMatches(springAopAdviceAndPointcutList, springBeanClass, springBeanMethod, methodInfo, writeDbHandler4SpringAopAdviceAffectedMethod);
                }
            }

            return true;
        } catch (NoClassDefFoundError e) {
            logger.error("未找到指定的类，请检查是否有将对应的jar文件添加到需要解析的jar文件列表中 ", e);
            return false;
        } catch (Exception e) {
            logger.error("处理Spring AOP advice影响的方法异常 ", e);
            return false;
        } finally {
            writeDbHandler4SpringAopAdviceAffectedMethod.afterHandle();
        }
    }

    // 根据Spring Bean类名前缀判断是否需要忽略
    private boolean checkIgnoreBySpringBeanClassName(Set<String> ignoreSpringBeanClassPrefixSet, String springBeanClassName) {
        for (String ignoreSpringBeanClassPrefix : ignoreSpringBeanClassPrefixSet) {
            if (springBeanClassName.startsWith(ignoreSpringBeanClassPrefix)) {
                logger.info("忽略当前Spring Bean类名 {} {}", springBeanClassName, ignoreSpringBeanClassPrefix);
                return true;
            }
        }
        return false;
    }

    // 记录每个Spring Aop advice匹配的方法
    private void recordSpringAopAdviceMatches(List<SpringAopAdviceAndPointcut> springAopAdviceAndPointcutList, Class<?> springBeanClass, Method springBeanMethod,
                                              WriteDbData4MethodInfo methodInfo, WriteDbHandler4SpringAopAdviceAffectedMethod writeDbHandler4SpringAopAdviceAffectedMethod) {
        for (SpringAopAdviceAndPointcut springAopAdviceAndPointcut : springAopAdviceAndPointcutList) {
            AspectJExpressionPointcut aspectJExpressionPointcut = springAopAdviceAndPointcut.getAspectJExpressionPointcut();
            if (!aspectJExpressionPointcut.matches(springBeanMethod, springBeanClass)) {
                // advice不匹配的方法跳过
                continue;
            }
            String underlyingExpression = "";
            PointcutExpression pointcutExpression = aspectJExpressionPointcut.getPointcutExpression();
            if (pointcutExpression instanceof PointcutExpressionImpl) {
                PointcutExpressionImpl pointcutExpressionImpl = (PointcutExpressionImpl) pointcutExpression;
                Pointcut pointcut = pointcutExpressionImpl.getUnderlyingPointcut();
                underlyingExpression = pointcut.toString();
            }

            WriteDbData4SpringAopAdvice springAopAdvice = springAopAdviceAndPointcut.getSpringAopAdvice();
            logger.info("找到Spring Aop advice方法 {} 表达式 [{}] 底层表达式 [{}] 匹配的方法 {}", springAopAdvice.getAdviceFullMethod(), springAopAdvice.getExpression(), underlyingExpression,
                    methodInfo.getFullMethod());
            WriteDbData4SpringAopAdviceAffectedMethod writeDbData4SpringAopAdviceAffectedMethod = new WriteDbData4SpringAopAdviceAffectedMethod();
            writeDbData4SpringAopAdviceAffectedMethod.setRecordId(writeDbHandler4SpringAopAdviceAffectedMethod.genNextRecordId());
            writeDbData4SpringAopAdviceAffectedMethod.setType(springAopAdvice.getType());
            writeDbData4SpringAopAdviceAffectedMethod.setXmlAspectId(springAopAdvice.getXmlAspectId());
            writeDbData4SpringAopAdviceAffectedMethod.setXmlAspectMethodName(springAopAdvice.getXmlAspectMethodName());
            writeDbData4SpringAopAdviceAffectedMethod.setAdviceType(springAopAdvice.getAdviceType());
            writeDbData4SpringAopAdviceAffectedMethod.setXmlPointcutRef(springAopAdvice.getXmlPointcutRef());
            writeDbData4SpringAopAdviceAffectedMethod.setExpression(springAopAdvice.getExpression());
            writeDbData4SpringAopAdviceAffectedMethod.setAspectOrder(springAopAdvice.getAspectOrder());
            writeDbData4SpringAopAdviceAffectedMethod.setAdviceFullMethod(springAopAdvice.getAdviceFullMethod());
            writeDbData4SpringAopAdviceAffectedMethod.setAdviceMethodReturnType(springAopAdvice.getAdviceMethodReturnType());
            writeDbData4SpringAopAdviceAffectedMethod.setAdviceMethodHash(springAopAdvice.getAdviceMethodHash());
            writeDbData4SpringAopAdviceAffectedMethod.setAspectClassName(springAopAdvice.getAspectClassName());
            writeDbData4SpringAopAdviceAffectedMethod.setDefineXmlPath(springAopAdvice.getDefineXmlPath());
            writeDbData4SpringAopAdviceAffectedMethod.setUnderlyingExpression(underlyingExpression);
            writeDbData4SpringAopAdviceAffectedMethod.setAffectedFullMethod(methodInfo.getFullMethod());
            writeDbData4SpringAopAdviceAffectedMethod.setAffectedMethodReturnType(methodInfo.getReturnType());
            writeDbData4SpringAopAdviceAffectedMethod.setAffectedMethodHash(methodInfo.getMethodHash());

            writeDbHandler4SpringAopAdviceAffectedMethod.addData(writeDbData4SpringAopAdviceAffectedMethod);
            writeDbHandler4SpringAopAdviceAffectedMethod.tryInsertDb();
        }
    }
}
