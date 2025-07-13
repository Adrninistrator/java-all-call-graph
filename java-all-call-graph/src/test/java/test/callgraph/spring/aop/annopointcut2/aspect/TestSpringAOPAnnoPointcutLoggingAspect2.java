package test.callgraph.spring.aop.annopointcut2.aspect;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class TestSpringAOPAnnoPointcutLoggingAspect2 {
    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPAnnoPointcutLoggingAspect2.class);

    @Pointcut("within(test.callgraph.spring.aop.annopointcut1.service..*)")
    public void classCondition1() {
    }

    @Pointcut("within(test.callgraph.spring.aop.annopointcut2.service..*)")
    public void classCondition() {
    }

    @Pointcut("execution(* getUserById(..)) || execution(* updateUser(..)) || execution(* deleteUser(..))")
    public void methodCondition() {
    }

    @Pointcut("classCondition() && methodCondition()")
    public void classMethodCondition() {
    }

    // 前置通知
    @Before("classMethodCondition()")
    public void logBefore(JoinPoint joinPoint) {
        logger.info("Before method: {}", joinPoint.getSignature().getName());
    }

    // 后置通知
    @After("classMethodCondition()")
    public void logAfter(JoinPoint joinPoint) {
        logger.info("After method: {}", joinPoint.getSignature().getName());
    }

    // 返回后通知
    @AfterReturning(pointcut = "classMethodCondition()", returning = "result")
    public void logAfterReturning(JoinPoint joinPoint, Object result) {
        logger.info("AfterReturning method: {}, result: {}", joinPoint.getSignature().getName(), result);
    }

    // 异常通知
    @AfterThrowing(pointcut = "classMethodCondition()", throwing = "error")
    public void logAfterThrowing(JoinPoint joinPoint, Throwable error) {
        logger.error("AfterThrowing in method: {} , exception: {}", joinPoint.getSignature().getName(), error);
    }
}