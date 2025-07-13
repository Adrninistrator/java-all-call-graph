package test.callgraph.spring.aop.multi.aspect;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

@Aspect
@Component
@Order(30)
public class TestSpringAOPCustomAnnoLoggingAspectMulti2Order30 {
    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPCustomAnnoLoggingAspectMulti2Order30.class);

    // 前置通知
    @Before("@annotation(test.callgraph.spring.aop.multi.annotation.TestSpringAOPCustomAnnotationMulti2)")
    public void logBefore(JoinPoint joinPoint) {
        logger.info("Before method: {}", joinPoint.getSignature().getName());
    }

    // 后置通知
    @After("@annotation(test.callgraph.spring.aop.multi.annotation.TestSpringAOPCustomAnnotationMulti2)")
    public void logAfter(JoinPoint joinPoint) {
        logger.info("After method: {}", joinPoint.getSignature().getName());
    }

    // 返回后通知
    @AfterReturning(pointcut = "@annotation(test.callgraph.spring.aop.multi.annotation.TestSpringAOPCustomAnnotationMulti2)",
            returning = "result")
    public void logAfterReturning(JoinPoint joinPoint, Object result) {
        logger.info("AfterReturning method: {}, result: {}", joinPoint.getSignature().getName(), result);
    }

    // 异常通知
    @AfterThrowing(pointcut = "@annotation(test.callgraph.spring.aop.multi.annotation.TestSpringAOPCustomAnnotationMulti2)",
            throwing = "error")
    public void logAfterThrowing(JoinPoint joinPoint, Throwable error) {
        logger.error("AfterThrowing in method: {} , exception: {}", joinPoint.getSignature().getName(), error);
    }
}