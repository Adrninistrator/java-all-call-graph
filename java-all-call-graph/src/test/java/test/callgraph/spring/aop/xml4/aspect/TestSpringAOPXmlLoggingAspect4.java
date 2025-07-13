package test.callgraph.spring.aop.xml4.aspect;

import org.aspectj.lang.JoinPoint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestSpringAOPXmlLoggingAspect4 {
    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPXmlLoggingAspect4.class);

    // 前置通知
    public void logBefore() {
        throw new RuntimeException();
    }

    // 同名方法
    public void logBefore(JoinPoint joinPoint) {
        logger.info("Before method: {}", joinPoint.getSignature().getName());
    }
}