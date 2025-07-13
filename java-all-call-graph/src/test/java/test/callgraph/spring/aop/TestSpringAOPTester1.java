package test.callgraph.spring.aop;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import test.callgraph.spring.aop.annopointcut1.service.TestSpringAOPAnnoPointcutUserService1;
import test.callgraph.spring.aop.annopointcut2.service.TestSpringAOPAnnoPointcutUserService2;
import test.callgraph.spring.aop.annotation1.service.TestSpringAOPAnnotationUserService1;
import test.callgraph.spring.aop.annotation2.service.TestSpringAOPAnnotationUserService2;
import test.callgraph.spring.aop.customanno1.service.TestSpringAOPCustomAnnoUserService1;
import test.callgraph.spring.aop.customanno2.service.TestSpringAOPCustomAnnoUserService2;
import test.callgraph.spring.aop.multi.service.TestSpringAOPXmlUserServiceMulti1;
import test.callgraph.spring.aop.multi.service.TestSpringAOPXmlUserServiceMulti2;
import test.callgraph.spring.aop.xml1.service.TestSpringAOPXmlUserService1;
import test.callgraph.spring.aop.xml2.service.TestSpringAOPXmlUserService2;
import test.callgraph.spring.aop.xml3.service.TestSpringAOPXmlUserService3;
import test.callgraph.spring.aop.xml4.service.TestSpringAOPXmlUserService4;
import test.junit.base.TestSpringBase;

/**
 * @author adrninistrator
 * @date 2025/6/9
 * @description:
 */
public class TestSpringAOPTester1 extends TestSpringBase {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPTester1.class);

    @Autowired
    private TestSpringAOPAnnoPointcutUserService1 testSpringAOPAnnoPointcutUserService1;

    @Autowired
    private TestSpringAOPAnnoPointcutUserService2 testSpringAOPAnnoPointcutUserService2;

    @Autowired
    private TestSpringAOPAnnotationUserService1 testSpringAOPAnnotationUserService1;

    @Autowired
    private TestSpringAOPAnnotationUserService2 testSpringAOPAnnotationUserService2;

    @Autowired
    private TestSpringAOPXmlUserService1 testSpringAOPXmlUserService1;

    @Autowired
    private TestSpringAOPXmlUserService2 testSpringAOPXmlUserService2;

    @Autowired
    private TestSpringAOPXmlUserService3 testSpringAOPXmlUserService3;

    @Autowired
    private TestSpringAOPXmlUserService4 testSpringAOPXmlUserService4;

    @Autowired
    private TestSpringAOPCustomAnnoUserService1 testSpringAOPCustomAnnoUserService1;

    @Autowired
    private TestSpringAOPCustomAnnoUserService2 testSpringAOPCustomAnnoUserService2;

    @Autowired
    private TestSpringAOPXmlUserServiceMulti1 testSpringAOPXmlUserServiceMulti1;

    @Autowired
    private TestSpringAOPXmlUserServiceMulti2 testSpringAOPXmlUserServiceMulti2;

    @Test
    public void testAnnoPointcut1() {
        // 测试正常调用
        logger.info("getUserById(1)");
        testSpringAOPAnnoPointcutUserService1.getUserById(null);

        logger.info("deleteUser(1)");
        testSpringAOPAnnoPointcutUserService1.deleteUser(1);

        logger.info("deleteUser(-1)");
        testSpringAOPAnnoPointcutUserService1.deleteUser(-1);
    }

    @Test
    public void testAnnoPointcut2() {
        // 测试正常调用
        logger.info("getUserById(1)");
        testSpringAOPAnnoPointcutUserService2.getUserById(1, null);

        try {
            logger.info("deleteUser(1)");
            testSpringAOPAnnoPointcutUserService2.deleteUser(1);
        } catch (Exception e) {
            logger.error("error1 {}", e.getMessage());
        }

        // 测试异常情况
        boolean error = false;
        try {
            logger.info("deleteUser(-1)");
            testSpringAOPAnnoPointcutUserService2.deleteUser(-1);
        } catch (Exception e) {
            error = true;
            logger.error("error2 {}", e.getMessage());
        }
        Assert.assertTrue(error);
    }

    @Test
    public void testAnnotation1() {
        // 测试正常调用
        logger.info("getUserById(1)");
        testSpringAOPAnnotationUserService1.getUserById(1);

        try {
            logger.info("deleteUser(1)");
            testSpringAOPAnnotationUserService1.deleteUser(1);
        } catch (Exception e) {
            logger.error("error1 {}", e.getMessage());
        }

        // 测试异常情况
        boolean error = false;
        try {
            logger.info("deleteUser(-1)");
            testSpringAOPAnnotationUserService1.deleteUser(-1);
        } catch (Exception e) {
            error = true;
            logger.error("error2 {}", e.getMessage());
        }
        Assert.assertTrue(error);
    }

    @Test
    public void testAnnotation2() {
        // 测试正常调用
        logger.info("getUserById(1)");
        testSpringAOPAnnotationUserService2.getUserById(1);

        try {
            logger.info("deleteUser(1)");
            testSpringAOPAnnotationUserService2.deleteUser(1);
        } catch (Exception e) {
            logger.error("error1 {}", e.getMessage());
        }

        // 测试异常情况
        boolean error = false;
        try {
            logger.info("deleteUser(-1)");
            testSpringAOPAnnotationUserService2.deleteUser(-1);
        } catch (Exception e) {
            error = true;
            logger.error("error2 {}", e.getMessage());
        }
        Assert.assertTrue(error);
    }

    @Test
    public void testXml1() {
        // 测试正常调用
        logger.info("getUserById(1)");
        testSpringAOPXmlUserService1.getUserById(1);

        try {
            logger.info("deleteUser(1)");
            testSpringAOPXmlUserService1.deleteUser(1);
        } catch (Exception e) {
            logger.error("error1 {}", e.getMessage());
        }

        // 测试异常情况
        boolean error = false;
        try {
            logger.info("deleteUser(-1)");
            testSpringAOPXmlUserService1.deleteUser(-1);
        } catch (Exception e) {
            error = true;
            logger.error("error2 {}", e.getMessage());
        }
        Assert.assertTrue(error);
    }

    @Test
    public void testXml2() {
        // 测试正常调用
        logger.info("getUserById(1)");
        testSpringAOPXmlUserService2.getUserById(1);

        try {
            logger.info("deleteUser(1)");
            testSpringAOPXmlUserService2.deleteUser(1);
        } catch (Exception e) {
            logger.error("error1 {}", e.getMessage());
        }

        // 测试异常情况
        boolean error = false;
        try {
            logger.info("deleteUser(-1)");
            testSpringAOPXmlUserService2.deleteUser(-1);
        } catch (Exception e) {
            error = true;
            logger.error("error2 {}", e.getMessage());
        }
        Assert.assertTrue(error);
    }

    @Test
    public void testXml3() {
        // 测试正常调用
        logger.info("getUserById(1)");
        testSpringAOPXmlUserService3.getUserById(1);

        try {
            logger.info("deleteUser(1)");
            testSpringAOPXmlUserService3.deleteUser(1);
        } catch (Exception e) {
            logger.error("error1 {}", e.getMessage());
        }

        // 测试异常情况
        boolean error = false;
        try {
            logger.info("deleteUser(-1)");
            testSpringAOPXmlUserService3.deleteUser(-1);
        } catch (Exception e) {
            error = true;
            logger.error("error2 {}", e.getMessage());
        }
        Assert.assertTrue(error);
    }

    @Test
    public void testXml4() {
        logger.info("getUserById(1)");
        RuntimeException e1 = Assert.assertThrows(RuntimeException.class, () -> testSpringAOPXmlUserService4.getUserById(1));
        logger.error("e1 ", e1);

        logger.info("deleteUser(1)");
        RuntimeException e2 = Assert.assertThrows(RuntimeException.class, () -> testSpringAOPXmlUserService4.updateUser("1"));
        logger.error("e2 ", e2);
    }

    @Test
    public void testCustomAnnotation1() {
        // 测试正常调用
        logger.info("getUserById(1)");
        testSpringAOPCustomAnnoUserService1.getUserById(1);

        try {
            logger.info("deleteUser(1)");
            testSpringAOPCustomAnnoUserService1.deleteUser(1);
        } catch (Exception e) {
            logger.error("error1 {}", e.getMessage());
        }

        // 测试异常情况
        boolean error = false;
        try {
            logger.info("deleteUser(-1)");
            testSpringAOPCustomAnnoUserService1.deleteUser(-1);
        } catch (Exception e) {
            error = true;
            logger.error("error2 {}", e.getMessage());
        }
        Assert.assertTrue(error);
    }

    @Test
    public void testCustomAnnotation2() {
        // 测试正常调用
        logger.info("getUserById(1)");
        testSpringAOPCustomAnnoUserService2.getUserById(1);

        try {
            logger.info("deleteUser(1)");
            testSpringAOPCustomAnnoUserService2.deleteUser(1);
        } catch (Exception e) {
            logger.error("error1 {}", e.getMessage());
        }

        // 测试异常情况
        boolean error = false;
        try {
            logger.info("deleteUser(-1)");
            testSpringAOPCustomAnnoUserService2.deleteUser(-1);
        } catch (Exception e) {
            error = true;
            logger.error("error2 {}", e.getMessage());
        }
        Assert.assertTrue(error);
    }

    @Test
    public void testMulti1() {
        // 测试正常调用
        logger.info("getUserById(1)");
        testSpringAOPXmlUserServiceMulti1.getUserById(1);

        try {
            logger.info("deleteUser(1)");
            testSpringAOPXmlUserServiceMulti1.deleteUser(1);
        } catch (Exception e) {
            logger.error("error1 {}", e.getMessage());
        }

        // 测试异常情况
        boolean error = false;
        try {
            logger.info("deleteUser(-1)");
            testSpringAOPXmlUserServiceMulti1.deleteUser(-1);
        } catch (Exception e) {
            error = true;
            logger.error("error2 {}", e.getMessage());
        }
        Assert.assertTrue(error);
    }

    @Test
    public void testMulti2() {
        // 测试正常调用
        logger.info("getUserById(1)");
        testSpringAOPXmlUserServiceMulti2.getUserById(1);

        try {
            logger.info("deleteUser(1)");
            testSpringAOPXmlUserServiceMulti2.deleteUser(1);
        } catch (Exception e) {
            logger.error("error1 {}", e.getMessage());
        }

        // 测试异常情况
        boolean error = false;
        try {
            logger.info("deleteUser(-1)");
            testSpringAOPXmlUserServiceMulti2.deleteUser(-1);
        } catch (Exception e) {
            error = true;
            logger.error("error2 {}", e.getMessage());
        }
        Assert.assertTrue(error);
    }
}
