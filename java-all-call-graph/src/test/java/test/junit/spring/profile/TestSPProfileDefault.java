package test.junit.spring.profile;

import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import test.callgraph.spring.profile.bean.DefaultMessageService1;
import test.callgraph.spring.profile.bean.DevMessageService1;
import test.callgraph.spring.profile.bean.DevMessageService2A;
import test.callgraph.spring.profile.bean.DevMessageService2B;
import test.callgraph.spring.profile.bean.MessageService1;
import test.callgraph.spring.profile.bean.MessageService2;
import test.callgraph.spring.profile.bean.ProdMessageService1;

/**
 * @author adrninistrator
 * @date 2025/10/18
 * @description:
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TestSPProfileDefault {

    private static final Logger logger = LoggerFactory.getLogger(TestSPProfileDefault.class);

    @Test
    public void test1Default() {
        doTest1("", DefaultMessageService1.class.getName());
    }

    @Test
    public void test2Dev() {
        doTest1("dev", DevMessageService1.class.getName());
    }

    @Test
    public void test3Prod() {
        doTest1("prod", ProdMessageService1.class.getName());
    }

    @Test
    public void test4Dev2a() {
        doTest2("dev2a", DefaultMessageService1.class.getName(), null);
    }

    @Test
    public void test4Dev2b() {
        doTest2("dev2b", DefaultMessageService1.class.getName(), null);
    }

    @Test
    public void test4DevDev2a() {
        doTest2("dev,dev2A", DevMessageService1.class.getName(), DevMessageService2A.class.getName());
    }

    @Test
    public void test4DevDev2b() {
        doTest2("dev,dev2B", DevMessageService1.class.getName(), DevMessageService2B.class.getName());
    }

    private void doTest1(String profile, String expectedService1ClassName) {
        System.setProperty("spring.profiles.active", profile);

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("applicationContext.xml");

        MessageService1 service1 = context.getBean(MessageService1.class);
        String message = service1.getMessage();
        System.out.println(service1.getClass().getName() + " " + message);
        Assert.assertEquals(expectedService1ClassName, service1.getClass().getName());

        Assert.assertThrows(NoSuchBeanDefinitionException.class, () -> context.getBean(MessageService2.class));

        context.close();
    }

    private void doTest2(String profile, String expectedService1ClassName, String expectedService2ClassName) {
        System.setProperty("spring.profiles.active", profile);

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("applicationContext.xml");

        MessageService1 service1 = null;
        try {
            service1 = context.getBean(MessageService1.class);
        } catch (NoSuchBeanDefinitionException e) {
            logger.error("error ", e);
        }
        if (service1 == null) {
            Assert.assertNull(expectedService1ClassName);
        } else {
            String message = service1.getMessage();
            System.out.println(service1.getClass().getName() + " " + message);
            Assert.assertEquals(expectedService1ClassName, service1.getClass().getName());
        }

        MessageService2 service2 = null;
        try {
            service2 = context.getBean(MessageService2.class);
        } catch (NoSuchBeanDefinitionException e) {
            logger.error("error ", e);
        }
        if (service2 == null) {
            Assert.assertNull(expectedService2ClassName);
        } else {
            String message = service2.getMessage();
            System.out.println(service2.getClass().getName() + " " + message);
            Assert.assertEquals(expectedService2ClassName, service2.getClass().getName());
        }
        context.close();
    }
}
