package test.runbycode.handler.spring;

import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfo;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.spring.bean.define.impl.SpringServiceImplA1;
import test.callgraph.spring.mvc.TestSpringController1;
import test.callgraph.spring.mvc.TestSpringController2;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description:
 */
public class TestSpringHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testGetAllControllerMethod() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<SpringControllerInfo> springControllerInfoList = springHandler.getAllControllerMethod();
            Assert.assertFalse(JavaCGUtil.isCollectionEmpty(springControllerInfoList));
            printListContent(springControllerInfoList);
        }
    }

    @Test
    public void testGetAllTaskMethod() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<String> springTaskMethodList = springHandler.getAllTaskMethod();
            Assert.assertFalse(JavaCGUtil.isCollectionEmpty(springTaskMethodList));
            printListContent(springTaskMethodList);
        }
    }

    @Test
    public void testGetControllerUriList() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            doGetControllerUriList(springHandler, JavaCGClassMethodUtil.formatFullMethodStr(TestSpringController1.class.getName(), "<init>"), false);
            doGetControllerUriList(springHandler, JavaCGClassMethodUtil.formatFullMethodStr(TestSpringController1.class.getName(), "test1"), true);
            doGetControllerUriList(springHandler, JavaCGClassMethodUtil.formatFullMethodStr(TestSpringController1.class.getName(), "test2"), true);
            doGetControllerUriList(springHandler, JavaCGClassMethodUtil.formatFullMethodStr(TestSpringController2.class.getName(), "test1"), true);
        }
    }

    private void doGetControllerUriList(SpringHandler springHandler, String fullMethod, boolean exists) {
        List<String> controllerUriList = springHandler.getControllerUriList(fullMethod);
        Assert.assertEquals(exists, !JavaCGUtil.isCollectionEmpty(controllerUriList));
        printListContent(controllerUriList, fullMethod);

        String controllerUri = springHandler.getControllerUri(fullMethod);
        Assert.assertEquals(exists, StringUtils.isNotBlank(controllerUri));
        printObjectContent(controllerUri, fullMethod);
    }

    @Test
    public void testCheckSpringTask() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            doCheckSpringTask(springHandler, JavaCGClassMethodUtil.formatFullMethodStr(SpringServiceImplA1.class.getName(), "test1"));
            doCheckSpringTask(springHandler, JavaCGClassMethodUtil.formatFullMethodStr(SpringServiceImplA1.class.getName(), "test2"));
        }
    }

    private void doCheckSpringTask(SpringHandler springHandler, String fullMethod) {
        boolean isSpringTask = springHandler.checkSpringTask(fullMethod);
        printObjectContent(isSpringTask, fullMethod);
    }
}
