package test.run_by_code.handler.spring;

import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfo;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.javacg.util.JavaCGMethodUtil;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.call_graph.spring.bean.define.impl.SpringServiceImplA1;
import test.call_graph.spring.mvc.TestSpringController1;
import test.call_graph.spring.mvc.TestSpringController2;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description:
 */
public class TestSpringHandler extends TestRunByCodeBase {

    @Test
    public void testGetAllControllerMethod() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<SpringControllerInfo> springControllerInfoList = springHandler.getAllControllerMethod();
            printListContent(springControllerInfoList);
        }
    }

    @Test
    public void testGetAllTaskMethod() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<String> springTaskMethodList = springHandler.getAllTaskMethod();
            printListContent(springTaskMethodList);
        }
    }

    @Test
    public void testGetControllerUriList() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            doGetControllerUriList(springHandler, JavaCGMethodUtil.formatFullMethodStr(TestSpringController1.class.getName(), "<init>"));
            doGetControllerUriList(springHandler, JavaCGMethodUtil.formatFullMethodStr(TestSpringController1.class.getName(), "test1"));
            doGetControllerUriList(springHandler, JavaCGMethodUtil.formatFullMethodStr(TestSpringController1.class.getName(), "test2"));
            doGetControllerUriList(springHandler, JavaCGMethodUtil.formatFullMethodStr(TestSpringController2.class.getName(), "test1"));
        }
    }

    private void doGetControllerUriList(SpringHandler springHandler, String fullMethod) {
        List<String> controllerUriList = springHandler.getControllerUriList(fullMethod);
        printListContent(controllerUriList, fullMethod);

        String controllerUri = springHandler.getControllerUri(fullMethod);
        printObjectContent(controllerUri, fullMethod);
    }

    @Test
    public void testCheckSpringTask() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            doCheckSpringTask(springHandler, JavaCGMethodUtil.formatFullMethodStr(SpringServiceImplA1.class.getName(), "test1"));
            doCheckSpringTask(springHandler, JavaCGMethodUtil.formatFullMethodStr(SpringServiceImplA1.class.getName(), "test2"));
        }
    }

    private void doCheckSpringTask(SpringHandler springHandler, String fullMethod) {
        boolean isSpringTask = springHandler.checkSpringTask(fullMethod);
        printObjectContent(isSpringTask, fullMethod);
    }
}
