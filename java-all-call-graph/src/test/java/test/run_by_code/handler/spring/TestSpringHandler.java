package test.run_by_code.handler.spring;

import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfo;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.javacg.util.JavaCGMethodUtil;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.call_graph.spring.bean.define.SpringServiceImplA1;
import test.call_graph.spring.mvc.TestSpringController1;
import test.call_graph.spring.mvc.TestSpringController2;
import test.run_by_code.handler.base.TestHandlerBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description:
 */
public class TestSpringHandler extends TestHandlerBase {
    private static final Logger logger = LoggerFactory.getLogger(TestSpringHandler.class);

    @Test
    public void testGetAllControllerMethod() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<SpringControllerInfo> springControllerInfoList = springHandler.getAllControllerMethod();
            if (springControllerInfoList == null) {
                return;
            }
            for (SpringControllerInfo springControllerInfo : springControllerInfoList) {
                logger.info("{} {}", springControllerInfo.getShowUri(), springControllerInfo.getFullMethod());
            }
        }
    }

    @Test
    public void testGetAllTaskMethod() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<String> springTaskMethodList = springHandler.getAllTaskMethod();
            if (springTaskMethodList == null) {
                return;
            }
            logger.info("{}", StringUtils.join(springTaskMethodList, "\n"));
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
        if (controllerUriList == null) {
            logger.info("不是Spring Controller方法 {}", fullMethod);
            return;
        }
        logger.info("{}\n{}", fullMethod, StringUtils.join(controllerUriList, "\n"));

        String controllerUri = springHandler.getControllerUri(fullMethod);
        logger.info("controllerUri {}", controllerUri);
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
        logger.info("{} {}", fullMethod, isSpringTask);
    }
}
