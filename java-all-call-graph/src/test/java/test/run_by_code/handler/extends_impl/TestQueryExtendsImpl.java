package test.run_by_code.handler.extends_impl;

import com.adrninistrator.jacg.handler.extends_impl.JACGExtendsImplHandler;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.call_graph.interfaces.interfaces.InterfaceSuper1;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/5
 * @description:
 */
public class TestQueryExtendsImpl extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestQueryExtendsImpl.class);

    @Test
    public void testGetChildClassList() {
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            doTestGetChildClassList(jacgExtendsImplHandler, InterfaceSuper1.class.getName());
        }
    }

    private void doTestGetChildClassList(JACGExtendsImplHandler jacgExtendsImplHandler, String className) {
        List<String> list = jacgExtendsImplHandler.queryChildClassListByFull(className, true, true, true, true);
        logger.info("子接口+子类 {}\n{}", className, StringUtils.join(list, "\n"));

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, true, false, false, false);
        logger.info("接口 {}\n{}", className, StringUtils.join(list, "\n"));

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, true, true);
        logger.info("类 {}\n{}", className, StringUtils.join(list, "\n"));

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, true, false);
        logger.info("抽象类 {}\n{}", className, StringUtils.join(list, "\n"));

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, false, true);
        logger.info("非抽象类 {}\n{}", className, StringUtils.join(list, "\n"));
    }
}
