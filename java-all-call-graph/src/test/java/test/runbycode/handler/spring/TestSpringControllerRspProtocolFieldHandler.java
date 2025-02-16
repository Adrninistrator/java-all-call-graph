package test.runbycode.handler.spring;

import com.adrninistrator.jacg.handler.dto.spring.SpringControllerReturnTypeWithGenerics;
import com.adrninistrator.jacg.handler.spring.SpringControllerRspProtocolFieldHandler;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/12/3
 * @description:
 */
public class TestSpringControllerRspProtocolFieldHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void testQueryAllReturnTypeWithGenerics() {
        try (SpringControllerRspProtocolFieldHandler springControllerRspProtocolFieldHandler = new SpringControllerRspProtocolFieldHandler(configureWrapper)) {
            List<SpringControllerReturnTypeWithGenerics> list = springControllerRspProtocolFieldHandler.queryAllReturnTypeWithGenerics();
            printListContent(list);
        }
    }
}
