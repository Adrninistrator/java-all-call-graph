package test.callgraph.spring.bean.use;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.define.SpringInterfaceA;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Service
public class TestSpringBeanA {

    @Autowired
    private SpringInterfaceA springInterfaceA;

    @Resource(name = "threadPoolTaskExecutor1")
    private ThreadPoolTaskExecutor threadPoolTaskExecutor;

    public void test() {
        springInterfaceA.test1();
        threadPoolTaskExecutor.execute(() -> springInterfaceA.test1());
    }
}
