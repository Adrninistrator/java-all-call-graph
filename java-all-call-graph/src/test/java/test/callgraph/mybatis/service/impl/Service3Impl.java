package test.callgraph.mybatis.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.callgraph.mybatis.service.AbstractService1;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2021/11/9
 * @description:
 */
@Service
public class Service3Impl {

    private Service2Impl service2;

    @Resource(name = Service1AImpl.BEAN_NAME)
    private AbstractService1 service1;

    public void test1() {
        service2.test1();

        service1.test1();
    }
}
