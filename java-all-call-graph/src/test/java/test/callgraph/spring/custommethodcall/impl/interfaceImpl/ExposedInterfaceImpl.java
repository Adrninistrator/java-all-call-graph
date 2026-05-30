package test.callgraph.spring.custommethodcall.impl.interfaceImpl;

import test.callgraph.spring.custommethodcall.service.ExposedInterface;
import test.callgraph.spring.custommethodcall.service.MyService;

import javax.annotation.Resource;
import java.util.Map;

public class ExposedInterfaceImpl implements ExposedInterface {
    @Resource(name = "MyServiceA")
    private MyService myServiceA;
    @Override
    public Map<String, Object> execute(Map<String, Object> map) {
        return myServiceA.computeSqure(map);
    }
}
