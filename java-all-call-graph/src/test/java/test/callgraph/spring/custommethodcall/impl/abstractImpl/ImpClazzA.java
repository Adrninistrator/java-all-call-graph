package test.callgraph.spring.custommethodcall.impl.abstractImpl;

import org.springframework.stereotype.Service;
import test.callgraph.spring.custommethodcall.service.AbstractClazz;
import test.callgraph.spring.custommethodcall.service.MyService;

import javax.annotation.Resource;
import java.util.Map;

@Service
public class ImpClazzA extends AbstractClazz {
    @Resource(name = "MyServiceA")
    private MyService myService;

    @Override
    protected int getA() {
        return methodB();
    }

    @Override
    protected int step1(Object input) {
        Map<String, Object> map = (Map<String, Object>) input;

        return Double.valueOf(myService.computeSqrt(map).get("result").toString()).intValue();
    }

    @Override
    protected int step2(Object input) {
        Map<String, Object> map = (Map<String, Object>) input;

        return Integer.parseInt(myService.computeSqure(map).get("result").toString());
    }

    @Override
    protected int step3(Object input) {
        Map<String, Object> map = (Map<String, Object>) input;

        return Double.valueOf(myService.computeSqrt(map).get("result").toString()).intValue()
                + Integer.parseInt(myService.computeSqure(map).get("result").toString());
    }

    private int methodB() {
        return methodC();
    }

    private int methodC() {
        return 0;
    }
}
