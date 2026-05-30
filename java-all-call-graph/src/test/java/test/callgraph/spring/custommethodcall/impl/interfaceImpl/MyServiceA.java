package test.callgraph.spring.custommethodcall.impl.interfaceImpl;

import org.springframework.stereotype.Service;
import test.callgraph.spring.custommethodcall.router.RouteClazz;
import test.callgraph.spring.custommethodcall.service.MyService;

import java.util.Map;

@Service("MyServiceA")
public class MyServiceA implements MyService {

    @Override
    public Map<String, Object> computeSqure(Map<String, Object> input) {
        RouteClazz routeClazz = new RouteClazz("ImpClazzD", input);
        routeClazz.setAsyInvoke(true);
        return routeClazz.invoke();
    }

    @Override
    public Map<String, Object> computeSqrt(Map<String, Object> input) {
        RouteClazz routeClazz = new RouteClazz("ImpClazzE", input);
        routeClazz.setAsyInvoke(true);
        return routeClazz.invoke();
    }

}
