package test.callgraph.spring.custommethodcall;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import test.callgraph.spring.custommethodcall.router.RouteClazz;
import test.callgraph.spring.custommethodcall.service.MyService;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.Map;

@RestController
public class TestController {
    @Resource(name = "MyServiceA")
    private MyService myServiceA;

    @GetMapping("/serviceA")
    public Map<String, Object> serviceA(String num1, String num2, String opType) {
        Map<String, Object> map = new HashMap<>();
        map.put("n1", num1);
        map.put("n2", num2);
        RouteClazz routeClazz = new RouteClazz("impClazzA", map);
        routeClazz.setAsyInvoke(true);
        return routeClazz.invoke();
    }

    @GetMapping("/serviceB")
    public Map<String, Object> serviceB(String num1, String num2, String opType) {
        Map<String, Object> map = new HashMap<>();
        map.put("n1", num1);
        map.put("n2", num2);
        if (opType.equals("1")) {
            return myServiceA.computeSqure(map);
        } else {
            return myServiceA.computeSqrt(map);
        }
    }
}
