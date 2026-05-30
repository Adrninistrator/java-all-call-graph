package test.callgraph.spring.custommethodcall.impl.interfaceImpl;

import org.springframework.stereotype.Service;
import test.callgraph.spring.custommethodcall.service.Itrade;

import java.util.Collections;
import java.util.Map;

@Service("ImpClazzD")
public class ImpClazzD implements Itrade {
    @Override
    public Map<String, Object> execute(Object input) {
        Map<String,Object> map = (Map<String,Object>)input;
        int result = getSquare(Integer.parseInt(map.get("n1").toString())) + getSquare(Integer.parseInt(map.get("n2").toString()));
        return Collections.singletonMap("result", result);
    }

    public int getSquare(int a) {
        return a*a;
    }

}
