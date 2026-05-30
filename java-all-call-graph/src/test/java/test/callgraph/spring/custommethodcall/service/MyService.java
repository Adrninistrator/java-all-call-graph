package test.callgraph.spring.custommethodcall.service;

import java.util.Map;

public interface MyService {
    Map<String, Object> computeSqure(Map<String, Object> input);

    Map<String, Object> computeSqrt(Map<String, Object> input);

}
