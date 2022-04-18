package test.call_graph.manual_add_callgraph.unfixed;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/4/16
 * @description:
 */
public class TestUnfixedManualAddCallGraph {
    public String test1() {
        return new UnfixedService1a().invoke("");
    }

    public Map test2(Map map1) {
        return new AbstractUnFixedService1<Map, Map>() {
            @Override
            protected Map execute(Map map) {
                System.setProperty("", "");
                return map;
            }
        }.invoke(map1);
    }

    public Map test3(Map map1) {
        return new AbstractUnFixedService1<Map, Map>() {
            @Override
            protected Map execute(Map map) {
                System.setIn(null);
                return map;
            }
        }.invoke(map1);
    }

    public List test4(List list1) {
        return new AbstractUnFixedService1<List, List>() {
            @Override
            protected List execute(List list) {
                System.setOut(null);
                return list;
            }
        }.invoke(list1);
    }
}
