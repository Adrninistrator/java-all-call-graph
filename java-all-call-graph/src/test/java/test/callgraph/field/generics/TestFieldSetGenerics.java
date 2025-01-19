package test.callgraph.field.generics;

import test.callgraph.field.TestField1;
import test.callgraph.field.TestField2;
import test.callgraph.field.cycle.TestFieldCycle1;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * @author adrninistrator
 * @date 2024/12/3
 * @description:
 */
public class TestFieldSetGenerics {

    private Object data;

    public void setData(Object data) {
        this.data = data;
    }

    public void testList1() {
        List<TestField1> testField1List = new ArrayList<>();
        setData(testField1List);

        setData(new ArrayList<Integer>());
        setData(new ArrayList<TestField2>());
        setData(genData1());
    }

    public void testListWrite1() {
        List<TestField1> testField1List = new ArrayList<>();

        testField1List.add(new TestField1());

        setData(testField1List);
    }

    public void testListWrite2() {
        ArrayList<TestField1> testField1List = new ArrayList<>();

        testField1List.add(new TestField1());

        setData(testField1List);
    }

    public void testListRead1() {
        List<TestField1> testField1List = new ArrayList<>();

        TestField1 testField1 = testField1List.get(0);
        System.out.println(testField1);

        setData(testField1List);
    }

    public void testListRead2() {
        List<TestField1> testField1List = new ArrayList<>();

        for (TestField1 testField1a : testField1List) {
            System.out.println(testField1a);
        }

        setData(testField1List);
    }

    public void testMapWrite1() {
        Map<String, TestField1> testField1Map = new HashMap<>();

        testField1Map.put("", new TestField1());

        setData(testField1Map);
    }

    public void testMapWrite2() {
        Map<String, TestField1> testField1Map = new HashMap<>();

        testField1Map.computeIfAbsent("", k -> new TestField1());

        setData(testField1Map);
    }

    public void testMapWrite3() {
        Map<String, TestField1> testField1Map = new HashMap<>();

        testField1Map.computeIfAbsent("", new Function() {
            @Override
            public Object apply(Object object) {
                return new TestField1();
            }
        });

        setData(testField1Map);
    }

    public void testMapWrite4() {
        Map<String, TestField1> testField1Map = new HashMap<>();

        testField1Map.computeIfAbsent("", (Function) object -> new TestField1());

        setData(testField1Map);
    }

    public void testMapRead1() {
        Map<String, TestField1> testField1Map = new HashMap<>();

        TestField1 testField1 = testField1Map.get("");
        System.out.println(testField1);

        setData(testField1Map);
    }

    public void testMapRead2() {
        Map<String, TestField1> testField1Map = new HashMap<>();

        for (Map.Entry<String, TestField1> entry : testField1Map.entrySet()) {
            String key = entry.getKey();
            TestField1 value = entry.getValue();
            System.out.println(key + " " + value);
        }

        setData(testField1Map);
    }

    private Object genData1() {
        return new ArrayList<TestFieldCycle1>();
    }
}
