package test.callgraph.signature;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description:
 */
public interface TestInterfaceWithSignature1<T1, T2> {
    void test();

    T2 test2(T1 t1);

    T2 test3(List<String> stringList);
}
