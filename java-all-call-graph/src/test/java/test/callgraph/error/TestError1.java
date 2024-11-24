package test.callgraph.error;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.lang.reflect.Type;

/**
 * @author adrninistrator
 * @date 2024/11/23
 * @description:
 */
public class TestError1 {

    public Pair<Type, Type> getType1() {
        return new ImmutablePair<>(int[].class, String[].class);
    }
}
