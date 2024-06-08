package test.runbycode.junit;

import org.junit.internal.MethodSorter;
import org.junit.runners.model.FrameworkMethod;

import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2024/3/17
 * @description:
 */
public class ComparatorFrameworkMethod implements Comparator<FrameworkMethod> {
    private static final ComparatorFrameworkMethod INSTANCE = new ComparatorFrameworkMethod();

    public static ComparatorFrameworkMethod getInstance() {
        return INSTANCE;
    }

    private ComparatorFrameworkMethod() {
    }

    @Override
    public int compare(FrameworkMethod o1, FrameworkMethod o2) {
        return MethodSorter.NAME_ASCENDING.compare(o1.getMethod(), o2.getMethod());
    }
}
