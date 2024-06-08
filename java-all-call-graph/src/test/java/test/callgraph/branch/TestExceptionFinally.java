package test.callgraph.branch;

import org.junit.Assert;
import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2023/12/28
 * @description:
 */
public class TestExceptionFinally {

    @Test
    public void test1da_normal() {
        TestExceptions.test1da(2);
    }

    @Test
    public void test1da_exception() {
        Assert.assertThrows(ArithmeticException.class, () -> TestExceptions.test1da(1));
    }

    @Test
    public void test1daa_exception() {
        TestExceptions.test1daa(1);
    }

    @Test
    public void test3a() {
        TestExceptions.test3a(1);
    }
}
