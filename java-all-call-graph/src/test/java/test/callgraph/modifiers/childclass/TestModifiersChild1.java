package test.callgraph.modifiers.childclass;

import test.callgraph.modifiers.superclass.TestModifiersSuper1;

/**
 * @author adrninistrator
 * @date 2025/8/7
 * @description:
 */
public class TestModifiersChild1 extends TestModifiersSuper1 {

    protected String flagInChild = "";

    private static final String STATIC_CHILD_FLAG = "";

    private final String nonStaticChildFlag = "";

    private static void staticChildMethod1() {
    }

    private void nonStaticChildMethod1() {
    }

    @Override
    protected void protectedInChildProtectedMethod1() {
        System.out.println("aaa");
    }

    @Override
    public void protectedInChildPublicMethod1() {
    }

    private void testAnonymousInnerClass() {
        Thread thread = new Thread() {
            private final String threadFlag = "";

            @Override
            public void run() {
                System.out.println(nonStaticChildFlag);
                nonStaticChildMethod1();
            }
        };

//        System.out.println(thread.threadFlag);
    }

    private void useNamedInnerClass() {
        System.out.println(NamedStaticInnerClass.STATIC_FLAG);
        NamedStaticInnerClass.test();

        NamedNonStaticInnerClass namedNonStaticInnerClass = new NamedNonStaticInnerClass();
        System.out.println(namedNonStaticInnerClass.nonStaticFlag);
        namedNonStaticInnerClass.test();

        System.out.println(NamedStaticInnerClass.NamedStaticInnerInnerClass.STATIC_FLAG);
        NamedStaticInnerClass.NamedStaticInnerInnerClass.test();

        NamedNonStaticInnerClass.NamedNonStaticInnerInnerClass namedNonStaticInnerInnerClass = namedNonStaticInnerClass.new NamedNonStaticInnerInnerClass();
        System.out.println(namedNonStaticInnerInnerClass.nonStaticFlag);
        namedNonStaticInnerInnerClass.test();
    }

    static class NamedStaticInnerClass {
        private static final String STATIC_FLAG = "";

        private static void test() {
            System.out.println(STATIC_CHILD_FLAG);
            staticChildMethod1();

            System.out.println(NamedStaticInnerInnerClass.STATIC_FLAG);
            NamedStaticInnerInnerClass.test();
        }

        static class NamedStaticInnerInnerClass {
            private static final String STATIC_FLAG = "";

            private static void test() {
                System.out.println(STATIC_CHILD_FLAG);
                staticChildMethod1();
            }
        }
    }

    class NamedNonStaticInnerClass {
        private final String nonStaticFlag = "";

        private void test() {
            System.out.println(nonStaticChildFlag);
            nonStaticChildMethod1();

            NamedNonStaticInnerInnerClass namedNonStaticInnerInnerClass = new NamedNonStaticInnerInnerClass();
            System.out.println(namedNonStaticInnerInnerClass.nonStaticFlag);
            namedNonStaticInnerInnerClass.test();

            System.out.println(NamedStaticInnerClass.STATIC_FLAG);
            NamedStaticInnerClass.test();
        }

        class NamedNonStaticInnerInnerClass {
            private final String nonStaticFlag = "";

            private void test() {
                System.out.println(nonStaticChildFlag);
                nonStaticChildMethod1();
            }
        }
    }
}
