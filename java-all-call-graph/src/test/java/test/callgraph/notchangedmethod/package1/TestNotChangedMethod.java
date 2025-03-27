package test.callgraph.notchangedmethod.package1;

import test.callgraph.enums.DbStatementEnum;

/**
 * @author adrninistrator
 * @date 2025/3/22
 * @description:
 */
public class TestNotChangedMethod {

    private static int staticField;

    private int instanceField;

    public int compareMethod(int input) throws Exception {
        // 加载/存储指令 (iload, istore, aload, astore)
        int localVar = input;
        Object obj = new Object();
        int[] array = new int[10];

        // 算术指令 (iadd, isub, imul, idiv, irem)
        localVar = localVar + 5;
        localVar -= 3;
        localVar *= 2;
        localVar /= 4;
        int remainder = localVar % 3;

        // 类型转换指令 (i2d, d2i)
        double d = (double) localVar;
        int fromDouble = (int) d;

        // 控制转移 (ifeq, if_icmpge, goto)
        if (localVar == 0) {
            localVar = 1;
        } else if (localVar >= 10) {
            localVar = 9;
        }

        // 循环与数组操作 (iinc, iaload, iastore)
        for (int i = 0; i < array.length; i++) {
            array[i] = i * i;
            remainder += array[i];
        }

        // switch语句 (tableswitch/lookupswitch)
        switch (localVar) {
            case 1:
                remainder += 1;
                break;
            case 2:
                remainder += 2;
                break;
            default:
                remainder -= 1;
        }

        // 方法调用 (invokevirtual, invokestatic, invokespecial)
        obj.toString();
        staticField = Math.max(remainder, 0);
        TestNotChangedMethod instance = new TestNotChangedMethod();

        // 异常处理 (athrow, try-catch)
        try {
            if (remainder < 0) {
                throw new Exception("Negative remainder");
            }
        } catch (Exception e) {
            System.out.println(e.getMessage());
            throw e;
        }

        // 同步块 (monitorenter, monitorexit)
        synchronized (obj) {
            instance.instanceField = remainder;
        }

        // 字段访问 (getfield, putfield, getstatic)
        staticField += instance.instanceField;

        // 类型检查 (instanceof, checkcast)
        if (obj instanceof String) {
            String str = (String) obj;
            System.out.println(str);
        }

        DbStatementEnum dbStatementEnum = DbStatementEnum.getFromStatement(String.valueOf(System.currentTimeMillis()));
        switch (dbStatementEnum) {
            case DSE_SELECT:
                System.out.println("1");
                break;
            case DSE_INSERT:
                System.out.println("2");
                break;
            case DSE_REPLACE:
                System.out.println("3");
                break;
            case DSE_UPDATE:
                System.out.println("4");
                break;
            case DSE_DELETE:
                System.out.println("5");
                break;
            default:
                System.out.println("a");
                break;
        }

        return staticField;
    }
}
