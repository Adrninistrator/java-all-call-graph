package com.adrninistrator.jacg.util;

import org.apache.bcel.classfile.LineNumberTable;
import org.apache.bcel.generic.*;

/**
 * @author adrninistrator
 * @date 2021/8/11
 * @description:
 */
public class InstructionUtil {

    /**
     * 获取当前指令对应的代码行号相同的第一条指令
     *
     * @param ih
     * @param methodGen
     * @return null: 获取失败
     */
    public static InstructionHandle getSameLineFirstIH(InstructionHandle ih, MethodGen methodGen) {
        LineNumberTable lineNumberTable = methodGen.getLineNumberTable(methodGen.getConstantPool());
        int sourceLineNum = lineNumberTable.getSourceLine(ih.getPosition());
        if (sourceLineNum == -1) {
            System.err.println("### 获取代码行号失败 " + getMethodInfo(methodGen));
            return null;
        }

        InstructionHandle currIh = ih;
        InstructionHandle prevIh = currIh.getPrev();
        while (true) {
            if (prevIh == null) {
                // 获取到前一条指令为空，说明当前指令为当前方法第一行
                return currIh;
            }

            int sourceLineNumPrev = lineNumberTable.getSourceLine(prevIh.getPosition());
            if (sourceLineNumPrev == -1) {
                System.err.println("### 获取代码行号失败2 " + getMethodInfo(methodGen));
                return null;
            }
            if (sourceLineNumPrev < sourceLineNum) {
                return currIh;
            }

            currIh = currIh.getPrev();
            prevIh = currIh.getPrev();
        }
    }

    public static String getMethodInfo(MethodGen methodGen) {
        return methodGen.getClassName() + " " + methodGen.getMethod().getName();
    }

    /**
     * 从指定范围的InstructionHandle获取对应的类名
     *
     * @param beginIh   起始InstructionHandle
     * @param endIh     结束InstructionHandle
     * @param methodGen
     * @return
     */
    public static String getClassNameFromIH(InstructionHandle beginIh, InstructionHandle endIh, MethodGen methodGen) {
        String reqClassName = null;
        InstructionHandle tmpIh = beginIh;
        while (tmpIh != null && !tmpIh.equals(endIh)) {
            String tmpClassName;
            Instruction instruction = tmpIh.getInstruction();
            if (instruction instanceof ALOAD) {
                // 尝试从aload指令获得对应的类名
                tmpClassName = getClassNameFromAloadInstruction((ALOAD) instruction, tmpIh, methodGen);
            } else {
                // 尝试从非aload指令获得对应的类名
                tmpClassName = getClassNameFromOtherInstruction(instruction, methodGen);
            }

            if (tmpClassName != null) {
                reqClassName = tmpClassName;
                break;
            }
            tmpIh = tmpIh.getNext();
        }

        if (reqClassName == null || reqClassName.startsWith("java.")) {
            // 忽略JDK中的类
            return null;
        }

        return reqClassName;
    }

    // 尝试从aload指令获得对应的类名
    public static String getClassNameFromAloadInstruction(ALOAD aload, InstructionHandle curIh, MethodGen methodGen) {
        int index = aload.getIndex();
        if (!methodGen.isStatic() && index <= 0) {
            // 非静态方法，方法参数下标从1开始，跳过ALOAD 0
            return null;
        }

        Type[] argTypes = methodGen.getArgumentTypes();
        if (argTypes != null && argTypes.length > 0) {
            // 当前方法参数非空的处理
            if (methodGen.isStatic()) {
                // 静态方法，方法参数下标从0开始
                if (index <= argTypes.length - 1) {
                    return argTypes[index].toString();
                }
            } else {
                // 非静态方法，方法参数下标从1开始
                if (index <= argTypes.length) {
                    return argTypes[index - 1].toString();
                }
            }
        }

        // 向前找到对应的astore指令的前一条指令
        InstructionHandle prevIh = curIh.getPrev();
        InstructionHandle beforeAstoreIh = null;
        while (prevIh != null) {
            Instruction instruction = prevIh.getInstruction();
            if (instruction instanceof ASTORE) {
                ASTORE astore = (ASTORE) instruction;
                if (astore.getIndex() == index) {
                    beforeAstoreIh = prevIh.getPrev();
                    break;
                }
            }
            prevIh = prevIh.getPrev();
        }

        if (beforeAstoreIh == null) {
            System.err.println("### 向前找对应的astore指令的前一条指令失败 " + InstructionUtil.getMethodInfo(methodGen));
            return null;
        }

        // 尝试从非aload指令获得对应的类名
        return getClassNameFromOtherInstruction(beforeAstoreIh.getInstruction(), methodGen);
    }

    // 尝试从非aload指令获得对应的类名
    public static String getClassNameFromOtherInstruction(Instruction instruction, MethodGen methodGen) {
        ConstantPoolGen cp = methodGen.getConstantPool();
        if (instruction instanceof InvokeInstruction) {
            // 当前指令为调用方法
            InvokeInstruction invokeInstruction = (InvokeInstruction) instruction;
            String methodName = invokeInstruction.getMethodName(cp);
            if ("<init>".equals(methodName)) {
                // 构造函数
                return invokeInstruction.getClassName(cp);
            }
            // 非构造函数
            Type type = invokeInstruction.getReturnType(cp);
            return type != null ? type.toString() : null;
        }
        if (instruction instanceof CHECKCAST) {
            // 当前指令为强制类型转换
            CHECKCAST checkcast = (CHECKCAST) instruction;
            ObjectType objectType = checkcast.getLoadClassType(cp);
            return objectType != null ? objectType.toString() : null;
        }
        if (instruction instanceof GETFIELD) {
            // 当前指令为获取成员变量
            GETFIELD getfield = (GETFIELD) instruction;
            Type type = getfield.getType(cp);
            return type != null ? type.toString() : null;
        }
        if (instruction instanceof GETSTATIC) {
            // 当前指令为获取类静态变量
            GETSTATIC getstatic = (GETSTATIC) instruction;
            Type type = getstatic.getType(cp);
            return type != null ? type.toString() : null;
        }

        return null;
    }

    private InstructionUtil() {
        throw new IllegalStateException("illegal");
    }
}
