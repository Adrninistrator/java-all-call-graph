package com.adrninistrator.jacg.instruction.util;

import com.adrninistrator.jacg.instruction.extractor.StringConstantExtractor;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.ElementValuePair;
import org.apache.bcel.classfile.LineNumberTable;
import org.apache.bcel.generic.*;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2021/8/11
 * @description:
 */
public class InstructionUtil {

    private static final Logger logger = LoggerFactory.getLogger(InstructionUtil.class);

    private static final String LONG_CLASS_NAME = Long.class.getName();

    private static StringConstantExtractor stringConstantExtractor = new StringConstantExtractor();

    /**
     * 获取指定指令向前，代码行号对应的偏移量第一条指令
     *
     * @param ih        指定指令
     * @param offset    偏移量
     * @param methodGen 指定方法的MethodGen
     * @return null: 获取失败
     */
    public static InstructionHandle getInstructionHandleForward(InstructionHandle ih, int offset, MethodGen methodGen) {
        LineNumberTable lineNumberTable = methodGen.getLineNumberTable(methodGen.getConstantPool());
        int sourceLineNum = lineNumberTable.getSourceLine(ih.getPosition());
        if (sourceLineNum == -1) {
            logger.error("获取代码行号失败 {}", getMethodInfo(methodGen));
            return null;
        }

        int targetSourceLineNum = sourceLineNum + offset;

        InstructionHandle currIh = ih;
        InstructionHandle prevIh = currIh.getPrev();
        while (true) {
            if (prevIh == null) {
                // 获取到前一条指令为空，说明当前指令为当前方法第一行
                return currIh;
            }

            int sourceLineNumPrev = lineNumberTable.getSourceLine(prevIh.getPosition());
            if (sourceLineNumPrev == -1) {
                logger.error("获取代码行号失败2 {}", getMethodInfo(methodGen));
                return null;
            }
            if (sourceLineNumPrev < targetSourceLineNum) {
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

    /**
     * 尝试从aload指令获得对应的类名
     *
     * @param aload     aload指令
     * @param curIh     当前的InstructionHandle
     * @param methodGen 指定方法的MethodGen
     * @return
     */
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
        InstructionHandle beforeAstoreIh = getIHBeforeASTORE(curIh, index);

        if (beforeAstoreIh == null) {
            logger.error("向前找对应的astore指令的前一条指令失败 {}", getMethodInfo(methodGen));
            return null;
        }

        // 尝试从非aload指令获得对应的类名
        return getClassNameFromOtherInstruction(beforeAstoreIh.getInstruction(), methodGen);
    }

    /**
     * 从当前指令往前查找，获得index对应的ASTORE指令的前一条指令
     *
     * @param curIh 当前指令
     * @param index ASTORE指令的index
     * @return
     */
    public static InstructionHandle getIHBeforeASTORE(InstructionHandle curIh, int index) {
        InstructionHandle beforeAstoreIh = null;
        InstructionHandle prevIh = curIh.getPrev();
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

        return beforeAstoreIh;
    }

    /**
     * 从当前指令往前查找，获得所有index对应的ASTORE指令的前一条指令
     *
     * @param curIh 当前指令
     * @param index ASTORE指令的index
     * @return
     */
    public static List<InstructionHandle> getAllIHBeforeASTORE(InstructionHandle curIh, int index) {
        List<InstructionHandle> ihList = new ArrayList<>();

        InstructionHandle prevIh = curIh.getPrev();
        while (prevIh != null) {
            Instruction instruction = prevIh.getInstruction();
            if (instruction instanceof ASTORE) {
                ASTORE astore = (ASTORE) instruction;
                if (astore.getIndex() == index && prevIh.getPrev() != null) {
                    ihList.add(prevIh.getPrev());
                }
            }
            prevIh = prevIh.getPrev();
        }

        return ihList;
    }

    /**
     * 尝试从非aload指令获得对应的类名
     *
     * @param instruction 当前的指令
     * @param methodGen   指定方法的MethodGen
     * @return
     */
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

        logger.debug("调用了未处理的指令 {} {}:{}", instruction.getClass().getName(), methodGen.getClassName(), methodGen.getName());
        return null;
    }

    // 判断方法参数中是否有指定Long类型参数
    public static boolean checkExistsLongArg(Type[] arguments) {
        boolean longInArg = false;
        for (Type type : arguments) {
            if (type instanceof ObjectType) {
                ObjectType objectType = (ObjectType) type;
                if (LONG_CLASS_NAME.equals(objectType.getClassName())) {
                    longInArg = true;
                    break;
                }
            }
            if (type instanceof BasicType) {
                // 基本类型
                if (type.equals(Type.LONG)) {
                    longInArg = true;
                    break;
                }
            }
        }

        return longInArg;
    }

    /**
     * 从指定指令对应的指定偏移代码行号下一行的第一条指令（如果offset为0，则直接使用指定的指令）
     *
     * @param ih        指定指令
     * @param methodGen 指定方法的MethodGen
     * @param offset    指定指令对应代码行号向前的偏移
     * @return
     */
    public static InstructionHandle getOffsetNextLineFirstIH(InstructionHandle ih, MethodGen methodGen, int offset) {
        InstructionHandle offsetNextLineFirstIH;
        if (offset == 0) {
            offsetNextLineFirstIH = ih;
        } else {
            offsetNextLineFirstIH = getInstructionHandleForward(ih, offset + 1, methodGen);
        }
        return offsetNextLineFirstIH;
    }

    /**
     * 获得使用局部变量时赋值的常量字符串参数值列表
     *
     * @param ih        指定方法调用指令
     * @param methodGen 指定方法的MethodGen
     * @param offset    指定指令对应代码行号向前的偏移
     * @return
     */
    public static List<String> getLocalVariableConstantStringValue(InstructionHandle ih, MethodGen methodGen, int offset) {
        List<String> stringList = new ArrayList<>();

        // 从指定指令对应的指定偏移代码行号的第一条指令开始获取参数值
        InstructionHandle offsetLineFirstIH = getInstructionHandleForward(ih, offset, methodGen);
        if (offsetLineFirstIH == null) {
            logger.error("未获取到指定指令对应的代码行号相同的第一条指令 {} {}", offset, getMethodInfo(methodGen));
            // 返回空列表;
            return stringList;
        }

        // 从指定指令对应的指定偏移代码行号下一行的第一条指令
        InstructionHandle offsetNextLineFirstIH = getOffsetNextLineFirstIH(ih, methodGen, offset);

        // 从偏移第一条指令往后找
        InstructionHandle currIh = offsetLineFirstIH;
        while (currIh != null && !currIh.equals(offsetNextLineFirstIH)) {
            Instruction instruction = currIh.getInstruction();

            if (instruction instanceof ALOAD) {
                // 获取到ALOAD指令
                int index = ((ALOAD) instruction).getIndex();
                // 获取所有对应的ASTORE指令的前一条指令
                List<InstructionHandle> ihBeforeASTOREList = getAllIHBeforeASTORE(currIh, index);
                for (InstructionHandle ihBeforeASTORE : ihBeforeASTOREList) {
                    // 尝试获取对应的常量字符串参数值
                    Object o = stringConstantExtractor.extractConstantFromInstruction(ihBeforeASTORE.getInstruction(), methodGen);
                    if (o != null) {
                        stringList.add((String) o);
                    }
                }
            }

            currIh = currIh.getNext();
        }

        return stringList;
    }

    /**
     * 获取指定注解中指定名称的属性值
     *
     * @param annotationEntry
     * @param key
     * @return
     */
    public static String getAnnotationAttributesValue(AnnotationEntry annotationEntry, String key) {
        for (ElementValuePair elementValuePair : annotationEntry.getElementValuePairs()) {
            if (StringUtils.equals(key, elementValuePair.getNameString())) {
                return elementValuePair.getValue().toString();
            }
        }

        return "";
    }

    /**
     * 获取指定注解中所有属性名称及对应值，返回Map类型
     *
     * @param annotationEntry
     * @return
     */
    public static Map<String, String> getAnnotationAttributesMap(AnnotationEntry annotationEntry) {
        Map<String, String> map = new HashMap<>();
        for (ElementValuePair elementValuePair : annotationEntry.getElementValuePairs()) {
            String value = elementValuePair.getValue().toString();
            if (StringUtils.isNotBlank(value)) {
                map.put(elementValuePair.getNameString(), value);
            }
        }

        return map;
    }

    /**
     * 获取指定注解中所有属性名称及对应值，返回Map类型
     *
     * @param annotationEntryGen
     * @return
     */
    public static Map<String, String> getAnnotationGenAttributesMap(AnnotationEntryGen annotationEntryGen) {
        Map<String, String> map = new HashMap<>();
        for (ElementValuePairGen elementValuePairGen : annotationEntryGen.getValues()) {
            ElementValuePair elementValuePair = elementValuePairGen.getElementNameValuePair();
            String value = elementValuePair.getValue().toString();
            if (StringUtils.isNotBlank(value)) {
                map.put(elementValuePair.getNameString(), value);
            }
        }

        return map;
    }

    private InstructionUtil() {
        throw new IllegalStateException("illegal");
    }
}
