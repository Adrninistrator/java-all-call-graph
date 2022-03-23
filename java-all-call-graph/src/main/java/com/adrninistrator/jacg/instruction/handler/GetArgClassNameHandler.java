package com.adrninistrator.jacg.instruction.handler;

import com.adrninistrator.jacg.instruction.util.InstructionUtil;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.MethodGen;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/10/31
 * @description: 获取指定方法调用指令对应的参数类名列表处理类
 */
public class GetArgClassNameHandler {

    private static final Logger logger = LoggerFactory.getLogger(GetArgClassNameHandler.class);

    /**
     * 获取指定方法调用指令对应的常量参数值列表
     *
     * @param ih        指定方法调用指令
     * @param methodGen 指定方法的MethodGen
     * @param offset    指定指令对应代码行号向前的偏移
     * @return
     */
    public List<String> getArgClassNameList(InstructionHandle ih, MethodGen methodGen, int offset) {
        List<String> classNameList = new ArrayList<>();

        // 从指定指令对应的指定偏移代码行号的第一条指令开始获取参数类名
        InstructionHandle offsetLineFirstIH = InstructionUtil.getInstructionHandleForward(ih, offset, methodGen);
        if (offsetLineFirstIH == null) {
            logger.error("未获取到指定指令对应的代码行号相同的第一条指令 {} {}", offset, InstructionUtil.getMethodInfo(methodGen));
            // 返回空列表;
            return classNameList;
        }

        // 从指定指令对应的指定偏移代码行号下一行的第一条指令
        InstructionHandle offsetNextLineFirstIH = InstructionUtil.getOffsetNextLineFirstIH(ih, methodGen, offset);

        // 从偏移第一条指令往后找
        InstructionHandle currIh = offsetLineFirstIH;
        while (currIh != null && !currIh.equals(offsetNextLineFirstIH)) {

            // 获取参数类名
            String argClassName = InstructionUtil.getClassNameFromIH(currIh, ih, methodGen);
            if (argClassName != null && !classNameList.contains(argClassName)) {
                classNameList.add(argClassName);
            }

            currIh = currIh.getNext();
        }

        return classNameList;
    }
}
