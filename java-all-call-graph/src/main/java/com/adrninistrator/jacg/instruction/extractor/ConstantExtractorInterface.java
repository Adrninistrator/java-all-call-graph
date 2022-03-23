package com.adrninistrator.jacg.instruction.extractor;

import org.apache.bcel.generic.Instruction;
import org.apache.bcel.generic.MethodGen;

/**
 * @author adrninistrator
 * @date 2021/10/31
 * @description: 从指令获取常量参数值的接口
 */
public interface ConstantExtractorInterface {

    /**
     * 从指令获取常量参数值
     *
     * @param instruction 指令
     * @param methodGen   指定方法的MethodGen
     * @return null: 未获取到；非null: 获取到
     */
    Object extractConstantFromInstruction(Instruction instruction, MethodGen methodGen);
}
