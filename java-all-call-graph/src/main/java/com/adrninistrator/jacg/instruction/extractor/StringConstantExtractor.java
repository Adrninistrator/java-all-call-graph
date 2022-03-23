package com.adrninistrator.jacg.instruction.extractor;

import org.apache.bcel.generic.Instruction;
import org.apache.bcel.generic.LDC;
import org.apache.bcel.generic.MethodGen;

/**
 * @author adrninistrator
 * @date 2021/10/31
 * @description:
 */
public class StringConstantExtractor implements ConstantExtractorInterface {
    @Override
    public Object extractConstantFromInstruction(Instruction instruction, MethodGen methodGen) {
        if (!(instruction instanceof LDC)) {
            return null;
        }

        // String常量
        LDC ldc = (LDC) instruction;
        Object o = ldc.getValue(methodGen.getConstantPool());
        if (!(o instanceof String)) {
            return null;
        }
        return o;
    }
}
