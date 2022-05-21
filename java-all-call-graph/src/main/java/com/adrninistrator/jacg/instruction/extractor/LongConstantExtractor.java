package com.adrninistrator.jacg.instruction.extractor;

import org.apache.bcel.generic.Instruction;
import org.apache.bcel.generic.LDC2_W;
import org.apache.bcel.generic.MethodGen;

/**
 * @author adrninistrator
 * @date 2021/10/31
 * @description:
 */
public class LongConstantExtractor implements ConstantExtractorInterface {
    @Override
    public Object extractConstantFromInstruction(Instruction instruction, MethodGen methodGen) {
        if (!(instruction instanceof LDC2_W)) {
            return null;
        }

        // Long常量
        LDC2_W ldc2W = (LDC2_W) instruction;
        Object o = ldc2W.getValue(methodGen.getConstantPool());
        if (!(o instanceof Long)) {
            return null;
        }
        return o;
    }
}
