package com.adrninistrator.jacg.dto.access_flag;

import org.apache.bcel.classfile.AccessFlags;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description:
 */
public class JACGAccessFlags extends AccessFlags {
    public JACGAccessFlags(int accessFlags) {
        super(accessFlags);
    }
}
