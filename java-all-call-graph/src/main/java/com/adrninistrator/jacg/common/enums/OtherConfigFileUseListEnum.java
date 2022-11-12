package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OtherConfigFileUseListEnum {
    OCFULE_FIND_KEYWORD_4CALLEE(InputDirEnum.IDE_KEYWORD_CONF.getDirName() + "/find_keyword_4callee.properties",
            "先生成对应的向上方法完整调用链，再对生成目录的文件根据关键字生成到起始方法的调用链时，用于指定关键字"),
    OCFULE_FIND_KEYWORD_4CALLER(InputDirEnum.IDE_KEYWORD_CONF.getDirName() + "/find_keyword_4caller.properties",
            "先生成对应的向下方法完整调用链，再对生成目录的文件根据关键字生成到起始方法的调用链时，用于指定关键字"),
    ;

    private final String fileName;
    private final String desc;

    OtherConfigFileUseListEnum(String fileName, String desc) {
        this.fileName = fileName;
        this.desc = desc;
    }

    public String getFileName() {
        return fileName;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return fileName;
    }
}
