package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/8/21
 * @description:
 */
public enum InputDirEnum {
    IDE_CONFIG("~jacg_config", "主要的配置文件"),
    IDE_SQL("~jacg_sql", "保存sql脚本的目录"),
    IDE_KEYWORD_CONF("~jacg_find_keyword", "保存需要查找的关键字的目录"),
    IDE_EXTENSIONS("~jacg_extensions", "保存自定义处理类配置文件的目录"),
    ;

    private String dirName;
    private String desc;

    InputDirEnum(String dirName, String desc) {
        this.dirName = dirName;
        this.desc = desc;
    }

    public String getDirName() {
        return dirName;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return dirName;
    }
}
