package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;

/**
 * @author adrninistrator
 * @date 2025/6/6
 * @description: 解析文件并将解析的数据写入自定义表对应文件的抽象父类
 * 只支持创建一个实例，否则生成的文件会覆盖，不同类型的数据都需要在同一个子类中处理
 */
public abstract class AbstractCodeParserWithCustomData extends AbstractSaveData2FileParser {

    public static final String FILE_NAME = "parsed_custom_data";

    @Override
    public String chooseFileName() {
        return FILE_NAME;
    }
}
