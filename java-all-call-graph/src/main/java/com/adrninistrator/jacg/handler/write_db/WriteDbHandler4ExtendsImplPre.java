package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4ExtendsImpl;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，继承与实现相关信息，预处理
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_EXTENDS_IMPL,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_ILLEGAL
)
public class WriteDbHandler4ExtendsImplPre extends AbstractWriteDbHandler<WriteDbData4ExtendsImpl> {
    // 父类或接口类名
    private final Set<String> superClassOrInterfaceNameSet = new HashSet<>();

    @Override
    protected WriteDbData4ExtendsImpl genData(String[] array) {
        String upwardClassName = array[3];
        // 记录父类或接口类名
        superClassOrInterfaceNameSet.add(upwardClassName);

        // 固定返回null，当前类处理时不需要向数据库表写入数据
        return null;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ExtendsImpl data) {
        throw new JavaCGRuntimeException("不会调用当前方法");
    }

    public Set<String> getSuperClassOrInterfaceNameSet() {
        return superClassOrInterfaceNameSet;
    }
}
