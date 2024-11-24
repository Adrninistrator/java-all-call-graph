package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ExtendsImpl;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

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
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_EXTENDS_IMPL,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_EXTENDS_IMPL_PRE
)
public class WriteDbHandler4ExtendsImplPre extends AbstractWriteDbHandler<WriteDbData4ExtendsImpl> {
    // 父类或接口类名
    private final Set<String> superClassOrInterfaceNameSet = new HashSet<>();

    public WriteDbHandler4ExtendsImplPre(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4ExtendsImpl genData(String[] array) {
        String upwardClassName = array[3];
        // 记录父类或接口类名
        superClassOrInterfaceNameSet.add(upwardClassName);

        // 固定返回null，当前类处理时不需要向数据库表写入数据
        return null;
    }

    @Override
    public String[] chooseFileColumnDesc() {
        throw new JavaCG2RuntimeException("不会调用当前方法");
    }

    @Override
    public String[] chooseFileDetailInfo() {
        throw new JavaCG2RuntimeException("不会调用当前方法");
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ExtendsImpl data) {
        throw new JavaCG2RuntimeException("不会调用当前方法");
    }

    public Set<String> getSuperClassOrInterfaceNameSet() {
        return superClassOrInterfaceNameSet;
    }
}
