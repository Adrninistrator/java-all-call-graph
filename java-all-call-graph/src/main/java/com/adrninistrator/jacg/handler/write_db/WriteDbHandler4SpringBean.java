package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringBean;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.dto.output.JavaCGOutputInfo;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，Spring Bean信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_SPRING_BEAN,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_BEAN
)
public class WriteDbHandler4SpringBean extends AbstractWriteDbHandler<WriteDbData4SpringBean> {
    /*
        记录Spring Bean相关信息
        key
            Spring Bean名称
        value
            Spring Bean对应类名
     */
    private Map<String, String> springBeanMap;

    public WriteDbHandler4SpringBean(JavaCGOutputInfo javaCGOutputInfo) {
        super(javaCGOutputInfo);
    }

    @Override
    protected WriteDbData4SpringBean genData(String[] array) {
        String springBeanName = array[0];
        String seq = array[1];
        String className = array[2];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }
        springBeanMap.put(springBeanName, className);
        return new WriteDbData4SpringBean(springBeanName, Integer.parseInt(seq), className);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringBean data) {
        return new Object[]{
                genNextRecordId(),
                data.getSpringBeanName(),
                data.getSeq(),
                data.getClassName()
        };
    }

    public Map<String, String> getSpringBeanMap() {
        return springBeanMap;
    }

    public void setSpringBeanMap(Map<String, String> springBeanMap) {
        this.springBeanMap = springBeanMap;
    }
}
