package com.adrninistrator.jacg.neo4j.handler.extendsimpl;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ExtendsImpl;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.neo4j.domain.node.JACGExtendsImpl;
import com.adrninistrator.jacg.neo4j.repository.JACGExtendsImplRepository;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/7/23
 * @description:
 */
public class Neo4jExtendsImplHandler extends JACGExtendsImplHandler {

    private final JACGExtendsImplRepository jacgExtendsImplRepository;

    public Neo4jExtendsImplHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        jacgExtendsImplRepository = applicationContext.getBean(JACGExtendsImplRepository.class);
    }

    @Override
    protected boolean useNeo4j() {
        return true;
    }

    @Override
    protected List<WriteDbData4ExtendsImpl> queryDownloadBySimple(String simpleClassName) {
        List<JACGExtendsImpl> list = jacgExtendsImplRepository.queryDownloadBySimple(appName, simpleClassName);
        return new ArrayList<>(list);
    }
}
