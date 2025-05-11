package com.adrninistrator.jacg.handler.entrymethodinfo;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.entrymethodinfo.BaseEntryMethodInfo;
import com.adrninistrator.jacg.dto.entrymethodinfo.EntryMethodInfo4SpringController;
import com.adrninistrator.jacg.dto.entrymethodinfo.EntryMethodInfo4SpringTask;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2024/3/24
 * @description: 入口方法信息填充类，处理Spring Controller与Task
 */
public class EntryMethodInfoFiller4Spring extends AbstractEntryMethodInfoFiller {

    private SpringHandler springHandler;

    public EntryMethodInfoFiller4Spring(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        init();
    }

    public EntryMethodInfoFiller4Spring(ConfigureWrapper configureWrapper, String tableSuffix) {
        super(configureWrapper, tableSuffix);
        init();
    }

    public EntryMethodInfoFiller4Spring(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        init();
    }

    private void init() {
        springHandler = new SpringHandler(dbOperWrapper);
    }

    @Override
    public BaseEntryMethodInfo query(String entryMethod, String entryMethodReturnType) {
        String controllerUri = springHandler.queryControllerUri(entryMethod, entryMethodReturnType);
        if (StringUtils.isNotBlank(controllerUri)) {
            EntryMethodInfo4SpringController entryMethodInfo4SpringController = new EntryMethodInfo4SpringController();
            entryMethodInfo4SpringController.setControllerUti(controllerUri);
            return entryMethodInfo4SpringController;
        }

        if (springHandler.checkSpringTask(entryMethod, entryMethodReturnType)) {
            return new EntryMethodInfo4SpringTask();
        }
        return null;
    }
}
