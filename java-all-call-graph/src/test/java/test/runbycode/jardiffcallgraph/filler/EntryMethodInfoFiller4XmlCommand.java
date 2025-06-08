package test.runbycode.jardiffcallgraph.filler;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.entrymethodinfo.BaseEntryMethodInfo;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ParsedCustomData;
import com.adrninistrator.jacg.handler.businessdata.ParsedCustomDataHandler;
import com.adrninistrator.jacg.handler.entrymethodinfo.AbstractEntryMethodInfoFiller;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import test.runbycode.jardiffcallgraph.dto.entrymethodinfo.EntryMethod4XmlCommand;
import test.runbycode.parsedcustomdata.parser.TestParser4XmlCommand;

/**
 * @author adrninistrator
 * @date 2025/6/7
 * @description:
 */
public class EntryMethodInfoFiller4XmlCommand extends AbstractEntryMethodInfoFiller {

    private final ParsedCustomDataHandler parsedCustomDataHandler;

    public EntryMethodInfoFiller4XmlCommand(ConfigureWrapper configureWrapper, String tableSuffix) {
        super(configureWrapper, tableSuffix);
        parsedCustomDataHandler = new ParsedCustomDataHandler(dbOperWrapper);
    }

    public EntryMethodInfoFiller4XmlCommand(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        parsedCustomDataHandler = new ParsedCustomDataHandler(dbOperWrapper);
    }

    public EntryMethodInfoFiller4XmlCommand(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        parsedCustomDataHandler = new ParsedCustomDataHandler(dbOperWrapper);
    }

    @Override
    public BaseEntryMethodInfo query(String entryMethod, String entryMethodReturnType) {
        MethodDetailNoReturnType methodDetailNoReturnType = JACGClassMethodUtil.genMethodDetailNoReturnType(entryMethod);
        String classAndMethod = JACGClassMethodUtil.genClassAndMethodName(methodDetailNoReturnType.getClassName(), methodDetailNoReturnType.getMethodName());
        WriteDbData4ParsedCustomData parsedCustomData = parsedCustomDataHandler.querySingleParsedCustomDataByTypeKey(TestParser4XmlCommand.DATA_TYPE, classAndMethod);
        if (parsedCustomData == null) {
            return null;
        }
        EntryMethod4XmlCommand entryMethod4XmlCommand = new EntryMethod4XmlCommand();
        entryMethod4XmlCommand.setCommandKey(parsedCustomData.getDataValue());
        return entryMethod4XmlCommand;
    }
}
