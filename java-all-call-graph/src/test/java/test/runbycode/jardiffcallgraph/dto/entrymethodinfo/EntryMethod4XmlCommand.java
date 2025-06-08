package test.runbycode.jardiffcallgraph.dto.entrymethodinfo;

import com.adrninistrator.jacg.dto.entrymethodinfo.BaseEntryMethodInfo;
import test.runbycode.parsedcustomdata.parser.TestParser4XmlCommand;

/**
 * @author adrninistrator
 * @date 2025/6/7
 * @description:
 */
public class EntryMethod4XmlCommand extends BaseEntryMethodInfo {

    private String commandKey;

    public EntryMethod4XmlCommand() {
        this.type = TestParser4XmlCommand.DATA_TYPE;
    }

    public String getCommandKey() {
        return commandKey;
    }

    public void setCommandKey(String commandKey) {
        this.commandKey = commandKey;
    }
}
