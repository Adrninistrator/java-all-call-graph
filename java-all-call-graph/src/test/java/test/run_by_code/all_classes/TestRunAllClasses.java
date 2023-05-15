package test.run_by_code.all_classes;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.DoublePredicate;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.function.ToDoubleFunction;

/**
 * @author adrninistrator
 * @date 2023/4/27
 * @description:
 */
public class TestRunAllClasses extends TestRunByCodeBase {

    @Test
    public void testCaller() {
        DbOperWrapper dbOperWrapper = DbOperWrapper.genInstance(configureWrapper, this.getClass().getSimpleName());
        DbOperator dbOperator = dbOperWrapper.getDbOperator();
        try {
            String sql = "select distinct(" + DC.MC_CALLER_SIMPLE_CLASS_NAME + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName();
            sql = dbOperWrapper.formatSql(sql);
            List<String> callerSimpleClassNameList = dbOperator.queryListOneColumn(sql, String.class);
            configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, new HashSet<>(callerSimpleClassNameList));
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName);
            new RunnerGenAllGraph4Caller().run(configureWrapper);
        } finally {
            if (dbOperator != null) {
                dbOperator.closeDs();
            }
        }
    }

    @Test
    public void testCallee() {
        DbOperWrapper dbOperWrapper = DbOperWrapper.genInstance(configureWrapper, this.getClass().getSimpleName());
        DbOperator dbOperator = dbOperWrapper.getDbOperator();
        try {
            String sql = "select distinct(" + DC.MC_CALLEE_SIMPLE_CLASS_NAME + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName();
            sql = dbOperWrapper.formatSql(sql);
            List<String> calleeSimpleClassNameList = dbOperator.queryListOneColumn(sql, String.class);
            Set<String> calleeSimpleClassNameSet = new HashSet<>(calleeSimpleClassNameList.size());
            for (String calleeSimpleClassName : calleeSimpleClassNameList) {
                if (!calleeSimpleClassName.endsWith("[]") && !StringUtils.equalsAny(calleeSimpleClassName,
                        Consumer.class.getSimpleName(),
                        BiConsumer.class.getSimpleName(),
                        Predicate.class.getSimpleName(),
                        DoublePredicate.class.getSimpleName(),
                        Supplier.class.getSimpleName(),
                        Function.class.getSimpleName(),
                        ToDoubleFunction.class.getSimpleName(),
                        Object.class.getSimpleName()
                )) {
                    calleeSimpleClassNameSet.add(calleeSimpleClassName);
                }
            }
            configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, calleeSimpleClassNameSet);
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName);
            new RunnerGenAllGraph4Callee().run(configureWrapper);
        } finally {
            if (dbOperator != null) {
                dbOperator.closeDs();
            }
        }
    }
}
