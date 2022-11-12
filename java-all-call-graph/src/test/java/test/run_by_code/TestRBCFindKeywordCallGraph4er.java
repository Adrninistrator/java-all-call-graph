package test.run_by_code;

import com.adrninistrator.jacg.find_keyword.FindKeywordCallGraph;
import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class TestRBCFindKeywordCallGraph4er extends TestRunByCodeBase {
    @Test
    public void test() {
        new FindKeywordCallGraph().find(false, configureWrapper);
    }
}
