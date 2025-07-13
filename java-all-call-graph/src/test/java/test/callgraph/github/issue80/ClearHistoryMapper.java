package test.callgraph.github.issue80;

import lombok.experimental.UtilityClass;
import org.apache.ibatis.annotations.DeleteProvider;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.util.StringUtils;

import java.time.LocalDate;

@Mapper
public interface ClearHistoryMapper {

    /**
     * 历史数据清理操作
     *
     * @param targetDate 目标日期
     * @param task       处理任务
     * @return 处理条数
     */
    @DeleteProvider(type = ClearHistorySqlProvider.class, method = "buildSql")
    int clearHistory(@Param("targetDate") LocalDate targetDate,
                     @Param("task") ClearHistoryTask task);

    @UtilityClass
    @SuppressWarnings("java:S1118")
    class ClearHistorySqlProvider {

        public static String buildSql(
                @Param("task") final ClearHistoryTask task) {
            StringBuilder sqlBuilder = new StringBuilder()
                    .append("DELETE FROM ")
                    .append(task.getTableName())
                    .append(" WHERE ")
                    .append(task.getTimeField())
                    .append(" < #{targetDate}");
            if (!StringUtils.isEmpty(task.getCondition())) {
                sqlBuilder.append(" AND ")
                        .append(task.getCondition());
            }
            sqlBuilder.append(" LIMIT ")
                    .append(task.getBatchSize());
            return sqlBuilder.toString();
        }
    }
}