package test.callgraph.github.issue80;

import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * 历史数据清理任务对象
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
public class ClearHistoryTask {

    /**
     * 主键ID
     */
    private Long id;

    /**
     * 任务标识
     */
    private String taskCode;

    /**
     * 任务排序
     */
    private Integer priority;

    /**
     * 策略类型
     */
    private Integer strategy;

    /**
     * 策略配置
     */
    private String strategyConfig;

    /**
     * 业务表名
     */
    private String tableName;

    /**
     * 时间字段
     */
    private String timeField;

    /**
     * 保留时长（日）
     */
    private Integer retainDay;

    /**
     * 过滤条件
     */
    private String condition;

    /**
     * 分批大小
     */
    private Integer batchSize;

    /**
     * 停顿时间（毫秒）
     */
    private Integer pauseMillis;


    @Builder(builderClassName = "SimpleBuilder",
            builderMethodName = "simpleBuilder")
    public ClearHistoryTask(Long id,
                            String tableName,
                            String timeField,
                            Integer retainDay,
                            String condition,
                            Integer batchSize,
                            Integer pauseMillis) {
        this.id = id;
        this.priority = 10;
        this.tableName = tableName;
        this.timeField = timeField;
        this.retainDay = retainDay;
        this.condition = condition;
        this.batchSize = batchSize;
        this.pauseMillis = pauseMillis;
    }

    @Builder(builderClassName = "DefaultBuilder")
    public ClearHistoryTask(Long id,
                            String taskCode,
                            Integer priority,
                            Integer strategy,
                            String strategyConfig,
                            String tableName,
                            String timeField,
                            Integer retainDay,
                            String condition,
                            Integer batchSize,
                            Integer pauseMillis) {
        this.id = id;
        this.taskCode = taskCode;
        this.priority = priority;
        this.strategy = strategy;
        this.strategyConfig = strategyConfig;
        this.tableName = tableName;
        this.timeField = timeField;
        this.retainDay = retainDay;
        this.condition = condition;
        this.batchSize = batchSize;
        this.pauseMillis = pauseMillis;
    }
}