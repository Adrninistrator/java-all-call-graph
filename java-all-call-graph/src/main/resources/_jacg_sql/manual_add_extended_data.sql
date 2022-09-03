CREATE TABLE if not exists manual_add_extended_data_{appName} (
  data_id integer(18) NOT NULL AUTO_INCREMENT COMMENT '自增主键',
  caller_full_method text NOT NULL COMMENT '调用方，完整方法（类名+方法名+参数），或使用*代表不限调用方',
  callee_full_method text NOT NULL COMMENT '被调用方，完整方法（类名+方法名+参数）',
  callee_seq_in_caller integer(18) NOT NULL DEFAULT 1 COMMENT '被调用方法在调用方法中的序号，从1开始',
  data_type text NOT NULL COMMENT '自定义数据类型，MB_SQL: Mybatis的Mapper的数据库操作及表名',
  data_value text NOT NULL COMMENT '自定义数据内容，JSON字符串格式',
  PRIMARY KEY (data_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='人工添加的自定义数据表';