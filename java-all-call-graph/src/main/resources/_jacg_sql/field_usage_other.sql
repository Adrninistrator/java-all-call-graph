CREATE TABLE if not exists jacg_field_usage_other_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  method_return_type varchar(255) NOT NULL COMMENT '方法返回类型类名（包含数组标志）',
  static_flag tinyint NOT NULL COMMENT 'static标志，1:是，0:否',
  get_or_put tinyint NOT NULL COMMENT '使用字段时get还是put，1:get，0:put',
  field_in_simple_class_name varchar(255) NOT NULL COMMENT '被使用的字段所在的唯一类名',
  field_name varchar(350) NOT NULL COMMENT '被使用的字段名称',
  field_type varchar(255) NOT NULL COMMENT '被使用的字段类型（包含数组标志）',
  line_number int NOT NULL COMMENT '使用字段的源代码行号',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  method_hash varchar(32) NOT NULL COMMENT '方法hash+字节数',
  field_in_class_name varchar(255) NOT NULL COMMENT '被使用的字段所在的类名',
  class_jar_num int DEFAULT NULL COMMENT '类所在的jar文件序号',
  field_jar_num int DEFAULT NULL COMMENT '被使用的字段所在的jar文件序号',
  PRIMARY KEY (record_id),
  INDEX idx_fuo_scn_{appName}(simple_class_name),
  -- 需要使用的单列索引
  INDEX idx_fuo_fiscn_{appName}(field_in_simple_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='使用其他类中字段的使用情况表';