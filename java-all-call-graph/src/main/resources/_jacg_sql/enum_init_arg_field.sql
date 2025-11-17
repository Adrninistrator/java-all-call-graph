CREATE TABLE IF NOT EXISTS jacg_enum_init_arg_field_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(300) NOT NULL COMMENT '枚举唯一类名',
  arg_seq int NOT NULL COMMENT '枚举类构造函数用于赋值的参数序号（从1开始）',
  field_type varchar(255) NOT NULL COMMENT '枚举类构造函数被赋值的字段类型',
  field_name varchar(255) NOT NULL COMMENT '枚举类构造函数被赋值的字段名',
  class_name varchar(300) NOT NULL COMMENT '枚举类完整类名',
  full_method text NOT NULL COMMENT '枚举类构造函数完整方法（类名+方法名+参数）',
  PRIMARY KEY (record_id),
  INDEX idx_eiaf_scnas_{appName}(simple_class_name(255), arg_seq)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='枚举类构造函数参数与字段赋值关系表';