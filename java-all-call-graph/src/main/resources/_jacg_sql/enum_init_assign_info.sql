CREATE TABLE if not exists jacg_enum_init_assign_info_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(255) NOT NULL COMMENT '枚举唯一类名',
  const_name varchar(255) NOT NULL COMMENT '枚举常量名称',
  ordinal int NOT NULL COMMENT '枚举字段序号',
  arg_seq int NOT NULL COMMENT '通过枚举类构造函数被赋值的参数序号（从1开始，最小为3）',
  field_type varchar(255) NOT NULL COMMENT '通过枚举类构造函数被赋值的字段类型',
  field_value text NOT NULL COMMENT '通过枚举类构造函数被赋值的字段值',
  class_name varchar(255) NOT NULL COMMENT '枚举完整类名',
  full_method text NOT NULL COMMENT '枚举类构造函数完整方法（类名+方法名+参数）',
  PRIMARY KEY (record_id),
  INDEX idx_eiai_scnas_{appName}(simple_class_name, arg_seq)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='枚举类初始化赋值信息表';