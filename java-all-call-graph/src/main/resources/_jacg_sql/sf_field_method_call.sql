CREATE TABLE IF NOT EXISTS jacg_sf_field_method_call_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(300) NOT NULL COMMENT '唯一类名',
  field_name varchar(200) NOT NULL COMMENT '字段名',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  call_id int NOT NULL COMMENT '字段初始化对应的方法调用序号，从1开始',
  field_type_nad varchar(255) NOT NULL COMMENT '字段类型（不包含数组标志）',
  array_dimensions tinyint NOT NULL COMMENT '字段数组类型的维度，为0代表不是数组类型',
  class_name varchar(300) NOT NULL COMMENT '完整类名',
  callee_class_name varchar(300) NOT NULL COMMENT '初始化方法被调类名',
  callee_method_name varchar(200) NOT NULL COMMENT '初始化方法被调用方法名',
  PRIMARY KEY (record_id),
  INDEX idx_sffmc_scf_{appName}(simple_class_name(255), field_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='static、final字段初始化方法信息表（含枚举）';