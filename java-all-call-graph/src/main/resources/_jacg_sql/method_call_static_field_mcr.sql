CREATE TABLE IF NOT EXISTS jacg_method_call_static_field_mcr_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  call_id int NOT NULL COMMENT '方法调用序号，从1开始',
  obj_args_seq int NOT NULL COMMENT '被调用对象或参数序号，0代表被调用对象，1开始为参数',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  caller_method_hash varchar(32) NOT NULL COMMENT '调用方，方法hash+字节数',
  simple_class_name varchar(300) NOT NULL COMMENT '静态字段所在类唯一类名',
  field_name varchar(200) NOT NULL COMMENT '静态字段名称',
  simple_field_type varchar(255) NOT NULL COMMENT '静态字段类型唯一类名',
  class_name varchar(300) NOT NULL COMMENT '静态字段所在类完整类名',
  field_type varchar(255) NOT NULL COMMENT '静态字段类型',
  callee_method_hash varchar(32) COLLATE utf8_bin NOT NULL COMMENT '被调用方（静态字段所在类），方法hash+字节数',
  callee_method_name varchar(300) COLLATE utf8_bin NOT NULL COMMENT '被调用方（静态字段所在类），方法名',
  callee_full_method text COLLATE utf8_bin NOT NULL COMMENT '被调用方（静态字段所在类），完整方法（类名+方法名+参数）',
  callee_return_type varchar(255) NOT NULL COMMENT '被调用方（静态字段所在类），方法返回类型，包含数组标志',
  PRIMARY KEY (record_id),
  INDEX idx_mcsfmcr_scnfn_{appName}(simple_class_name(255), field_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法调用（的被调用对象或参数）中使用静态字段的方法调用返回值信息表';