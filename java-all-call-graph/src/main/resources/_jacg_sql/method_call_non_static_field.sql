CREATE TABLE if not exists jacg_method_call_non_static_field_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  call_id int NOT NULL COMMENT '方法调用序号，从1开始',
  obj_args_seq int NOT NULL COMMENT '被调用对象或参数序号，0代表被调用对象，1开始为参数',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  caller_method_hash varchar(32) NOT NULL COMMENT '调用方，方法hash+字节数',
  simple_class_name varchar(255) NOT NULL COMMENT '非静态字段所在类唯一类名',
  field_name varchar(200) NOT NULL COMMENT '非静态字段名称',
  simple_field_type varchar(255) NOT NULL COMMENT '非静态字段类型唯一类名',
  class_name varchar(255) NOT NULL COMMENT '非静态字段所在类完整类名',
  field_type varchar(255) NOT NULL COMMENT '非静态字段类型',
  PRIMARY KEY (record_id),
  INDEX idx_mcnsf_scnfn_{appName}(simple_class_name, field_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法调用使用非静态字段信息表';