CREATE TABLE if not exists jacg_method_call_info_{appName} (
  call_id int NOT NULL COMMENT '方法调用序号',
  obj_args_seq int NOT NULL COMMENT '被调用对象或参数序号，0代表被调用对象，1开始为参数',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  type varchar(5) NOT NULL COMMENT '类型，t:类型，v:值，bv:BASE64编码后的值，sf:静态字段，sfm:静态字段的方法',
  array_flag int NOT NULL COMMENT '是否为数组格式，1:是，0:否',
  the_value text COLLATE utf8mb4_bin NOT NULL COMMENT '对应的值',
  PRIMARY KEY (call_id, obj_args_seq, seq, type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法调用信息表';