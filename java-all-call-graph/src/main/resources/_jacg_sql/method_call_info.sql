CREATE TABLE if not exists jacg_method_call_info_{appName} (
  call_id int NOT NULL COMMENT '调用序号',
  obj_args_seq int NOT NULL COMMENT '被调用对象或参数序号，0代表被调用对象，1开始为参数',
  type varchar(5) NOT NULL COMMENT '类型，t:类型，v:值，bv:BASE64编码后的值，sf:静态字段，sfm:静态字段的方法',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  the_value text NOT NULL COMMENT '对应的值',
  PRIMARY KEY (call_id, obj_args_seq, type, seq)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法调用信息表';