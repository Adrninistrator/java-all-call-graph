CREATE TABLE if not exists jacg_set_method_assign_info_{appName} (
  set_record_id int NOT NULL COMMENT 'set方法记录id，从1开始',
  set_method_call_id int NOT NULL COMMENT 'set方法被调用时的方法调用序号，从1开始',
  seq int NOT NULL COMMENT 'set方法当前被调用时被赋值情况的序号，从0开始',
  step int NOT NULL COMMENT 'set方法当前被调用时被赋值时通过方法调用传递的步骤，从0开始',
  fld_relationship_id int DEFAULT NULL COMMENT '字段关联关系id，从1开始',
  curr_call_id int NOT NULL COMMENT '当前的方法调用序号，从1开始',
  caller_method_hash varchar(32) NOT NULL COMMENT '调用方，方法hash+字节数',
  caller_full_method text NOT NULL COMMENT '调用方，完整方法（类名+方法名+参数）',
  caller_line_number int NOT NULL COMMENT '调用方法源代码行号',
  callee_full_method text NOT NULL COMMENT '被调用方，完整方法（类名+方法名+参数）',
  set_method_hash varchar(32) NOT NULL COMMENT 'set方法hash+字节数',
  set_full_method text NOT NULL COMMENT 'set方法完整方法（类名+方法名+参数）',
  set_method_in_super tinyint NOT NULL COMMENT 'set方法是否在超类中，1:是，0:否',
  flag varchar(20) NOT NULL COMMENT 'set方法被调用时的赋值情况标志，见 SetMethodAssignFlagEnum 类',
  flag_desc varchar(50) NOT NULL COMMENT 'set方法被调用时的赋值情况标志描述',
  assign_info text NOT NULL COMMENT 'set方法被调用时的赋值信息',
  equivalent_conversion tinyint NOT NULL COMMENT '是否属于等值转换前的数据，1:是，0:否',
  PRIMARY KEY (set_record_id, set_method_call_id, seq, step),
  INDEX idx_smai_rmh_{appName}(caller_method_hash),
  INDEX idx_smai_smh_{appName}(set_method_hash)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='dto的set方法被调用时的赋值信息';