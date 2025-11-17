CREATE TABLE IF NOT EXISTS jacg_method_finally_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  method_hash varchar(32) NOT NULL COMMENT '方法hash+字节数',
  simple_class_name varchar(300) NOT NULL COMMENT '唯一类名',
  try_catch varchar(7)  NOT NULL COMMENT '当前的finally对应try或catch',
  try_catch_start_line_number int NOT NULL COMMENT 'try或catch代码块开始代码行号',
  try_catch_end_line_number int NOT NULL COMMENT 'try或catch代码块结束代码行号',
  try_catch_min_call_id int NOT NULL COMMENT 'try或catch代码块最小方法调用ID',
  try_catch_max_call_id int NOT NULL COMMENT 'try或catch代码块最大方法调用ID',
  finally_start_line_number int NOT NULL COMMENT 'finally代码块开始代码行号',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  return_type varchar(255) NOT NULL COMMENT '方法返回类型，包含数组标志',
  PRIMARY KEY (record_id),
  INDEX idx_mf_mh_{appName}(method_hash),
  INDEX idx_mf_scn_{appName}(simple_class_name(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法的finally信息';