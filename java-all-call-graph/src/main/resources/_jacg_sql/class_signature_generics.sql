CREATE TABLE if not exists jacg_class_signature_generics_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  seq int NOT NULL COMMENT '序号，从0开始',
  generics_name varchar(255) NOT NULL COMMENT '签名中的泛型名称',
  generics_extends_class_name varchar(255) NOT NULL COMMENT '签名中的泛型的父类类名',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  PRIMARY KEY (record_id),
  INDEX idx_csg_scn_{appName}(simple_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='类的签名中的泛型信息';