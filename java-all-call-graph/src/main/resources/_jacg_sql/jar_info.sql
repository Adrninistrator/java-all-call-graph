CREATE TABLE if not exists jar_info_{appName} (
  jar_num int NOT NULL COMMENT 'Jar包序号',
  jar_type varchar(5) NOT NULL COMMENT 'Jar包类型，jar: jar包，dir: 目录',
  jar_path_hash varchar(30) NOT NULL COMMENT 'Jar包路径HASH+字节数',
  jar_full_path text NOT NULL COMMENT 'Jar包完整路径',
  last_modified varchar(15) NOT NULL COMMENT 'Jar包完整路径',
  jar_hash varchar(30) NOT NULL COMMENT 'Jar包文件HASH',
  PRIMARY KEY (jar_num),
  UNIQUE INDEX idx_ji_jph_{appName}(jar_path_hash)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='jar包信息表';