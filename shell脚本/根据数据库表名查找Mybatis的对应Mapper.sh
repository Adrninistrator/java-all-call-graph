# 执行当前脚本前，需要将对应应用的jar/war包解压，以下find_dir参数为保存Mybatis的XML文件的目录
array_string="table1 table2"

find_dir=/test/dir1/

array_table=($array_string)
for i in ${!array_table[@]}
do 
table=${array_table[i]}
xml_file=`find $find_dir -name \*.xml | xargs grep "insert into $table " | head -1 | awk -F ':' '{print $1}'`
if [ "$xml_file" == "" ]; then
xml_file=`find $find_dir -name \*.xml | xargs grep "insert into $table" | head -1 | awk -F ':' '{print $1}'`
fi

mapper=`grep 'mapper namespace' $xml_file | awk -F '"' '{print $2}' | awk -F '.' '{print $NF}'`

echo $table $mapper
done
