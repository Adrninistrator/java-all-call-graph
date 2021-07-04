# 当前脚本应在向下完整方法调用链文件所在目录执行，以下'\.dao\.'为Mapper所在的包名关键字
array_string="Class1.func1 Class2.func2"

result_dir=result
[ -d $result_dir ] || mkdir $result_dir

array_entry=($array_string)
for i in ${!array_entry[@]}
do 
entry=${array_entry[i]}
class=`echo $entry | awk -F ':' '{print $1}'`
method=`echo $entry | awk -F ':' '{print $2}'`
file_result=$result_dir/"$class"@"$method"_result.txt
count=`ls | grep ^"$class"@ | grep "$method"@ | grep "\.txt" | wc -l`
echo "#" > $file_result
echo $class $method >> $file_result
if [ ! $count -eq 1 ]; then
	echo "find file count is $count"
fi

file=`ls | grep ^"$class"@ | grep "$method"@ | grep "\.txt"`
cat "$file" | grep '\.dao\.' | awk -F '#' '{print $2}' | sed 's# ##g' | awk -F '.' '{print $NF}' | sort | uniq | sort >> $file_result
done

all_result=$result_dir/\~all_result.txt
[ -f $all_result ] && rm $all_result
cat $result_dir/*_result.txt > $all_result