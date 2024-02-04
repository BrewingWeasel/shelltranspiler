




















_len() {
local __n_timecalled=$1
shift


__return_val='$'"$1"'_len'
eval "___len_return_value_$__n_timecalled=\"$__return_val\""
return 0
}
_push() {
local __n_timecalled=$1
shift

len="$1"'_len'


_len 0 "$1"

'eval' "$1"'_'$___len_return_value_0'='"$2" 



'eval' "$len"'=$(('"$len"' + 1))' 
}
_add_default() {
local __n_timecalled=$1
shift
local b=1
while test $# -gt 0; do
case "$1" in
--b)
shift
export b="$1"
shift
;;
*)
break
;;
esac
done

__return_val=$(("$1" + "$b"))
eval "___add_default_return_value_$__n_timecalled=\"$__return_val\""
return 0
}
_get_first() {
local __n_timecalled=$1
shift
__return_val="$(eval "echo \"\$$(echo "$1")_$(echo "1")\"")"
eval "___get_first_return_value_$__n_timecalled=\"$__return_val\""
return 0
}
_add() {
local __n_timecalled=$1
shift

__return_val=$(("$1" + "$2"))
eval "___add_return_value_$__n_timecalled=\"$__return_val\""
return 0
}
_input() {
local __n_timecalled=$1
shift

'read' '-p' "$1" '__readvar' 

__return_val='$__readvar' 
eval "___input_return_value_$__n_timecalled=\"$__return_val\""
return 0
}
term_cursor_up() {
local __n_timecalled=$1
shift

echo -e '\033['"$1"'A'
}
term_yellow() {
local __n_timecalled=$1
shift

__return_val='\033[33m'"$1"
eval "__term_yellow_return_value_$__n_timecalled=\"$__return_val\""
return 0
}







__list_1_len=3
__list_1_0='apple'
__list_1_1='banana'
__list_1_2='orange'

fruits="__list_1"
_push 0 "$fruits" 'grape'
__index_1=0; while eval "fruit=\"\${${fruits}_${__index_1}}\"; [ $__index_1 -lt \"\$${fruits}_len\" ]"; do

echo -e ''"$fruit"''

__index_1=$((__index_1 + 1))
done


_add 0 1 2
sum=$___add_return_value_0



_add_default 0 1
value=$___add_default_return_value_0


echo -e 'value is '"$value"''
_add_default 1 --b 2 1
value=$___add_default_return_value_1


echo -e 'value is '"$value"''


__list_2_len=1
__list_2_0=1

_get_first 0 "__list_2"
first_num=$___get_first_return_value_0

__list_3_len=1
__list_3_0='one'

_get_first 1 "__list_3"
first_number=$___get_first_return_value_1




Option_vals_0_0=1
Option_vals_0_v=Some

val=Option_vals_0

Option_vals_1_v=None

other_val=Option_vals_1
if true && eval "[ $"$val"_v = Some ]"; then
eval "value=$"$val"_0"



echo -e 'the value is '"$value"''
fi
if true && eval "[ $"$other_val"_v = Some ]"; then
eval "value=$"$other_val"_0"

echo -e 'this will never be printed'
fi

cat='kitty'

output=$('echo' 'the value is' "$cat" )

echo -e 'output: '"$output"''
str_val=5

term_yellow 0 'yellow'

echo -e 'the color is '$__term_yellow_return_value_0''
term_cursor_up 0 1

_input 0 'what is your name? '

echo -e 'Hi there '$___input_return_value_0''
