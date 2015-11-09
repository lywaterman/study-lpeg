require 'lpeg'

require 'ml'.import()

tostring=tstring

function_list = {}

match=lpeg.match
P = lpeg.P
S = lpeg.S
R = lpeg.R

C = lpeg.C
Ct= lpeg.Ct
Cg = lpeg.Cg

test = [===[:: Start
<<silently>>
<<set $toldname = 0>>
<<set $boulderdayone = 0>>
<<set $deadenddayone = 0>>
<<set $hurtshoulder = 0>>
<<set $mapsuggest = 0>>
<<set $toldstudent = 0>>
<<set $backofship = 0>>
<<set $frontofship = 0>>
<<set $rations = 0>>
<<set $ratpellets = 0>>
<<set $startedburialtalk = 0>>
<<set $capalive = 1>>
<<set $ginny = 0>>
<<set $power = "none">>
<<set $capburied = 0>>
<<set $crewburied = 0>>
<<set $hurtankle = 0>>
<<set $pills = 0>>
<<set $glowrods = 0>>
<<set $ginnycaravel = 0>>
<<set $zombierats = 0>>
<<set $compassweird = 0>>
<<set $bringginnytwo = 0>>
<<set $proximityalarm = 0>>
<<set $trycaravelgalley = 0>>
<<set $homemadecompass = 0>>
<<set $triedgalley = 0>>
<<set $peakdoorway = 0>>
<<set $sendingsos = 0>>
<<set $warnship = 0>>
<<set $overridetarget = 0>>
<<set $clockwisecrater = 0>>
<<set $plural = "none">>
<<set $testrods = 10>>
<<endsilently>>
#[通讯正在接入]#
[[launch]]



:: launch
#[正在建立连接]
[正在接收消息]
喂喂？
这玩意儿能用吗？
有谁能收到吗？#
<<choice [[#谁在说话？#|whois]]>> | <<choice [[#我收到了。#|message received]]>>

]===]

local sp=P' '^0
local function space(pat) return sp*pat*sp end

local idenchar = R('AZ', 'az')+'_'
local iden = idenchar * (idenchar+R'09')^0

local fun_name = idenchar * (sp*idenchar+R'09')^0

local digit = R'09'
local digits= digit^1
local cdigits = C(digits)
local int = S'+-'^-1 * digits / tonumber

local function to_bool (b) 
    if b == 'true' then 
        return true 
    elseif b == 'false' then 
        return false 
    end
end 
local bool=(P'true'+P'false') / to_bool

local function to_second(t,s) 
    if s == 's' then
        return t
    elseif s == 'm' then
        return t * 60 
    end
end

local time=int*C(P's'+P'm') / to_second

local var = '$'*iden

local Q = P'"'

local string =  Q * (1-Q)^0 * Q

local fun_decl = P'::'*space(C(fun_name))*S'\r\n'^1*C((P(1)-':')^0)


local assignmentOperator= P'='+P'+='+P'-='

local operatorComparison = P'is'+P'gte'
local operatorAddSub = P'+'+P'-'
local operatorAndAnd = P'and'
local operatorOrOr = P'or'

local Or='|'

local new_line = S'\r\n'^0

local new_line_sp = S' \r\n'^0

local function new_line_space(pat) return new_line_sp*pat*new_line_sp end

local fun_decl_list = (new_line_space(fun_decl))^1

local b_operator = operatorComparison+operatorAddSub+operatorAndAnd+operatorOrOr

local function any_text_except(except) return (P(1)-except)^1 end

local exp_v = int+var+bool+time+string

local exp_op = space(exp_v)*b_operator*space(exp_v)

local exp = exp_op+exp_v

local set_st = P'<<set '*space(var)*'='*space(exp)*P'>>'

local then_st = '|'*fun_name

local call_function = '[['*space(fun_name)*']]' + ('[['*space(P'delay')*time*space(then_st)*']]') + ('[['*any_text_except(S'|]')*then_st*']]')

local choice_st = P'<<choice '*space(call_function)*P'>>'

local select_st = choice_st * space(Or) * choice_st

local silently_st = new_line_space(P'<<silently>>') *(new_line_space(set_st))^0* new_line_space(P'<<endsilently>>')

local if_st = P'<<if '*space(exp)*P'>>' * new_line_space(any_text_except(S'<')) * new_line_space(P'<<elseif '*space(exp)*P'>>' * new_line_space(any_text_except(S'<')))^0 * new_line_space(P'<<endif>>') 

--print(C(if_st):match [==[<<if $x is 1>>你说的福建省地方
--
--<<elseif $x is 2>>收到了附近的考虑是放假了第三方<<endif>>]==])

--print(C(set_st):match '<<set $x = $y+1>>')
--
--print(C(silently_st):match [==[
--    <<silently>><<set $toldname = 0>>
--    <<set $boulderdayone = 0>>
--    <<set $deadenddayone = 0>>
--    <<set $hurtshoulder = 0>>
--    <<set $mapsuggest = 0>>
--    <<set $toldstudent = 0>>
--    <<set $backofship = 0>>
--    <<set $frontofship = 0>>
--    <<set $rations = 0>>
--    <<set $ratpellets = 0>>
--    <<set $startedburialtalk = 0>>
--    <<set $capalive = 1>>
--    <<set $ginny = 0>>
--    <<set $power = "none">>
--    <<set $capburied = 0>>
--    <<set $crewburied = 0>>
--    <<set $hurtankle = 0>>
--    <<set $pills = 0>>
--    <<set $glowrods = 0>>
--    <<set $ginnycaravel = 0>>
--    <<set $zombierats = 0>>
--    <<set $compassweird = 0>>
--    <<set $bringginnytwo = 0>>
--    <<set $proximityalarm = 0>>
--    <<set $trycaravelgalley = 0>>
--    <<set $homemadecompass = 0>>
--    <<set $triedgalley = 0>>
--    <<set $peakdoorway = 0>>
--    <<set $sendingsos = 0>>
--    <<set $warnship = 0>>
--    <<set $overridetarget = 0>>
--    <<set $clockwisecrater = 0>>
--    <<set $plural = "none">>
--    <<set $testrods = 10>>
--    <<endsilently>>
--   ]==]
--)

--print(C(exp_op):match ' 1 + 1 ')
--print(C(string):match '""')
--
print(Ct(fun_decl_list):match(test))

print(Ct(time):match "1m")


function do_function_list() 

end




















