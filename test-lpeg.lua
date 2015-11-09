require 'lpeg'

match=lpeg.match
P = lpeg.P
S = lpeg.S
R = lpeg.R

C = lpeg.C
Ct= lpeg.Ct

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
local int = S'+-'^-1 * digits

local bool=P'true'+P'false'

local time=digits*(P's'+P'm')

local var = '$'*iden

local Q = P'"'

local string =  Q * (1-Q)^0 * Q

local fun_decl = P'::'*space(C(fun_name))*S'\r\n'^1*C(P(1)^0)


local assignmentOperator= P'='+P'+='+P'-='

local operatorComparison = P'is'+P'gte'
local operatorAddSub = P'+'+P'-'
local operatorAndAnd = P'and'
local operatorOrOr = P'or'

local Or='|'

local b_operator = operatorComparison+operatorAddSub+operatorAndAnd+operatorOrOr


local exp_v = int+var+bool+time+string

local exp_op = space(exp_v)*b_operator*space(exp_v)

local exp = exp_op+exp_v

local set_st = '<<set '*space(var)*'='*space(exp)*'>>'

print(C(set_st):match '<<set $x = $y+1>>')



--print(C(exp_op):match ' 1 + 1 ')
print(C(string):match '""')

print(C(fun_decl):match [==[
:: launch
[正在建立连接]
[正在接收消息]
喂喂？
这玩意儿能用吗？
有谁能收到吗
<<choice [[谁在说话？|whois]]>> | <<choice [[我收到了。|message received]]>>
]==])























