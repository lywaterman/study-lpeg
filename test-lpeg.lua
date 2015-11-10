require 'lpeg'

require 'ml'.import()

require 'storydata_cn'

x=1
tostring=tstring

function_list = {}

match=lpeg.match
P = lpeg.P
S = lpeg.S
R = lpeg.R

C = lpeg.C
Ct= lpeg.Ct
Cg = lpeg.Cg
Cf = lpeg.Cf

V = lpeg.V



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

local var = '$'*C(iden)

local Q = P'"'

local string =  Q * (1-Q)^0 * Q



local assignmentOperator= P'='+P'+='+P'-='

local operatorComparison = P'is'+P'gte'
local operatorAddSub = P'+'+P'-'
local operatorAndAnd = P'and'
local operatorOrOr = P'or'

local Or='|'

local new_line = S'\r\n'^0

local new_line_sp = S' \r\n'^0

local function new_line_space(pat) return new_line_sp*pat*new_line_sp end

local b_operator = operatorComparison+operatorAddSub+operatorAndAnd+operatorOrOr

local function any_text_except(except) return (P(1)-except)^1 end

local exp_v = int+var+bool+time+string
-------------------------------------------------
local function to_lua_exp(ev1, os, ev2) 
    if ev2 == nil then
        return ev1
    end 
    if os == 'is' then
        os = '=='
    elseif os == 'gte' then
        os = '>='
    end

    return ev1 ..' '.. os ..' ' .. ev2
end
local exp_op = (space(exp_v)*C(b_operator)*space(exp_v)) / to_lua_exp
---------------------------------------------------
local exp_op_op = space(exp_op+exp_v)*(C(b_operator)*space(exp_op+exp_v))^0 / to_lua_exp

local exp = exp_op_op + exp_v

--------set 表达式 set var
local function set_var(name, value)
    return {type='set_st', value=name .. '=' .. tostring(value)}
end

local set_st = P'<<set '*space(var)*'='*space(exp)*P'>>' / set_var
---------------------------------

local then_st = '|'*C(fun_name)

local function return_function_1(fun_name) 
    print('fun_name', fun_name)
    return {
        type='call_fun',
        fun=function ()
            do_function(fun_name)
    end}
end

local function return_function_2(time, fun_name)
    print('delay fun_name', time, fun_name)
    
    return {
        type='call_delay_fun',
        delay=time,
        fun=function ()
            do_function(fun_name)
        end 
    }
end

local function return_function_3(text, fun_name)
    print('choice fun_name', text, fun_name)
    
    return {
        type='call_choice_fun',
        text=text,
        fun=function ()
            do_function(fun_name)
        end 
    }
end

local function return_function_4(text)
    print('no fun_name', text)
    
    return {
        type='call_no_name_fun',
        fun=function ()
            show(text)
        end 
    }
end

local call_function = '[['*space(C(fun_name))*']]'/return_function_1 + ('[['*space(P'delay')*time*space(then_st)*']]')/return_function_2 + ('[['*C(any_text_except(S'|]'))*then_st*']]')/return_function_3+('['*C(any_text_except(S']'))*']')/return_function_4

local choice_st = P'<<choice '*space(call_function)*P'>>'

local function return_select_st(x) 
    return {type='select_st', value=x}
end
local select_st = Ct(choice_st * space(Or) * choice_st) / return_select_st

local silently_st = new_line_space(P'<<silently>>') *Ct((new_line_space(set_st))^0)* new_line_space(P'<<endsilently>>')

local function any_text_f(text)
    return {type='text_st', value=text}
end

local st_no_if = silently_st + select_st + call_function + (any_text_except(S'<[')/any_text_f)

local function do_if_st(t)
    return {type='if_st', value=t}
end

local ST_LIST = P{
    'ST';
    ST = (new_line_space(lpeg.V"if_st"+st_no_if)),
    if_st =  Ct(P'<<if ' * Cg(space(exp), 'if_exp') * P'>>' * new_line_space(Cg(V"ST", 'if_sts'))^1 *
            new_line_space(P'<<elseif '* Cg(space(exp), 'elseif_exp') * P'>>' * new_line_space(Cg(V"ST", 'elseif_sts'))^1)^0 * 
            new_line_space(P'<<else>>'                      * new_line_space(Cg(V"ST", 'else_sts'))^1)^-1 * 
            new_line_space(P'<<endif>>')) / do_if_st
} ^ 1

local fun_decl = Cg(P'::'*space(C(fun_name))*S'\r\n'^1*C((P(1)-':')^0))

local fun_decl_list = Cf(Ct""*(new_line_space(fun_decl))^1, rawset)

local function_p = ST_LIST 

print(ST_LIST:match [==[
<<silently>><<set $ratpellets = 1>><<endsilently>>
<<if $rations is 0>>还有个（有点）好的消息。我不会饿死在这里了。我找到了老鼠饲料，以及一个半满的水瓶。
瞧瞧。我们这叫“半满”呢。
简直是太乐观了。
所以……我该知足了，就靠老鼠饲料度日子了……
……或者，还是把这当成最好永远用不到的后备计划吧。我该去找点人吃的。
<<if $triedgalley is 0>>
<<choice [[人类的食物！试试厨房吧。|trythegalley]]>> | <<choice [[就吃老鼠饲料吧。|eatratfood]]>>
<<elseif $triedgalley is 1>>
<<choice [[再去试试厨房吧。|tryinggalleydoor]]>> | <<choice [[就吃老鼠饲料吧。|eatratfood]]>><<endif>>

<<elseif $rations is 1>>嘿，除了口粮，我还找到了万不得已能当饭吃的老鼠饲料，甚至一个半满的水瓶呢。
瞧瞧。我们这叫“半满”呢。
简直是太乐观了。
总之，现在天色已晚。我最好想好怎么睡觉的问题。
[[sleepingplans]]<<endif>>]==])



--print(C(exp_op):match [==[ $crewburied is 0 and $capalive is 0]==])
--print(C(if_st):match [==[<<if $crewburied is 0 and $capalive is 0>>我要接着去挖墓，埋葬船长和船员们，我想这是一件善事吧。
--至少要正式地告别一下，是吧？
--我希望为他们每个人讲一段告别辞。我干活时想独处一会儿。
--等下再联系你。[[delay 60m|everybodyburied]]
--<<elseif $crewburied is 0 and $capalive is 1>>我要去接着挖墓坑，埋葬船员们，我想这是一件善事吧。
--至少要正式地告别一下，是吧？
--我希望为他们每个人讲一段告别辞。我干活时想独处一会儿。
--等下再联系你。[[delay 60m|crewburied]]
--<<elseif $crewburied is 1 and $capalive is 0>>再挖一个墓坑应该不会花太多时间。今天我已经挖了好多次，很熟练了。
--我要把阿雅船长和船员们埋在一起。我想她会喜欢这样的。
--过会儿再联系你。[[delay 30m|everybodyburied]]<<endif>>]==])


--print(C(if_st):match [==[<<if $x is 1>>你说的福建省地方
--<<silently>><<set $toldname = 0>>
--<<set $boulderdayone = 0>>
--<<endsilently>><<elseif $x is 2>>收到了附近的考虑是放假了第三方
--<<elseif $x is 3>>收到了附近的考虑[[tttt]]
--<<else>>fuckyou<<endif>>]==])

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

--print(Ct(time):match "1m")

local open = io.open

local function read_file(path)
    local file = open(path, "r") -- r read mode and b binary mode
    if not file then return nil end
    local content = file:read "*a" -- *a or *all reads the whole file
    file:close()
    return content
end

local fileContent = read_file("StoryData_cn.txt");

--执行方法
function do_function(fun_name)
    
    print(function_p:match(function_list[fun_name]))
end

function delay_do_function(fun_name) 

end

function do_function_list(data) 
   function_list = (fun_decl_list):match(data)
    
   do_function('trythelab')
end

--do_function_list(all_data)

--print(select_st:match "<<choice [[谁在说话？|whois]]>> | <<choice [[我收到了。|message received]]>>")

--set_st:match "<<set $y=1+3>>"

--print(exp_op:match "1+2")

--print(y)















