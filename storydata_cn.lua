﻿all_data=[====[:: Start
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
[通讯正在接入]
[[launch]]



:: launch
[正在建立连接]
[正在接收消息]
喂喂？
这玩意儿能用吗？
有谁能收到吗？
<<choice [[谁在说话？|whois]]>> | <<choice [[我收到了。|message received]]>>


:: whois
<<silently>><<set $toldname = 1>><<endsilently>>
哎呀呀呀，不好意思。我应该先自我介绍一下的。
我只是收到回复太激动了。
我叫泰勒。我是，原本是瓦里亚号星际飞船上的一名宇航员。
<<choice [[瓦里亚号？|Variainfo]]>> | <<choice [[“原本”？出什么事了？|whathappened]]>>


:: Variainfo
瓦里亚号是艘运输船，没什么特别的，船员还不到一打。
我们本来在去天仓四的路上。我记得一切都挺顺利！
再有六天我们就能到港了。
但是后来……我不知道出了什么事，我们坠毁到了个什么卫星上。
我也不清楚这到底是哪儿。
<<choice [[你怎么会不知道？|dontknow]]>> | <<choice [[你可以用下地图呀。|usemap]]>>


:: dontknow
<<silently>><<set $toldstudent = 1>><<endsilently>>
<<if $mapsuggest is 1>>不知道，我真不知道。<<endif>>
我怎么可能不知道？！我怎么不像那些了不起的宇航员一样，把整个星区地图都背了下来？！
因为我是个学生！懂吗？
我在一场理科生抽奖活动里中了奖，所以才参加了这次远航。
在瓦里亚号上对老鼠和地衣什么的进行零重力实验，懂了吧？
我身边必须随时有一名有经验的导师陪同。
这么说吧，我的学员手册上从来就没有提过什么紧急迫降的事儿！
<<choice [[好了好了，冷静点。|calmdown]]>> | <<choice [[哇喔，放轻松。|whoatakeiteasy]]>>


:: calmdown
哎，对不起。你也看出来了，我现在是整个人都不好了！
我们针对各种情况进行了训练，但是现在的情况绝不是其中之一！
不过没事儿。没问题。我只要找到别的生还者就行了，他们会知道怎么处理的。
除非……
[[delay 4s|nosurvivors]]


:: nosurvivors
要是没有别的生还者了怎么办？
要是只有我活着怎么办？
噢，天哪，阿雅船长可能已经死了，其他人恐怕也没活下来。科尔比……糟糕！科尔比。
要是……要是这里真的只剩我一个人怎么办？
<<choice [[放轻松，深呼吸。|justbreathe]]>> | <<choice [[想聊聊科尔比吗？|talkColby]]>>


:: justbreathe
哎，你说得对。这里的空气能呼吸，干脆直接呼吸吧。
科尔比经常这么说：
“与其杞人忧天，不如专注当下。”
对，就直接呼吸吧。
<<choice [[好了，那么你现在在哪儿？|whereareyou]]>> | <<choice [[你受伤了吗？|areyouallright]]>>


:: sorryaboutearlier
没事儿，这么紧急的状况下，一般人说的话肯定辞不达意。
真高兴找到人帮我了。要是没有你，我都不知道该怎么办。
好了，先看看哪些是我们能处理的吧。
<<choice [[你现在在哪儿？|whereareyou]]>> | <<choice [[你受伤了吗？|areyouallright]]>>


:: message received
哦，谢天谢地！能联系到人，真是太好啊！
我等了好几个钟头了！
<<choice [[你是谁？|whois]]>> | <<choice [[出了什么事？|whathappened]]>>


:: whathappened
我们的飞船在个什么卫星上坠毁了，我也不知道具体是在哪儿。
我进了一个逃生舱，不过我不知道有没有别人也通过逃生舱逃生。
我的宇航服上的传感器说这里的大气可供呼吸，这简直是个奇迹。
只是，我身上既没吃的也没喝的。逃生舱里有个工具箱，里面都是些最普通的工具。
我想办法启用了逃生舱内的便携通讯器，但是——除了走运的你，好像没人收到我发出的信号。
我也不知道你是谁，在哪里。
<<choice [[你还好吗？|areyouallright]]>> | <<choice [[你现在在哪？|whereareyou]]>>


:: StoryTitle
Lifeline 041115


:: usemap
<<silently>><<set $mapsuggest = 1>><<endsilently>>
是啊，你想得倒挺美，就是有个小问题，那就是我，还有我的救生舱，在这里……
……可是导航室还在瓦里亚号里面呢。
而瓦里亚号呢，从南边那两道袅袅的黑烟来看，至少断成了两截，而且离我可不近。
所以，你要理解，“用下地图”可没那么容易。
<<choice [[你真的一点头绪都没有？|dontknow]]>> | <<choice [[对不起，你没事儿吧？|areyouallright]]>>


:: whoatakeiteasy
“放轻松”，说得倒轻巧！
我流落外太空，孤苦伶仃——说句心里话，我整个人都不好了！
<<choice [[一切都会好的，冷静点。|calmdown]]>> | <<choice [[别耍小孩子脾气。|dontbeababy]]>>


:: dontbeababy
“别耍小孩子脾气”？
哦，看来对你来说被困在荒凉星球上这种事倒是天天发生啊？
有没有人跟你说过，接听危机热线这活儿你可不怎么在行？
没跟你搭上话时，我感觉都要好点儿。
算了……我要去歇口气，评估一下状况。
等我想聊了，再联系你。
[[delay 30m|readytotalk]]


:: readytotalk
好吧，听着。我实在不想承认你哪怕有那么一点点对。
我可不想在这里唉声叹气，坐以待毙。我得采取行动了。
只是，请你……别忘了我真的吓得要屁滚尿流了，好吧？
我真的很庆幸能联系上你，真的，就是………请对我多包涵点，行吗？
<<choice [[刚才真不好意思。|sorryaboutearlier]]>> | <<choice [[你脸皮厚点就行了。|thickerskin]]>>


:: thickerskin
魂淡啊，你简直就是虎妈狼爸。
听着，我会将这个频道一直开着……
……不过这只是因为有你挂在线那头，总好过我对着空气自言自语。
不管你能帮什么忙都行。
<<choice [[你知道你在哪吗？|whereareyou]]>> | <<choice [[你没缺胳膊少腿吧？|areyouallright]]>>


:: whereareyou
对你这个问题，我唯一确定的答案是：“我被困了。”
要是你知道蛮荒在哪里的话，就朝那中间最荒的地方瞧，你就能找到我了。
<<if $toldname is 0>>顺便说一句，我叫泰勒。我该一上来就自我介绍一下的。<<endif>>
瓦里亚号本来沿着轻缓的椭圆形轨道前往天仓四去。
我不知道我们是不是偏离了轨道，或者说偏离了多少。
出事时，我离舰桥还远着呢。
科尔比，她……她一把将我推进了一个救生舱，然后转身跑去帮其他人去了。她就是这么个人。
不知道是震的，还是吓的，我在救生舱里晕了过去……我不知道有多久…… 
等我醒过来，我就在这儿了。
<<choice [[描述一下你说的“这儿”。|describehere]]>> | <<choice [[想谈谈科尔比吗？|talkColby]]>>


:: StoryAuthor
Dave Justus


:: areyouallright
让我瞧瞧。我们的飞船坠毁在这鸟不生蛋的地方。
这个频段上没人理我，放眼望去，也看不到别的幸存者……
……史上最没准备的船员居然遇到这种紧急状况！
不过，除了撞痛了的脚趾头，我在这场大难里居然毫发未伤，所以还算好啦。 
谢谢你关心啦。
<<choice [[希望你的脚趾头没事。别耍嘴皮子了。|sorrytoe]]>> | <<choice [[“最没准备的”？|leastprepared]]>>


:: leastprepared
<<if $toldstudent is 0>>
因为……我是个学生，好吧？
我不是宇航员。我在一场理科生抽奖活动里中了奖，所以才参加了这次远航。
在瓦里亚号上对老鼠和地衣什么的进行零重力实验。
我身边必须随时有一名有经验的导师陪同。
这么说吧，我的学员手册上从来就没有提过什么紧急迫降的事儿！<<endif>>
我是说，科学课的考试里绝对不会出这种题！我都在提前上大学的课了也没见过这种题！
<<choice [[哦哦，学霸！|smartkid]]>> | <<choice [[好了好了，你现在在哪里？|whereareyou]]>>


:: talkColby
哦，好吧，聊聊她也好。
科尔比……她在瓦里亚号上像是大家的妈妈。
她心肠最好，而且，我觉得，她也最聪明。
而且不是那种自鸣得意的聪明，而是，你懂的，要是什么东西坏了，她都能修好。
（没错，就算是在价值几十亿的星际飞船上，大部分时候修东西还是靠胶带！）
但科尔比最体贴的是她的聪明不会让别人觉得自己蠢，我真的很感激她这一点。
虽然希望渺茫，但我真的希望科尔比能躲过这一劫。
当然，我的意思是，我希望大家都能平安无事，但我特别希望她能挺过来。
<<choice [[谢谢你跟我分享这些。|thanksforsharing]]>> | <<choice [[跟我描述一下你现在所处的地方。|describehere]]>>


:: thanksforsharing
哦，不，该说谢谢的是我，谢谢你跟我聊天。
感觉像在过正常的生活一样，跟新朋友聊老朋友。
嘿，如果我能脱困的话，也许我们能一起去喝个咖啡什么的，我请客。
或者，见鬼，我要是能活着回去，那我一定要喝点烈的，咖啡太弱了！
<<choice [[听起来不错！|coffeesoundsgood]]>> | <<choice [[跟我描述一下你现在所处的地方。|describehere]]>>


:: sorrytoe
哈，嗯，好吧。大家都说我爱耍嘴皮子，现在，我除了耍嘴皮子，没别的能耐了。
要是外星小绿人突然跳出来攻击我的话……
……我就指望他们一遇到我的尖牙利齿和巨大白眼就抱头鼠窜了。
<<if $toldname is 0>>哦，嘿，顺便说下，我叫泰勒。我该一上来就自我介绍一下的。不好意思。<<endif>>
不管怎样，不如我给你说说从这儿能看到什么吧。说不定你能给我点建议，该往哪走。
[[describehere]]


:: smartkid
对，没错。撞痛了脚趾头的学霸。
<<choice [[希望你的脚趾头没事，小爱因斯坦。|sorrytoe]]>> | <<choice [[忍着点。你现在在哪里？|whereareyou]]>>


:: deadendmountain
<<silently>><<set $boulderdayone = 1>><<endsilently>>
糟糕！好像路堵死了。
我本来顺着这道小峡谷走的，结果遇到块巨大的石头。实在是太大了，爬都爬不过去。
你怎么想……我该找条路绕过去，还是返回坠毁点？
<<choice [[在附近找找路吧。|wayaround]]>> | <<choice [[返回坠毁点。|backtocrash]]>>


:: movetopeak
好吧，看来我得开始找路了。
嘿，我……唔……我感谢你这个建议。只是在这荒山野岭的，真的得要有点方向感才行。
总之……看起来得费一点时间了。
[[delay 60m|deadendmountain]]


:: gotopeak
你以为啊？那地方比去飞船坠毁点要远得多。
我可保不齐我能在天黑前走到。
<<choice [[那你最好现在就出发吧。|movetopeak]]>> | <<choice [[好吧，去坠毁点看看吧。|gotocrash]]>>


:: describehere
嗯，我的救生舱好像是掉在了什么戈壁里。
地上全是白色的碎石，还有座白色的山峰，在几英里开外。
嗯，我是说，有几公里吧。（他们叫我一路上要使用公制，可是有的东西已经根深蒂固，改不掉了。）
那座山峰左右对称，非常诡异，不太像是天然形成的。
我的宇航服上的指南针显示，山峰在我的东北方向。然后，山峰正对着的方向——确切说是南方和西南方……
有两股黑烟，我猜是瓦里亚号的两截残骸上冒出来的。
也就是说，最理想的状况是，瓦里亚号只！不！过！断成了两截。
坠毁点好像比山峰离我更近些。你觉得我应该怎么做才好？
<<choice [[前往坠毁点。|gotocrash]]>> | <<choice [[前往山峰。|gotopeak]]>>


:: saveyourenergy
当然，当然。不过我早晚得去哪里看看。
总不能一直死守在救生舱旁边吧。
来，我跟你说说我在这能看到啥，也许你可以建议我去哪。
[[describehere]]


:: backatflightdeck
我找到船长了。我觉得，有点怪，我还拿着她的飞行服什么的呢……
……不知道怎么回事，怎么没我想像的重呢……
现在，感觉她好像只是……一个空壳，似乎她的身体被掏空了，什么都没有了。
我打算先用太空毯和胶带做一副担架。这样搬运她最方便。
等我们又回到船员区时，我再跟你联系。
[[delay 30m|backtoburycap]]


:: wayaround
好吧，我去试试。不过这里的路可不好走啊。
[[delay 40m|deadendforreal]]


:: backtocrash
对，我也觉得这样做比较明智。
我希望能那里还剩下了些吃的喝的。
如果可能的话，我真不想饿死在荒漠里头。
总之……看来这段路有得走了。
我打算向南走，我到了后会通知你的。
[[delay 120m|stillhikingcrash]]


:: gotocrash
对，有点道理。那里可能有幸存者……交叉手指祈祷……
（穿着宇航服根本没法交叉两根手指祈祷，但是就当是吧。）
或者最起码能找点用得上的东西吧。
好了，我要向南前进了。看来要走到黑烟至少要花一个钟头呢。换成公制是多长时间，你自己算吧。
我到那儿就通知你。
[[delay 60m|stillhikingcrash]]


:: coffeesoundsgood
呵呵，除了瓦里亚号上的吃的，什么听起来都不错！
说起这个，要不了多久我就需要找食物和水了。
我有个理论，要是我在这里修个十字路口，一定会神奇地出现两家大大的星巴克。
但是我又不想白费一身力气，结果发现我那理论是错的。
<<choice [[保存点体力吧。|saveyourenergy]]>> | <<choice [[描述下你周围的环境。|describehere]]>>


:: justkeepmovingcrewcapdead
哎，你……你说得对。
我要是再死盯着看，不想点办法脱困的话，我成为他们中的一员也就不远了。
天，这是我有过的最变态的想法了。
[[branchingcorridor]]


:: gogetcap
是啊……穿着宇航服，走半个钟头去收尸，再拖着尸体走半个钟头回来，听起来跟度假似的。
我向你保证……没有哪本培训手册教过！
不说了，我出发了。
[[delay 30m|backatflightdeck]]


:: backtoburycap
郁闷地跋涉过一片荒原，我们又回了无人的船员区。
我的生活简直比中学生写的诗还要烂。
我要去再挖些墓坑，埋葬船长和船员们，我想这是一件善事吧。
至少要正式地告别一下，是吧？
我希望为他们每个人讲一段告别辞。我干活时想独处一会儿。
过会儿再联系你。
[[delay 60m|everybodyburied]]


:: keepitforcap
“振作点儿”？！你是认真的吗？
我身上沾满了一个女人的血，无论怎么看，都是我杀了她——顺便一说，是听了你！的！建议。
然后，你就轻描淡写地让我“振作点儿”？
你去死吧。对，这是我！的！建议。
[[delay 15m|goscrewyourself]]


:: goscrewyourself
嘿，唔……刚才不好意思。
我的情绪太过于激动了。
其实，你说的没错。如果我想获救的话，我确实应该振作点儿。
嗯……船长死得太惨了，愿逝者安息。她失了这么多血，也许这样也少受些煎熬。
现在，我！还活着，想想接下来该做什么吧。
<<choice [[你要努力前进。|movingonforcap]]>> | <<choice [[将她永远铭记在心中。|dontforgetcap]]>>


:: dontforgetcap
当然的了，我永远不会忘记她的。
我要好好活下来，才能永远缅怀她。
现在……我有失事信标和炮塔。我还需要能源、食物和水。小事一桩，是吧？
我要去搜刮下船员区的残骸了。我会尽快和你联系的。
[[delay 30m|headingtocrewqs]]


:: deadendforreal
<<silently>><<set $deadenddayone = 1>><<endsilently>>
听着，这石头旁边根本就没有出路。
石头上也没有能脚踩的地方，那怕往上爬几英尺都很难。哦，该说是“几米”，无所谓了。
峡谷壁上的石头看起来都松松垮垮的。
哎……泪奔……科技大赛冠军的我，就是“宅男”的代名词。
我不是攀岩的那块料。
<<choice [[那原路返回吧。|backtocrash]]>> | <<choice [[试试爬一下吧。|rockclimbing]]>>


:: stillhikingcrash
哎哟，吐血。这一路比看起来远多了。我走得两条腿像果冻一样打颤，有没有走到一半都不知道。
[[delay 4s|jello]]


:: refreshed
信不信由你，我打了个盹之后觉得好多了。
我要去接着挖墓坑，埋葬船员们，我想这是一件善事吧。
至少要正式地告别一下，是吧？
我希望为他们每个人讲一段告别辞。我干活时想独处一会儿。
过会儿再联系你。
[[delay 60m|crewburied]]


:: pullitout
好，好主意，我只要
哦！天！啊！血流得更多了！你说，怎么才能子血？
子血？止血？我连话都说不清了！
阿雅船长，求求你，求你别流血了。我要，我要去找纱布来。
我不知道……天啊……天啊。
[[delay 15m|capdidntmakeit]]


:: capdidntmakeit
<<silently>><<set $capalive = 0>><<endsilently>>
她……她去了。
阿雅船长死了。
<<choice [[请节哀。|sorryforcap]]>> | <<choice [[振作点儿！|keepitforcap]]>>


:: sorryforcap
这……这真是糟糕透了。阿雅船长是我在太空飞行指挥中心遇见的第一个人。
你知道吗？她不费吹灰之力就让我觉得我是团队的一份子。
这是她的本事，她懂得如何让大家团结合作，是位非常不了起的领袖。
我……我能够不假思索地将我生命托付给她。
[[delay 4s|capsgone]]


:: capsgone
可是，当她的生命由我来拯救的时候……我却只能眼睁睁地看着她流血不止。
我失去她了。我现在……还能干什么呢？
<<choice [[继续前进。|movingonforcap]]>> | <<choice [[将她永远铭记在心中。|dontforgetcap]]>>


:: movingonforcap
对，你说得对。除了前进没有别的办法了。
保持清醒和活力，不能这样停下来。
现在……我有失事信标和炮塔，我还需要能源、食物和水。小事一桩，是吧？
我要去搜刮下船员区的残骸了。我会尽快和你联系的。
[[delay 30m|headingtocrewqs]]


:: headingtocrewqs
哦，哦，这倒不是完全出乎意料，不过，还是有点……
嗯……我，我找到了几名船员。
是……哦，我不行了！等一下！
[[delay 2m|threwup]]


:: threwup
不好意思，我刚刚跑到石头后面去吐了。
看来瓦里亚号爆炸时，有几名船员正好处在船舱炸裂的地方。
我没法……我没法辨认出他们都是谁，甚至是有几个人。
他们都和金属熔在一起了，或者说，唔，互相熔在一起了。
我觉得，在失压时我们还丧失了几名船员。
[[delay 2s|deadcrew]]


:: deadcrew
在后舱的密封门后……我……我发现了剩下的船员。
安冬、特罗特、科尔比、阿戴尔，都死了。他们都死了……都死了。
我……我做不到……他们都死了，每一个人……而我……
<<choice [[安葬逝者。|burythemall]]>> | <<choice [[继续前进。|justkeepmovingcrewcapdead]]>>


:: burythemall
<<silently>><<set $startedburialtalk = 1>><<endsilently>>
对，没错儿。我应该把他们安葬，或至少做点什么，对吧？
这附近应该会有铁锹啥的能用来挖坑的东西吧。
妈哟，这够我忙活好一阵子。也许我应该先探索飞船的其余部分，看看能不能找点吃的。
我已经精疲力尽了。
<<choice [[有道理，继续探索吧。|branchingcorridor]]>> | <<choice [[现在就挖，稍后再探索。|dignow]]>>


:: dignow
没错，没错，你说得对。
他们都是好人，需要一个体面的追悼。
这儿有……一块坏得快掉下来的防护罩。
瓦里亚号坠毁时，它有一头撞凹了，能当铲子用。
听着，这得花点时间。我等会儿再联系你，如何？
[[delay 30m|hardground]]


:: hardground
管它地面是什么材质，反正难挖得很。
我挖了……一个墓穴，不过很浅。
而且，我想起来，我也许应该去取阿雅船长的遗体……她应该希望与她的船员葬在一起。
不如现在就去吧，走趟远路取回她的遗体，比留在这里挖坑还容易些。
<<choice [[对，去取船长的遗体。|gogetcap]]>> | <<choice [[也许最好继续挖。|betterkeepdigging]]>>


:: betterkeepdigging
真的吗？你想杀了我吗？
我完全出自对她的敬仰，而不是去给我自己挖个墓穴。
这真的很累人，我饿得前胸贴后背了，还没吃东西呢，说不定再也不用吃了。
<<choice [[好吧，休息一会吧。|takeabreak]]>> | <<choice [[那就去取船长的遗体。|gogetcap]]>>


:: takeabreak
好吧，这主意不错。
我要去打个盹儿。
哦，该用公制一样的标准用语精确表达为：小睡片刻。
[[delay 30m|refreshed]]


:: rockclimbing
我真的……噢，搞砸了。
好吧，我都已经走到这里了，没理由放弃的。上！
[[delay 30m|taylorfalls]]


:: jello
太好了，现在我满脑子想的都是果冻。
[[delay 60m|arriveatcrash]]


:: arriveatcrash
哦，终于，我看得到瓦里亚号了，或者说是瓦里亚号的残躯。
看来它在冲入大气层时受了重创。
果然没猜错……碎成了两截……
周围还散落着许多残片，简直跟世界末日一样吓人。
（真的很吓人，我已经魂飞魄散了。）
看起来驾驶舱和船员区各在一截上，还离得挺远，你觉得我应该先去哪边？
<<choice [[去船员区。|checkcrewqs]]>> | <<choice [[检查驾驶舱。|totheflightdeck]]>>


:: totheflightdeck
<<silently>><<set $frontofship = 1>><<endsilently>>
好吧。我要花点时间检查下残骸。
不少隔热罩在进入大气层时就脱落了，不过飞船的大部分损伤也是在外层的隔热罩上。
多数设备看上去居然都意想不到的完好。
让我好好看看……
[[delay 5m|defensesandbeacon]]


:: defensesandbeacon
太棒了！我们有希望了！
（不过，别忘了，大部分希望都是泡泡。）
我这里有好消息和坏消息。
好消息是：我找到了飞船上的失事信标，而且在我看来，完好无损的呢！
这就是说，我大概不会困死在这里了。
还有：飞船上还有一个防御炮塔能用。
对，也就是说另外三个不能用，不过毕竟只有我一个人来操作，一个炮塔能用就够了。
如果真的有外星小绿人来袭的话，那我除了耍嘴皮子以外还有别的可以用来防身。
但现在轮到坏消息了：我没找到任何可以给失事信标或是炮塔供电的东西。因为反应器是跟船的后半部一起坠毁的。
我很肯定我们上路时没准备那么长的电源线……
而且，就算是反应器在这儿躺着，我也严重怀疑那玩意儿能用。
所以……我要继续在这半截瓦里亚号里翻翻，看能不能找到点备用电源之类的东西。
[[delay 10m|captainsbody]]


:: captainsbody
哦……哦，糟糕！在驾驶舱这边，我没找到船员，但是……我刚找到了阿雅船长。
太恐怖了……这里……有很多血。
我滴神啊！她还没死！船长还活着！
一个铁家伙刺穿了她的身体，好像是支杆……哎呀无所谓了。
该怎么办？……拔出来吗？
噢，天啊。她真的流了很多血啊。噢，天啊。
<<choice [[拔出来！|pullitout]]>> | <<choice [[不要拔。|leaveitin]]>>


:: leaveitin
对，没错。我要是随便将它拽出来的话，谁知道会不会对船长造成二次伤害？
现在……伤口已经不怎么流血了，只是她的呼吸相当微弱而且还很……怎么说呢，急促？
噢，天啊。我觉得这玩意儿八成是戳进她的肺里了。
该死，该死，该死。我应该怎么办才好？
<<choice [[去找个急救箱。|lookformeds]]>> | <<choice [[让她舒服些。|makehercomfy]]>>


:: lookformeds
对，没错，好主意。驾驶舱里面应该有个急救包。我这就去找。
[[delay 1m|nomedshere]]


:: nomedshere
该死，前舱的医药柜烧焦了。我没有撬棍和电钻……
……像我这么倒霉的人，哪怕就是把柜子打开了，也说不定得受个重伤，自己用掉柜子里所有的药了。
飞船尾部还有一个医药柜，就在船员区旁边。我还是去那里看看吧。
[[delay 30m|medsupplyhunt]]


:: medsupplyhunt
对，好了，我到了船员区的那半截残骸。
这倒不是完全出乎意料的事，但还是有点……。
我……唔……我找了些船员。
是……哦，天哪。等一下。
[[delay 2m|threwupcapalive]]


:: movethecaptain
你确定吗？我是说，我觉得我能把她弄过来——如果做副担架什么的——
不过我担心要是动作太大的话，她会再流血的。
不过……要移动整个医疗舱的话更费劲……
<<choice [[移动船长。|movethecap2]]>> | <<choice [[移动医疗舱。|movethepod]]>>


:: movethecap2
好吧，毕竟时间宝贵。
也许，像我刚才说的，做一副担架。太空毯和胶带……对，我可以去找些材料来试试。
好了，我去去就回，我要用医疗舱来治船长！
你会替我们祈祷的吧？
[[delay 60m|captainDOA]]


:: captainDOA
<<silently>><<set $capalive = 0>><<endsilently>>
我……
我真混蛋！我都这么小心了！
我尽量缓慢小心地移动她。我只是……我只是想将她及时送过来！
可是，嗯……医疗舱说她失血过多。
“超出阈值”
那该死的LED灯一直冲着我闪啊闪啊，我像个白痴一样地盯着。
我……
我想一个人静一静，行吗？
[[delay 10m|sorryforcap2]]


:: sorryforcap2
这……这真是糟糕透了。阿雅船长是我在太空飞行指挥中心遇见的第一个人。
你知道吗？她不费吹灰之力就让我觉得我是团队的一份子。
这是她的本事，她懂得如何让大家团结合作，是位非常不了起的领袖。
我……我能够不假思索地将我生命托付给她。
[[delay 4s|capsgone2]]


:: capsgone2
可是，当她的生命由我来拯救的时候……我却只能眼睁睁地看着她流血不止。
我失去她了。我现在……还能干什么呢？
<<choice [[继续前进。|movingonforcap2]]>> | <<choice [[将她永远铭记在心中。|dontforgetcap2]]>>


:: dontforgetcap2
当然，我永远不会，不会忘记她的。
我要好好活下来，才能永远缅怀她。
所以……我现在有信标和炮塔，还有一台发电机，应该能给信标或者炮塔供电。
现在我只要有食物和水就能活下去了。小事一桩，是吧？
我觉得，我应该要么去挖墓，要么去探索飞船。
<<choice [[挖墓。|digthosegraves]]>> | <<choice [[继续探索吧。|branchingcorridor]]>>


:: checkcrewqs
<<silently>><<set $backofship = 1>><<endsilently>>
好吧，我朝那边走。如果真有地方能找到口粮，就一定是那里了。
不过，这里一片狼藉，给我点时间走过去。
[[delay 5m|crewbodies]]


:: makehercomfy
“舒服”？你的意思是让她死得舒服些吗？
听着，据我所知，除了我以外，这里就剩她一个活人了。
她血都要流光了，我现要要做的，可不该只是给她找个羽绒枕头什么的。
我一定要想方设法，稳定她的伤势！
[[delay 1m|nomedshere]]


:: taylorfalls
哎哟，痛，我的腿，我的腿摔断了
哦，天啊，是开放性骨折，骨头把宇航服都都刺穿了。
[[delay 4s|brokenleg]]


:: threwupcapalive
对不起，我刚刚在石头后面吐了。
看来瓦里亚号爆炸时，有几名船员正好处在船舱炸裂的地方。
我没法……我没法辨认出他们都是谁，甚至是有几个人。
他们都和金属熔在一起了，或者说，额，相互熔在一起了。
我觉得，在失压时我们还丧失了几名船员。
[[delay 2s|deadcrewcapalive]]


:: movethepod
对，这样更保险，不过我得想办法快点把医疗舱移去那边才行啊。
你知道这里头哪个功能会派上用场吗——
不对，不对，不对
啊哈！太好了！悬停模式！
好吧，虽然这样可能会消耗更多电，不过好钢就得用在刀刃上。
（老话就是这么说的，不是吗？）
好了，我这就回到船长那里去。确认她的平安之后再联系你。
[[delay 40m|capinpod]]


:: capinpod
<<silently>><<set $power = "pod">><<endsilently>>
成功了，医疗舱说她的状态已经稳定了！
虽然失了不少血，但是还不至于致命，而且这个可以防止她的肺穿孔恶化。
哦，天。在这鸟不拉屎的石头上，我终于不再是孤零零一个人了。
这简直是个奇迹啊。
<<choice [[先别得意呢。|dontcelebrateyet]]>> | <<choice [[你该去休息一下了。|takearest]]>>


:: dontcelebrateyet
对，是啊，当然。毕竟“稳定”不代表“好转”。
而且我还得想办法脱困，给船长治伤。
<<if $crewburied is 0>>另外……我觉得我应该再去看看船员们，对吧？
我是说，他们还……躺在那边。<<endif>>
我要带上装有船长的医疗舱还有这个发电机，回另外那半截飞船去了。
我到了之后再联系你。
[[delay 30m|backhalfcapalive]]



:: movingonforcap2
对，你说得对。除了前进没有别的办法了。
保持清醒和活力，不能这样停下来。
所以……我现在有失事信标和炮塔，还有一台发电机，应该能给信标或者炮塔充电。
现在我只要有食物和水就能活下去了。小事一桩，是吧？
我觉得，我应该要么去挖墓，要么去探索飞船。
<<choice [[挖墓。|digthosegraves]]>> | <<choice [[继续探索吧。|branchingcorridor]]>>


:: gogetmeds
<<silently>><<set $ginny = 1>><<endsilently>>
好了！我现在又有好消息和坏消息了。
坏消息是：我在这里没找到急救包。我打开了医药柜，但是里面的急救包不见了。
一定是坠毁前被人拿去用，然后没有放回来。所以应该还在附近哪里。
不过！好消息是：瓦里亚号上有两个医疗舱。
其中一个基本上成废铁了（这算是个坏消息吧），不过另一个完好无损，指示灯全都是绿的！
你可能会问我是怎么知道的？
因为我还找到了一台便携式发电机！
（然后我还很高兴地跳了一段舞庆祝。）
虽然很小，但能供这个医疗舱使用几天。
这个医疗舱可能治不好船长，不过好歹能防止她的伤势恶化。
不过要将这个医疗舱还有发电机一起拖回船长那边，实在是灭绝人性啊。
我是该把船长搬过来呢，还是把这玩意儿抬过去？
<<choice [[移动船长。|movethecaptain]]>> | <<choice [[移动医疗舱。|movethepod]]>>


:: crewbodies
哦，哦，这倒不是完全出乎意料，不过，还是有点……
我……唔……我找到了些船员。
是……哦，我不行了！等一下！
[[delay 2m|threwupcrewfirst]]


:: lookformedscrewfirst
对，没错，好主意。驾驶舱里面应该有个急救包。我这就去找。
[[delay 1m|nomedsherecrewfirst]]


:: nomedsherecrewfirst
该死，前舱的医药柜烧焦了。我没有撬棍和电钻……
……像我这么倒霉的人，哪怕就是把柜子打开了，也说不定得受个重伤，自己用掉柜子里所有的药了。
飞船尾部还有一个医药柜，就在船员区旁边。我还是回去那里看看吧。
[[delay 30m|medsupplyhuntcrewfirst]]


:: medsupplyhuntcrewfirst
哎呀，一直在两截残骸之间跑来跑去……我要累成狗了，你知不知道？
哎，好了，我回船员区来了。得……继续往前冲。
要是我不尽力的话，阿雅船长会死的。现在，我身边就只有她一个人了。
这里好乱……给我几分钟时间。
[[delay 10m|gogetmeds]]


:: brokenleg
我这辈子都没断过根骨头，一来就是个这么猛的
哎哟，痛死了，我怎么还没休克啊？
[[delay 4s|badadvice]]


:: takearest
对，有点道理。来回这样两头跑，卡路里都要烧没了……
我一直强打精神在干，是该稍微休息一下，恢复点体力。我要去打个小盹。
哦，该用公制一样的标准用语精确表达为：小睡片刻。
[[delay 30m|refreshedcapalive]]


:: refreshedcapalive
信不信由你，我打了个盹之后感觉好多了。
医疗舱显示船长情况稳定，这太好了。但是“稳定”不代表“好转”。
而且我还得想办法脱困，给船长治伤。
<<if $crewburied is 0>>另外……我觉得我应该再去看看船员们，对吧？
我是说，他们还……躺在那边。<<endif>>
我要带上装有船长的医疗舱还有这个发电机，回另外那半截飞船去了
我到了之后再联系你。
[[delay 30m|backhalfcapalive]]



:: backhalfcapalive
郁闷地跋涉过一片荒原，我们又回到了无人的船员区。
我的生活简直比中学生写的诗还要烂。
<<if $crewburied is 0>>
现在看来，我应该要么去挖墓，要么去探索飞船。
<<choice [[挖墓。|digthosegraves]]>> | <<choice [[继续探索吧。|branchingcorridor]]>>
<<elseif $crewburied is 1>>我觉得除了继续深入飞船，也没有别的可做的了。
[[branchingcorridor]]
<<endif>>



:: deadcrewcapalive
在尾舱的密封门后……我发现了剩下的船员。
安冬、特罗特、科尔比、阿戴尔，都死了。他们都死了……都死了。
我……我做不到……他们都死了，每一个人……而我……
<<choice [[请节哀。|sorrycapalive]]>> | <<choice [[继续前进。|keepmovingcapalive]]>>


:: keepmovingcapalive
对，你说得对。要是我不尽力的话，阿雅船长会死的。现在，我身边就只有她一个人了。
这里好乱……给我几分钟时间。
[[delay 10m|gogetmeds]]


:: totheflightdeckcrewfirst
<<silently>><<set $frontofship = 1>><<endsilently>>
好了，我得花几分钟检查一下残骸。
不少隔热罩在进入大气层时就脱落了，不过飞船的大部分损伤也是在外层的隔热罩上。
多数设备看上去居然都意想不到的完好。
让我好好看看……
[[delay 5m|defensesandbeaconcrewfirst]]


:: defensesandbeaconcrewfirst
太棒了！我们有希望了！
（不过，别忘了，大部分希望都是泡泡。）
好了，这里有好消息和坏消息。
好消息是：我找到了飞船上的失事信标，而且在我看来，完好无损的呢！
这就是说，我大概不会困死在这里了。
还有一个好消息：船上还有一个防御炮塔可以使用。
是的，也就是说另外那三个已经坏掉了，不过毕竟只有我一个人来操作，一个炮塔就够了。
也就是说如果真有小绿人进攻的话，那我除了耍嘴皮子以外还有别的能防身。
现在轮到坏消息了：我没有找到任何可以给失事信标或是炮塔供电的东西。因为反应器是跟飞船后半部一起坠毁的。
我很肯定我们上路时没有准备那么长的电源线……
……而且，就算是反应器在这儿躺着，我也严重怀疑那玩意儿能用。
所以……我要继续在这半截瓦里亚号里找找，看能不能找到点备用电源之类的。
[[delay 10m|captainsbodycrewfirst]]


:: captainsbodycrewfirst
哦……哦，不。
我刚找到了阿雅船长。
这里有好多血……我完全没有想到
……偶滴神啊！她还没死！船长还活着！
一个铁家伙刺穿了她的身体，好像是支杆……哎呀无所谓了。
该怎么办？……拔出来吗？
哦，天啊。她真的流了很多血啊。哦，天啊。
<<choice [[拔出来！|pullitoutcrewfirst]]>> | <<choice [[不要拔。|leaveitincrewfirst]]>>


:: leaveitincrewfirst
对，没错。我要是随便将它拽出来的话，谁知道会不会对船长造成二次伤害呢？
现在……伤口已经不怎么流血了，只是她的呼吸相当微弱而且还很……怎么说呢，急促？
噢，天啊。我觉得这玩意儿八成是戳进她的肺里了。
该死，该死，该死。我应该怎么办才好？
<<choice [[去找个急救箱。|lookformedscrewfirst]]>> | <<choice [[让她舒服些。|makehercomfycrewfirst]]>>


:: threwupcrewfirst
对不起，我刚刚在石头后面吐了。
看来瓦里亚号爆炸时，有几名船员正好处在船舱炸裂的地方。
我……我都辨不出他们是谁，甚至是几个人。
他们都和金属熔在一起了，或者说，额，相互熔在一起了。
我觉得，在失压时我们还丧失了几名船员。
[[delay 2s|deadcrewfirst]]


:: makehercomfycrewfirst
“舒服”，你的意思是让她死得舒服些吗？
听着，据我所知，除了我以外，这里就剩她一个活人了。
她血都要流光了，我现要要做的，可不该只是给她找个羽绒枕头什么的。
我一定要想方设法，稳定她的伤势！
[[delay 1m|nomedsherecrewfirst]]


:: badadvice
嘿，无意冒犯，但是我不得不说，你的建议真是弱爆了。
[[delay 4s|connectionlost]]


:: sorrycapalive
啊……这……真是太惨了……
我真心希望他们平安无事的，可是……哦，天啊。
我觉得我应该安葬他们，你懂吗？
<<choice [[对，安葬逝者。|burythemallcapalive]]>> | <<choice [[稍等下吧。先去找医疗用具。|gogetmeds]]>>



:: burythemallcapalive
对，你说得对。我应该安葬他们，或至少做点什么，对吧？
这附近应该会有铁锹啥的能用来挖坑的东西吧。
等下，我在想什么啊？
情况如此紧急，我不能浪费时间多愁善感，也确实没有工夫挖墓。
阿雅船长还在指望我呢！
[[delay 10m|gogetmeds]]


:: pullitoutcrewfirst
对，好主意，我只要
糟了！血流得更多了！你说，怎么才能子血？
子血？止血？我连话都说不清了！
阿雅船长，求你了，别再流血了。我要，我要去找纱布来。
我不知道……天啊……天啊。
[[delay 15m|capdidntmakeitcrewfirst]]


:: capsgonecrewfirst
可是，当她的生命由我来拯救的时候……我却只能眼睁睁地看着她流血不止。
她死了。我现在……还能干什么呢？
<<choice [[继续前进。|movingonforcapcrewfirst]]>> | <<choice [[将她永远铭记在心中。|dontforgetcapcrewfirst]]>>


:: movingonforcapcrewfirst
对，你说得对。除了前进，还能有什么办法？
保持清醒和活力，不能这样停下来。
现在……我有失事信标和炮塔，我还需要能源、食物和水。小事一桩，是吧？
我还有船长的遗体。感觉怪怪的，她还穿着飞行服什么的呢，可是……
……不知道怎么回事，怎么没我想像的重呢……
现在，感觉她好像只是……一个空壳，似乎她的身体被掏空了，什么都没有了。
我打算先用太空毯和胶带做一副担架。这样搬运她最方便。
等我们又回到船员区时，我再跟你联系。
[[delay 30m|headingtocrewqsagain]]


:: connectionlost
[连接丢失]
[[delay 2s|gameover]]


:: deadcrewfirst
在尾舱的密封门后……我发现了剩下的船员。
安冬、特罗特、科尔比、阿戴尔，都死了。他们都死了……都死了。
我到处都没看到阿雅船长。我不知道……
我……我做不到……他们都死了，每一个人……我只是……
<<choice [[安葬逝者。|burythemallcrewfirst]]>> | <<choice [[继续前进。|justkeepmovingcrewfirst]]>>


:: walktoflightdeck
好吧，这得花点时间。我到了之后再联系你。
[[delay 30m|totheflightdeckcrewfirst]]


:: capdidntmakeitcrewfirst
<<silently>><<set $capalive = 0>><<endsilently>>
她……她去了。
阿雅船长死了。
<<choice [[请节哀。|sorryforcapcrewfirst]]>> | <<choice [[振作点儿！|keepitforcapcrewfirst]]>>


:: justkeepmovingcrewfirst
哎，你……你说得对。
我要是再死盯着看，不想点办法脱困的话，我成为他们中的一员也就不远了。
天，这是我有过的最变态的想法了。
[[branchingcorridor]]


:: burythemallcrewfirst
<<silently>><<set $startedburialtalk = 1>><<endsilently>>
对，你说得对。我应该……安葬他们，或者至少做点什么，对吧？
这附近应该会有铁锹啥的能用来挖坑的东西吧。
妈哟，这够我忙活好一阵子。也许我应该先探索飞船的其余部分，看看能不能找点吃的。
我已经精疲力尽了。
<<choice [[有道理，继续探索吧。|branchingcorridor]]>> | <<choice [[现在就挖，稍后再探索。|dignowcrewfirst]]>>


:: hardgroundcrewfirst
管它地面是什么材质……反正难挖得很。
我挖了……一个墓穴，不过很浅。
而且，我想起来……我尽在这里忙活了，没想过去另外那半截飞船里看看有没有幸存者。
我暂时歇口气，去那边瞧瞧吧。
我没有不尊重逝者，但是万一还有人等我去救呢，留在这里挖墓确实过份了点，对吧？
<<choice [[检查驾驶舱。|walktoflightdeck]]>> | <<choice [[继续挖。|betterkeepdiggingcrewfirst]]>>


:: sorryforcapcrewfirst
这……这真是糟糕透了。船长是我在太空飞行指挥中心遇见的第一个人。
你知道吗？她不费吹灰之力就让我觉得我是团队的一份子。
这是她的本事，她懂得如何让大家团结合作，是位非常不了起的领袖。
我……我能够不假思索地将我生命托付给她。
[[delay 4s|capsgonecrewfirst]]


:: betterkeepdiggingcrewfirst
真的吗？你想杀了我吗？
我完全出自对死者的尊重，而不是为了给我自己挖个墓穴。
这真的很累人，我饿得前胸贴后背了，还没吃东西呢，说不定再也不用吃了。
<<choice [[好吧，休息一会吧。|takeabreakcrewfirst]]>> | <<choice [[检查驾驶舱。|walktoflightdeck]]>>


:: dontforgetcapcrewfirst
当然的了，我永远不会忘记她的。
我要好好活下来，才能永远缅怀她。
现在……我有失事信标和炮塔，我还需要能源、食物和水。小事一桩，是吧？
我还有船长的遗体。感觉怪怪的，她还穿着飞行服什么的呢，可是……
……不知道怎么回事，怎么没我想像的重呢……
现在，感觉她好像只是……一个空壳，似乎她的身体被掏空了，什么都没有了。
我打算先用太空毯和胶带做一副担架。这样搬运她最方便。
等我们又回到船员区时，我再跟你联系。
[[delay 30m|headingtocrewqsagain]]


:: headingtocrewqsagain
郁闷地跋涉过一片荒原，我们又回到了无人的船员区。
我的生活简直比中学生写的诗还要烂。
我觉得，我应该要么去挖墓，要么去探索飞船。
<<choice [[挖墓。|digthosegraves]]>> | <<choice [[继续探索吧。|branchingcorridor]]>>


:: dignowcrewfirst
没错，没错，你说得对。
他们都是好人，需要一个体面的追悼。
这儿有……一块坏得快掉下来的防护罩。
瓦里亚号撞裂时，它有一头撞凹了，能当铲子用。
听着，这得花点时间。我等会儿再联系你，如何？
[[delay 30m|hardgroundcrewfirst]]


:: takeabreakcrewfirst
对，有道理。
我要去打个小盹。
哦，该用公制一样的标准用语精确表达为：小睡片刻。
[[delay 30m|refreshedcrewfirst]]


:: keepitforcapcrewfirst
“振作点儿”？！你在开玩笑吧？
我身上沾满了一个女人的血，无论怎么看，都是我杀了她——顺便一说，是听了你！的！建议。
然后，你就轻描淡写地让我“振作点儿”？
你去死吧。对，这是我的建议。
[[delay 15m|goscrewyourselfcrewfirst]]


:: goscrewyourselfcrewfirst
嘿。唔……不好意思。
我情绪太过于激动了。
其实，你说的没错。如果我想获救的话，确实应该振作点儿。
……船长死得太惨了，愿逝者安息。她失了这么多血，也许这样也少受些煎熬。
现在，我！还活着，想想接下来该做什么吧。
<<choice [[继续前进。|movingonforcapcrewfirst]]>> | <<choice [[将她永远铭记在心中。|dontforgetcapcrewfirst]]>>


:: digthosegraves
好吧，就这么着了。
<<if $crewburied is 0 and $capalive is 0>>我要接着去挖墓，埋葬船长和船员们，我想这是一件善事吧。
至少要正式地告别一下，是吧？
我希望为他们每个人讲一段告别辞。我干活时想独处一会儿。
等下再联系你。[[delay 60m|everybodyburied]]
<<elseif $crewburied is 0 and $capalive is 1>>我要去接着挖墓坑，埋葬船员们，我想这是一件善事吧。
至少要正式地告别一下，是吧？
我希望为他们每个人讲一段告别辞。我干活时想独处一会儿。
等下再联系你。[[delay 60m|crewburied]]
<<elseif $crewburied is 1 and $capalive is 0>>再挖一个墓坑应该不会花太多时间。今天我已经挖了好多次，很熟练了。
我要把阿雅船长和船员们埋在一起。我想她会喜欢这样的。
过会儿再联系你。[[delay 30m|everybodyburied]]<<endif>>



:: refreshedcrewfirst
信不信由你，我打了个盹之后觉得好多了。
我要去接着挖墓坑，埋葬船员们，我想这是一件善事吧。
至少要正式地告别一下，是吧？
我希望为他们每个人讲一段告别辞。我干活时想独处一会儿。
等下再联系你。
[[delay 60m|crewburiedcrewfirst]]


:: crewburiedcrewfirst
<<silently>><<set $capburied = 0>><<set $crewburied = 1>><<endsilently>>
我想我现在要么去驾驶舱看看，希望能找到船长……
……或者深入探索飞船的这半截。
<<choice [[检查驾驶舱。|walktoflightdeck]]>> | <<choice [[继续探索这里。|branchingcorridor]]>>


:: everybodyburied
<<silently>><<set $capburied = 1>><<set $crewburied = 1>><<endsilently>>
好了，就这样了。所有人都安葬了。
我觉得除了继续深入飞船的这半截，也没有别的可做的了。
[[branchingcorridor]]



:: crewburied
<<silently>><<set $capburied = 0>><<set $crewburied = 1>><<endsilently>>
好了，就这样了。所有人都安葬了。
我觉得除了继续深入飞船这半截，也没有别的可做的了。
[[branchingcorridor]]


:: branchingcorridor
好吧，嗯……经过船员铺位之后，是一条有岔道的走廊。
去厨房的路看来烧毁严重。另一条道儿能去实验室。
真怕见到我那些小老鼠们的惨状。如果连我们人都活不下来，那些小东西还有什么机会？
<<choice [[去厨房看看。|trythegalley]]>> | <<choice [[去实验室看看。|trythelab]]>>


:: tryinggalleydoor
兄弟啊……你简直是站着说话不腰疼，反正不是你来干是吧？
好吧，不入虎穴，焉得虎子，接着砸。
[[delay 3m|intogalley]]


:: stilltryinggalley
好吧，“不可能”听起来有点夸大其辞，我实在不想用……
我就换个说法吧，要进厨房“不太可能”。
灰常，灰常的，不太可能。
反正现在时间也晚了。太阳（嗯……恒星……天仓星）也快下山了。
我要考虑一下过夜的问题了。
你怎么想？要我继续跟这扇门较劲，直到它放弃吗？
还是不管门了，先考虑睡觉的问题呢？
<<choice [[继续撞门。|tryinggalleydoor]]>> | <<choice [[先不管门。|stoptryinggalleydoor]]>>


:: trythelab
<<silently>><<set $ratpellets = 1>><<endsilently>>
好了，又该来播报一段好消息/坏消息了。
好消息是：我那些研究小白鼠在不同重力下走迷宫和觅食习性的笔记找到了。
全都完好无损！
你不用担心这趟旅程毫无收获了。这是好消息。
坏消息是：老鼠笼子就跟经历过一场铁笼赛一样，大半边因为高温变形损毁了。
小老鼠们也都不见了。
我希望它们逃进通风管道里躲过一劫，然后扎根在这个荒凉的星球上，传宗接代。
我要把我的笔记本留在这里。谁知道呢？千秋万代之后这些笔记就是这颗星球上珍贵的历史文献了。
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
[[sleepingplans]]<<endif>>



:: eatratfood
呃……能好吃吗？
老鼠饲料也许是能提供能量，但是味道嘛，实在不敢恭维。
要是闭上眼睛，想象成……唔……
不行，实在不行。我怀疑谁真有本事能把这些饲料想象成什么好东西。
卫星上嚼鼠粮，听着我都醉了。
[[delay 4s|tasteslikeone]]


:: trythegalley
<<silently>><<set $triedgalley = 1>><<endsilently>>
这扇门……卡死了！
好在我也不怕拿破铜烂铁使劲儿地砸……
反正，我敢肯定，这艘船的保证金是拿不回来了。
我也是拼了，因为，要是这儿找不到吃的，哪里还能！
只要我能打开……这扇……魂淡的……门！
[[delay 3m|stilltryinggalley]]


:: tasteslikeone
尝起来也很像。
好吧，这正好给了我想办法进入厨房搞点真正的食物的动力。
要么去厨房，要么我得开始考虑晚上怎么睡觉了……
希望梦见什么东西能消掉我嘴里的老鼠饲料味道。
<<if $triedgalley is 1>>
<<choice [[再去试试厨房吧。|tryinggalleydoor]]>> | <<choice [[准备洗洗睡吧。|sleepingplans]]>>
<<else>>
<<choice [[试试厨房吧。|trythegalley]]>> | <<choice [[准备洗洗睡吧。|sleepingplans]]>>
<<endif>>


:: intogalley
<<silently>><<set $rations = 1>><<set $hurtshoulder = 1>><<endsilently>>
我的肩膀都甩脱臼了！
可是这太值得了！
因为我在痛不欲生——！的同时！——吃着辣椒通心粉，喝着瓶装水！
这辣椒通心粉有点恶心——！的同时！——是我这辈子吃过最好吃的东西！
可惜马上就要吃完了……
……然后我就得处理我可怜的肩膀了，为了一份辣椒通心粉，我也是拼了，对着一扇打不开的液压门锤了那么久。
不过那个难题还是留给3分钟之后的我吧。
<<if $ratpellets eq 1>>而且我看谁都不会有反对意见：不论怎么看，这都比老鼠饲料好吃一万倍！
我现在有了食物和水，还有老鼠饲料作战备粮。
得考虑一下过夜的问题了。[[sleepingplans]]
<<else>>那么，现在食物的问题解决了，我可以要么去看看实验室里还剩下什么东西没有……
……要么考虑睡觉的事儿了。你怎么看？
<<choice [[去实验室看看。|trythelab]]>> | <<choice [[准备洗洗睡吧。|sleepingplans]]>><<endif>>



:: stoptryinggalleydoor
行！谢谢你。我需要有人安慰我，不那么拼命也可以。
<<if $ratpellets is 0>>那么，我要么去看看实验室里还剩下什么东西没有……
……要么考虑睡觉的事儿了。你觉得呢？
<<choice [[去实验室看看。|trythelab]]>> | <<choice [[准备洗洗睡吧。|sleepingplans]]>>
<<elseif $ratpellets is 1>>我想是该考虑一下睡觉的事了。
[[sleepingplans]]<<endif>>


:: sleepingplans
好吧。让我仔细想想，想清楚下一步怎么做。
<<if $crewburied is 1>>船员们都安葬好了。<<endif>>
<<if $capburied is 1>>船长也是。<<endif>>
<<if $ginny is 1 and $power is "pod">>医疗舱平稳地轰鸣着，船长在里面好好地活着。或者说，活着。
不过，只要发电机还在给医疗舱供电，我就不能使用失事信标或者炮塔。
用不了炮塔倒是没什么，我真心觉得这里除了我，没别的活物了……
……但用不了信标的话，怎么告诉别人我在这里并且还活着呢？我怎么能够脱困呢？
经历了这么多……我怎么能为了一己之利，扯下船长的医疗舱的电源？
能吗？
<<choice [[继续给医疗舱供电。|powerpodcapalive]]>> | <<choice [[给信标供电。|powerbeaconcapalive]]>>
<<elseif $ginny is 1 and $power is "none">>我取下了医疗舱旁的发电机。
现在，唔……既然船长已经不在了，我觉得能拿发电机来做点别的事。
给失事信标或者炮塔供电。
用不了炮塔倒是没什么，我真心觉得这里除了我，没别的活物了……
但信标是通知别人来救我的唯一途径。
你怎么看？
<<choice [[给炮塔供电。|powerturretcapdead]]>> | <<choice [[给信标供电。|powerbeaconcapdead]]>>
<<elseif $ginny is 0 and $frontofship is 1>>要是能把失事信标发射出去发送信号就太好了，或者让炮塔运转起来以防万一也不错……
……不过，没有电，啥也干不了。
而且，尤其杯具的是：天色越来越暗，也越来越冷了。
[[nightfalldayone]]
<<else>>
嘿，尤其杯具的是：天色越来越暗，也越来越冷了。
[[nightfalldayone]]<<endif>>




:: powerbeaconcapdead
<<silently>><<set $power = "beacon">><<endsilently>>
太棒了！我们不谋而合！给我几分钟把那玩意儿接上电，转起来。
[[delat 15m|powerbeaconcapdeadrunning]]



:: powerbeaconcapalive
我……天哪，看来我真的需要考虑那个选项，是吗？
我是说，我要是不发出求救信号，发电机耗尽了，船长就会死……
……我的储备吃完时我也没救了。
（我话说在前面，我可不吃人。死活都不吃，说什么都不吃。）
阿雅船长总是非常理性地做决策。我觉得我也应该这样做。
我真的要为了救自己，而眼看着她死吗？
<<choice [[不，继续给医疗舱供电。|powerpodcapalive]]>> | <<choice [[你需要个那个信标。|reallybeaconcapalive]]>>


:: reallybeaconcapalive
这个决定……感觉好烂……即使是对的。
我想……没有信标的话，我们两个都会死在这里。选这条路，至少我们其中一个有机会。
我打算用这点来说服自己。
好了，我现在给医疗舱断电。
我需要……我需要点时间处理一下。
[[delay 20m|beaconpowered]]


:: beaconpowered
<<silently>><<set $power = "beacon">><<set $capalive = 0>><<endsilently>>
好了，现在，广谱的失事信标，将我的位置传遍这个星区，传给所有在收听的人吧。
要是这屏幕上显示的没错，那我可以肯定，现在什么人都没有。
嗯，唔，医疗舱显示里面没有生命迹象了。
该死，信标，你最好工作。
[[nightfalldayone]]


:: powerpodcapalive
对，当然了。她挺过了这么多磨难，我怎么能让她死呢。
也许等明天天亮了，我就能找到启动失事信标的方法了。
我的心里有个声音在说他们选错随行学生了，或者说我选错旅程了。
不管怎么样，他们都应该多给我的期末成绩多打几分。
[[nightfalldayone]]



:: powerturretcapdead
<<silently>><<set $power = "turret">><<endsilently>>
听起来你比我更怕《异形》啊。
但是没错，小心驶得万年船。
给我几分钟把炮塔接上电，转起来。
[[delay 15m|powerturretcapdeadrunning]]


:: powerturretcapdeadrunning
好了，炮塔已经接上电了，我设置了自动防御模式。
只要有比面包箱大的东西在周围转，炮塔就会进行警告射击，并发信号给我。
（我可不认为我今晚能够在微风中入睡，如果再有两声炮响的话就更不可能了。）
如果入侵者仍不止步，我能切换到手动模式，或者由着自动射击把他们消灭。
这真是奇怪，我们居然还在用“面包箱“当测量标准。
不知道要是用公制应该算多大呢？
[[nightfalldayone]]


:: powerbeaconcapdeadrunning
好了，现在，广谱的失事信标，将我的位置传遍这个星区，传给所有在收听的人吧。
要是这屏幕上显示的没错，那我可以肯定，现在什么人都没有。
不过，嘿，这个星区可不小。有好多，我说不上来，好像是什么都没有的真空地带。旅行者们可能随时出现。
现在我们等着吧。
[[nightfalldayone]]


:: nightfalldayone
天仓星已经下山了，而且我不会介意告诉你，这里比一个哥特帮小子的日记还黑暗，比高冷的舞会皇后还冷。
所以，唔，我想我有这么几个选择。
我可以就呆在这残骸里面——虽然由于飞船失去了动力，我打开的门都关不上了。
我头顶上会有个屋顶，不过就得暴露在恶劣的气温中了。
或者我可以到飞船尾部去，在反应堆引擎旁边搭个帐篷……
……这会让我暖暖和和的——我可以看到引擎正在向夜空袅袅散射的热能。
（你喜欢这种高大上的说法吗？“袅袅散射的热能”？这可是高级的学术词汇。）
唯一的问题就是，那个引擎有辐射，我不知道辐射有多强，会不会一晚上下来我就翘辫子了。
所以能不能请你帮我查一下？
如果我一晚上吸收150拉德的辐射量，我会不会死？我的宇航服感应器说辐射量大概是这个值。
能帮忙查一下我会不会因此翘辫子吗？谢了。
[[delay 5m|reactorquestion]]


:: reactorquestion
那么，唔，希望你已经查到了，因为这里已经冷出翔了。
你怎么看？应该就地找点东西盖一下然后在瓦里亚号里头哆嗦一晚上？
还是冒着辐射危险去坏掉的反应堆旁边取一晚上的暖？
<<choice [[留在飞船里。|sleepintheship]]>> | <<choice [[去反应堆旁边。|sleepbythereactor]]>>


:: sleepintheship
是啊，我也觉得这个方法比较保险。我宁可冷点，也不想变异。
或者死于辐射。
好吧，那我就去过夜了。这真是……哎，我没夸张，还真是我有生以来最糟糕的一天了。
不过我还是很感激你陪我度过这一切。
愿明天一切都能好起来。
晚安。
[[delay 180m|socold]]


:: socold
我~~没~~法
感觉
到我的手了
太太太冷冷
[[delay 8s|connectionlost]]



:: nothanksspit
别后悔哦。
[[afterspitassessment]]


:: sleepbythereactor
说句心里话：我心里虚得很。
我是说，你帮我度过了今天——绝对是我一辈子最糟的一天。
（我肯定，谁都没有哪一天比我的今天更糟了。）
所以我决定再相信你一次。我是说，不管你真的确定这个辐射强度没事……
……还是你懒得查，由着我自生自灭了。
我想只有到了明天早上醒来后我才知道是不是了。
带着这个愉快的想法……晚安吧，不管你在哪儿。
愿明天一切都能好起来。
[[delay 360m|dawnofdaytwo]]



:: dawnofdaytwo
嘿！瞧瞧！我没死！
而且没有在额头上多长出个耳朵，也没有别的奇奇怪怪的辐射变异！太棒了。
好了。全新的一天，熟悉的荒漠。
或许我不该多嘴，不过我早上起来的时候，嘴里有一股味儿，怪味儿！
我漱了漱口，吐出来的水……有点怪怪的绿色。
大概跟这里的空气里的什么元素有关吧……
……不过，还是挺奇怪的，对吧？
<<choice [[噢哟，谢谢分享。|ughthanksforsharing]]>> | <<choice [[难道是得辐射病了？|radiationsicknessmaybe]]>>




:: ughthanksforsharing
客气，哎，除了你，我还能跟谁聊这门子事呢？
请务必与我分享你的口水长啥样，这才公平嘛。
<<choice [[不用了，总之谢谢了。|nothanksspit]]>> | <<choice [[和普通的口水一样啊，你这傻瓜。|normalspit]]>>



:: afterspitassessment
<<if $rations is 1>>
好在我能吃点东西来除掉嘴里的味道。
我打算来点甜香肠——这堆袋子最适合当早餐的东西。
我先吃去了，尽量恢复精力。我等会儿给你消息。 [[delay 15m|afterbreakfast]]
<<elseif $ratpellets is 1 and $rations is 0>>
总之，老鼠饲料的味儿也好不到哪去……
……不过我得想办法把嘴里的怪味儿弄掉。
等我先吃点老鼠饲料，活下去再说。
也许我还可以再喝点我找到的水。我等会儿给你消息。 [[delay 15m|afterbreakfast]]
<<else>>
好伤心，我昨天要是找到了点吃的东西就好了。
不仅是因为我虚弱得一塌糊涂，饿得前胸贴后背……
也是因为我真心希望能有什么东西把嘴里的怪味道盖掉。
我就最后再找一遍吧，看看能不能找到什么可以吃的。
这边有条走廊，通向实验室和厨房（厨房门烧得很坏了）。
我也可以继续搜索飞船里别的地方。
<<choice [[探索那条走廊。|trythatcorridor]]>> | <<choice [[探索飞船的其余地方。|trytherest]]>><<endif>>




:: afterbreakfast
好吧，这味道……绝说不上“美味”。
也许只能算得上是“能吃”。说“能吃”都勉强。
不过它应该可以支撑我完成今天的户外活动。
[[moveondaytwo]]


:: radiationsicknessmaybe
哦，天哪！你真这么想？我没觉得不舒服，也没觉得哪里不对劲。不头痛、不恶心，啥都没有。
事实上……除了“迷失外太空”这件事外，我今天一早起来，感觉还挺不错的。
只不过……绿色的口水确实有点奇怪，对吧？
天啊。我真心希望我不是得了辐射病。要在这里找到人骨髓配型成功可不容易啊。
[[afterspitassessment]]


:: normalspit
好吧，好吧。瞧瞧这是谁对自己的口水如此神气活现啊。
恭喜你有如此无与伦比的唾液。你爸妈一定很骄傲吧。
[[afterspitassessment]]


:: trythatcorridor
好吧，现在我们又到了铺位后头的走廊。
跟昨天一样，它还通向实验室和厨房。（有些东西没变，真不赖。）
<<choice [[试试厨房吧。|trythegalleydaytwo]]>> | <<choice [[去实验室看看。|trythelabdaytwo]]>>


:: trytherest
好吧，好主意。我要在这堆破烂中好好找找，没准能找到四菜一汤的大餐呢。
[[delay 15m|noluckfindingfood]]



:: noluckfindingfood
真不走运。并且你知道更倒霉的是什么吗？
这颗卫星，不像宇宙中别的卫星一样是绿奶酪做的！
开始觉得我学的科学知识对出这趟门一点用都没有。
哎，说不定翻过下个坡就有麦当劳了。通常能找到的，我是说在地球上。
[[nobreakfast]]


:: nobreakfast
哎哟，我现在又累、又饿、又渴，站都站不稳了。
还该动身去户外活动了。
[[moveondaytwo]]


:: intogalleydaytwo
<<silently>><<set $rations = 1>><<set $hurtshoulder = 1>><<endsilently>>
我的肩膀都拉脱臼了！
可是这太值得了！
因为我在痛不欲生——！的同时！——吃着甜香肠，喝着瓶装水！
这甜香肠有点恶心——！的同时！——是我这辈子吃过的最好吃的东西。
然后我就得处理我可怜的肩膀了，为了点香肠，我也是拼了，对着一扇打不开的液压门锤了那么久。
不过那个难题还是留给3分钟之后的我吧。
<<if $ratpellets eq 1>>而且我看谁都不会持反对意见：不论怎么看，这都比老鼠饲料好吃一万倍！
我现在有了食物和水，还有老鼠饲料作战备粮。
得考虑一下今天的行动了。 [[moveondaytwo]]
<<else>>好了，现在吃的问题解决了，我可以去看看实验室里还剩下什么东西没有……
或者快点出门看看。你觉得呢？
<<choice [[去实验室看看。|trythelabdaytwo]]>> | <<choice [[出门看看。|moveondaytwo]]>><<endif>>



:: tryinggalleydoor2a
哥们儿啊……你简直是站着说话不腰疼，反正不是你来干是吧？
好吧，不入虎穴，焉得虎子，接着砸！
[[delay 3m|intogalleydaytwo]]


:: stilltryinggalley2a
好吧，“不可能”听起来有点夸大其辞，我实在不想用……
我就换个说法吧，要进厨房“不太可能”。
灰常，灰常地，不太可能。
不过反正现在时间也晚了。太阳（嗯……恒星……天仓星）也要下山了。
得考虑一下过夜的问题了。
你怎么想？要我继续跟这扇门较劲，直到它放弃吗？
还是不管门了，先考虑睡觉的问题呢？
<<choice [[继续砸门。|tryinggalleydoor2a]]>> | <<choice [[先不管门。|stoptryinggalleydoor2a]]>>


:: trythegalleydaytwo
<<if $triedgalley is 0>>
<<silently>><<set $triedgalley = 1>><<endsilently>>
这扇门……卡死了！
反正我也不怕拿块破铜烂铁来砸门，这艘船的保证金肯定是拿不回来的了。
我也是拼了，因为，要是这儿找不到吃的，哪里还能！
只要我能打开……这扇……魂淡的……门！
[[delay 3m|stilltryinggalley2a]]
<<elseif $triedgalley is 1>>
门……还是……纹！丝！不！动！
我要换一块跟门的材制不一样的东西来砸门了，希望它们成好朋友，然后门就开了。
一般来说是可以的，对吧？
[[delay 3m|intogalleydaytwo]]<<endif>>



:: trythelabdaytwo
<<silently>><<set $ratpellets = 1>><<endsilently>>
好了，又该来播报一段好消息/坏消息了。
好消息是：我那些研究小白鼠在不同重力下走迷宫和觅食习性的笔记完好无损！
你不用担心这趟旅程毫无收获了。这是好消息。
坏消息是：老鼠笼子就跟被哥斯拉踩过一样，大半边因为高温变形了。
小老鼠也都不见了。
我希望它们逃进通风管道里躲过一劫，然后扎根在这个荒凉的星球上，传宗接代。
我要把我的笔记本留在这里。谁知道呢？千秋万代之后这些笔记就是这颗星球上珍贵的历史文献了。
<<if $rations is 0>>还有个（有点）好的消息。我不会饿死在这里了。我找到了老鼠饲料，以及一个半满的水瓶。
瞧瞧。我们这叫“半满”呢。
简直是太乐观了。
所以……我该知足了，就靠老鼠饲料度日子了……
……或者，还是把这当成最好永远用不到的后备计划吧。我该去找点人吃的。
<<if $triedgalley is 0>>
<<choice [[人类的食物！试试厨房吧。|trythegalleydaytwo]]>> | <<choice [[就吃老鼠饲料吧。|eatratfooddaytwo]]>>
<<elseif $triedgalley is 1>>
<<choice [[再去试试厨房吧。|tryinggalleydoor2a]]>> | <<choice [[就吃老鼠饲料吧。|eatratfooddaytwo]]>><<endif>>
<<elseif $rations is 1>>嘿，除了口粮，我还找到了万不得已能当饭吃的老鼠饲料，甚至半满的水瓶呢。
瞧瞧。我们这叫“半满”呢。
简直是太乐观了。
总之，我看是得快点上路了。
[[moveondaytwo]]<<endif>>



:: stoptryinggalleydoor2a
好的，谢谢你。我需要有人安慰我，不那么拼命也可以。
<<if $ratpellets is 0>>那么，我要么去看看实验室里还剩下什么东西没有……
要么快点出门。你觉得呢？
<<choice [[去实验室看看。|trythelabdaytwo]]>> | <<choice [[快出门。|moveondaytwo]]>>
<<elseif $ratpellets is 1>>
好吧，这味道……绝说不上“美味”。
也许只能算得上是“能吃”。说“能吃”都勉强。
不过它应该可以支撑我完成今天的户外活动。
得快点上路了。
[[moveondaytwo]]<<endif>>


:: eatratfooddaytwo
呃……能好吃吗？
老鼠饲料也许能提供能量，但是味道嘛，实在不敢恭维。
要是闭上眼睛，把饲料想象成……额……
不行，实在不行。我怀疑真有谁有本事能把这些饲料想象成什么好东西。
卫星上嚼鼠粮，听着我都醉了。
[[delay 4s|tasteslikeonedaytwo]]


:: moveondaytwo
<<if $power is "beacon">>信标仍然在发送信号，但屏幕显示整整一天都没有飞船出现。
所以我觉得应该有时间去瞧瞧北边那座奇怪的山峰……
……然后还能在天黑前回到瓦里亚号。
<<elseif $power is "turret">>应该有时间去瞧瞧北边那座奇怪的山峰……
然后还能在天黑前回到瓦里亚号。
<<elseif $power is "pod">>发电机还在低声轰鸣着，给医疗舱供着电……
里面的阿雅船长情况也仍然稳定，当然我更希望她是“好了”，哎，别做梦了。
估计像这种基本靠奇迹的情况，你就不能贪心了。
总之，她在这里很安全，我可以出去探索几个钟头。
应该有时间去瞧瞧北边那座奇怪的山峰……
……然后还能在天黑前回到瓦里亚号。
<<else>>应该有时间去瞧瞧北边那座奇怪的山峰……
……然后还能在天黑前回到瓦里亚号。<<endif>>
再扫荡一遍飞行船，确定没有遗漏了……
<<if $rations is 1>>
……然后我要带上食物和水出发了。
<<elseif $rations is 0 and $ratpellets is 1>>然后我带上鼠粮和水出发了。
<<else>>……然后我要出发了。心明胃净<<endif>>
在路上我会和你保持联系的。
[[delay 60m|backatescapepod]]



:: tasteslikeonedaytwo
尝起来也很像。
好吧，这正好给了我想办法进入厨房搞点真正的食物的动力。
不然的话，我最好快点出门。
<<choice [[试试厨房吧。|trythegalleydaytwo]]>> | <<choice [[快点出门。|moveondaytwo]]>>


:: backatescapepod
回到我的逃生舱所在地了。看来它在晚上翻了几个跟头。
我猜是风吹动降落伞把它拖开了点？
我的妈，这风力可真不小啊。大概移了十英尺。
哦，这只不过是……多少来着……三米远？三米听起来一点都不远。
公制单位下听起来没那么吓人了……
[[delay 40m|deadendmountaindaytwo]]



:: deadendmountaindaytwo
<<if $boulderdayone is 0>>
糟糕！好像没路了。
我本来顺着这个小峡谷走的，结果遇到块巨大的石头。太大了，爬都爬不过去。
<<elseif $boulderdayone is 1>>
坑爹啊……同一个峡谷，不同的方向。
我又走到了昨天大石头挡路的地方了。
但它一点也不比昨天小，（正常的，）而且我还是爬不过去。<<endif>>
你怎么看？是就近找条道呢？
还是返回到峡谷外面重新找路？
<<choice [[在附近找路。|wayaroundboulderdaytwo]]>> | <<choice [[退出峡谷。|backoutofcanyondaytwo]]>>



:: backoutofcanyondaytwo
好，我听你的。我觉得这样才对。
那块圆石头看起来就像是等着我去送死的陷阱一样。
往回走几百码远的山壁上有一处缓坡能上去。
（糟糕，我算不清楚公制了。百束？光年？)
我讨厌浪费时间原路返回……
不过总比“在离最近的医院5000光年外摔断腿”这个剧本可好多了。
总之，我走了。
[[delay 45m|craterlip]]



:: craterlip
好吧……我遇到了一个只有困在外星球上的人才会遇到的难题。且听我细细道来。
我正站在个巨大的陨石坑的边缘，我是说，巨！大！
要不是我怕随时孤死外太空的话，我一定会花点时间来欣赏一番的。
问题是，这个地理奇观偏偏挡住我的路了。
所以，是要么沿着边缘艰苦地绕上半圈……
要么试试慢慢滑下坑去，再从中间穿过去。
两点之间当然是直线最短……
不过一般人也遇不到隔着巨型陨石坑的两点。
你怎么看？
<<choice [[横穿过去。|straightacross]]>> | <<choice [[包抄过去。|walkperimeter]]>>



:: straightacross
遵命！我先找个最缓的坡。
马上回来。
[[delay 5m|slippedcrater]]



:: slippedcrater
<<silently>><<set $hurtankle = 1>><<endsilently>>
好了，哎，不太顺利啊。
本来一切都挺好的，突然我脚下一空。
（现在回想起来，这倒是挺适合描述我在地球老家时的大部分人际关系的。）
看起来坚硬的坑壁其实是沙子。
我的脚穿过沙子，整个人栽了进去，然后……嗯。
好消息：我成功到达了坑底！
坏消息：我的踝关节扭炸了。我觉得肿起来了。
好消息（算不算？）：要是真有外星人在看着我，我肯定可以进他们的“荒星囧视”的决赛。
坏消息：我走直道儿省的时间都因为受伤浪费掉了。
<<if $hurtshoulder is 1>>
要是地球上有谁在跟进……
报告一下：我现在肩膀脱臼(1)，受伤脚踝(1)。<<endif>>
我得要么继续朝陨石坑的另一头走……
要么现在往上爬回去，然后再从边缘绕过去。
<<choice [[继续前进。|keepmovingaheadcrater]]>> | <<choice [[往上爬回去。|climbbackupcrater]]>>



:: keepmovingaheadcrater
再没有更合理的决定了。
这趟长征会很无聊，一路上只有我对着我的伤腿骂骂咧咧。
要是你不介意，我先把通讯关了，免得烦你。
如果发生什么神奇的事情，我肯定会联系你的。
[[delay 45m|anythingamazing]]



:: anythingamazing
哦，天啊！说了你也不信！
我还困在陨石坑里，而且我的脚还很痛！
……不好意思。除了我满脑子的胡思乱想的声音，这儿安静得要把我逼疯了。
<<choice [[没事儿，想聊聊吗？|noworrieswannachat]]>> | <<choice [[对不起，没时间聊。|sorrycantchat]]>>



:: noworrieswannachat
好啊！求之不得。我是说，我不想打扰你，但我基本上就只有你了。
（哇喔，我怎么混得这么惨了？）
那么，嗯……你有没有在巨型陨石坑里面转悠过？
<<choice [[当然啦，经常的事。|allthetimeincraters]]>> | <<choice [[从来没走过这种狗屎运。|neverincraters]]>>



:: allthetimeincraters
哦，是吗？我可是头一回。
你说不定能给我讲点旅行小贴士。
你觉得这附近哪里能找到杯提神的摩卡咖啡？早上不来一杯，我一天都没精神。
让我没精神的事还包括：在一个巨型陨石坑里乱转。
哎。
<<choice [[加油，别松劲儿！|keepwanderingcrater]]>> | <<choice [[我想喝咖啡去了。|iwantcoffee]]>>



:: keepwanderingcrater
好吧，听你的。我能看到陨石坑的另一边了。
我要咬紧牙关，倾尽全力，瘸着我的腿朝那边去了。
谢谢你给我打气。我一会儿跟你联系。
[[delay 20m|oppositewall]]



:: oppositewall
好吧，穿过坑底花的时间没我想象的长。
我脚踝崴了，走路很痛，不过我还受得了。
不过这里有件怪事儿：刚才我走路时被什么东西绊倒了，离我现在的位置大概50码开外。
（哎，我知道，我这一路上凌波微步，你简直不敢相信我会被绊倒吧。）
总之，那玩意儿差不多全埋在了沙里，不过当我低头看时……它正在阳光下闪闪发光呢。
因为那是块金属。
把它挖出来没费什么事，然后我就一直在琢磨它，不过我还是不确定它是啥。
我的意思是，我猜它肯定是瓦里亚号上的一块板子……只是太奇怪了，它怎么会离飞船的残骸这么远？
总之，我找到了一道缓坡，我打算试试爬出去，我得全神贯注才行。
我爬上去再联系你，行吧？
[[delay 10m|moremetalpieces]]


:: wayaroundboulderdaytwo
好吧，我去试一试。不过说实话，这里的地形可不饶人啊。
[[delay 20m|deadendforrealdaytwo]]



:: walkperimeter
好吧，这应该是最安全的方法了。这上面都是平地。
平淡无聊的一马平川，喊一嗓子，喂——喂——
瞧瞧，我要为接下来几个钟头，做出最激动人心的抉择了：往左走还是往右走？
由于我手头没硬币抛（因为这边既没自动贩卖机也没电玩城）……
……还是你来决定吧！
我是该绕着这个笨坑顺时针走呢？还是逆时针走？
这可能是你今天最重要的决定！
（不会的。）
<<choice [[顺时针！|clockwisecrater]]>> | <<choice [[逆时针！|counterclockwisecrater]]>>



:: deadendforrealdaytwo
<<if $deadenddayone is 0>>
听着，这附近根本就没有出路。
石头上也没有踩脚的地方，那怕往上爬几尺都很难。哦，该说是“几米”，无所谓了。
峡谷壁上的石头看起来都松松垮垮的。
哎……泪奔……科技大赛冠军的我，就是“宅男”的代名词。
<<elseif $deadenddayone is 1>>
听着，就像我昨天说的，这附近根本就没有出路。
那块大石头太光滑了，但是峡谷壁上的石头又太松动。<<endif>>
我不是攀岩的那块料。
你怎么看？
<<choice [[退出峡谷。|backoutofcanyondaytwo]]>> | <<choice [[试试爬一下吧。|tryclimbingdaytwo]]>>



:: climbbackupcrater
啊，如果使把小劲儿，——好吧，可不是小劲儿，——我可以通过坑内壁里更坚固点的地方往上退回去。
居然想抄近道，真糗啊。这恐怕就是我应该付出的代价吧。
教训就是：凡是值得做的事情，都值得用你能想到的最沉闷无聊、单调无趣的方法做。
我要专心往上爬回去了，一会儿再联系你。
[[delay 5m|walkperimeter]]


:: sorrycantchat
好吧！好吧！我懂。
总之，我能看到陨石坑的另一边了。我要咬紧牙关，倾尽全力，瘸着我的腿朝那边去了。
我一会儿跟你联系。
[[delay 20m|oppositewall]]


:: neverincraters
想象一下，宇宙里面最大的一口锅里的一粒米。我就是那粒米。
我有点担心会有一颗巨型豌豆从天而降把我砸死。
（这种恐惧太不理性了……我知道……可是，两天前，我还不会觉得“宇宙飞船坠毁”是非常理性的恐惧呢。）
总之，我能看到陨石坑的另一边了。我要咬紧牙关，倾尽全力，瘸着我的腿朝那边去了。
谢谢你给我打气。我一会儿跟你联系。
[[delay 20m|oppositewall]]


:: iwantcoffee
你知道吧，你是个混蛋。
去给自己拿一杯吧。也许还可以倒出来点，分给你那在宇宙十字路口游荡的小伙伴泰勒。
我呢，我能看到陨石坑的另一边了。我要咬紧牙关，倾尽全力，瘸着我的腿朝那边去了。
谢谢你给我打气。我一会儿跟你联系。
[[delay 20m|oppositewall]]


:: moremetalpieces
额，这里有些东西。
在坑的这边，有一个小山脊。
在那个山脊上面，还有更多小金属块。
额，好几块。
然后，就在山脊的另一边——要登上山脊顶上才看得到——
有……嗯……
……有一艘飞船！
[[delay 1m|discoveredship]]


:: taylorfallsdaytwo
哎哟，我的腿，我的腿摔断了
哦，天啊，是开放性骨折，骨头把宇航服都都刺穿了。
[[delay 4s|brokenlegdaytwo]]



:: tryclimbingdaytwo
我真的……噢，不管了。
我都已经走到这里了，没理由放弃的。上！
[[delay 30m|taylorfallsdaytwo]]



:: counterclockwisecrater
哇喔！逆时针？真是一个大胆的决定！
在一个人迹罕至的宇宙象限里的未知卫星上，绕着一个巨大的圆圈，我要向右出发前进了……
……我相信你那强有力的小宇宙爆发出的决定果然与众不同！
顺时针简直弱爆了！逆时针才是王道！逆流而上的人才能遇上最好的鱼！
[[delay 4s|craterenthusiasm]]


:: clockwisecrater
<<silently>><<set $clockwisecrater = 1>><<endsilently>>
哇喔！顺时针？真是一个大胆的决定！
在一个人迹罕至的宇宙象限里的未知卫星上，绕着一个巨大的圆圈，我要向左出发前进了……
……我相信你那强有力的小宇宙爆发出的决定果然与众不同！
逆时针简直弱爆了！顺时针才是王道！没有顺时针，钟就没啥用了！数字钟不算！
[[delay 4s|craterenthusiasm]]


:: sorrycantchatperimeter
好吧！好吧！我懂。
总之，我能看到陨石坑的另一边了。我要咬紧牙关，倾尽全力，潇洒地朝那边去了。
谢谢你给我打气。我一会儿跟你联系。
[[delay 40m|oppositeedge]]


:: neverincratersperimeter
想象一下，宇宙里面最大的一口锅的锅边上的一粒米。我就是那粒米。
我有点担心会有一颗巨型豌豆从天而降把我砸死。
（这种恐惧太不理性了……我知道……可是，两天前，我还不会觉得“宇宙飞船坠毁”是非常理性的恐惧呢。）
总之，我能看到陨石坑的另一边了。我要咬紧牙关，倾尽全力，潇洒地朝那边去了。
谢谢你给我打气。我一会儿跟你联系。
[[delay 40m|oppositeedge]]


:: keepwanderingcraterperimeter
好吧，就这么定了。
我能看到陨石坑的另一边了。我要咬紧牙关，倾尽全力，潇洒地朝那边去了。
谢谢你给我打气。我一会儿跟你联系。
[[delay 40m|oppositeedge]]


:: oppositeedge
好吧，其实绕着坑沿儿走花的时间没我想像的长。
<<if $hurtankle is 1>>我脚踝崴了，走路很痛，不过我还受得了。<<endif>>
不过有件怪事儿：刚才我走路时被什么东西绊倒了，离我现在的位置大概50码开外。
（哎，我知道，我这一路上凌波微步，你简直不敢相信我会被绊倒吧。）
总之，那玩意儿差不多全埋在了沙里，不过当我低头看时……它正在阳光下闪闪发光呢。
因为那是块金属。
把它挖出来没费什么事，然后我就一直在研究它，不过我还是不确定它是啥。
我的意思是，我猜它肯定是瓦里亚号上的一块板子……只是太奇怪了，它怎么会离飞船的残骸这么远？
总之，我差不多绕了半圈了，我觉得可以朝北方进军了。
<<if $clockwisecrater is 1>>真是一段又长又怪的顺时针旅程啊。
<<else>>真是一段又长又怪的逆时针旅程啊。<<endif>>
[[delay 5m|moremetalpieces]]


:: discoveredship
好了……幸好这里的空气可供呼吸，因为我可是大喘了将近一分钟呢。
什么东东？我的意思是……我是说，什么东东？对，我说的是，什么东东？
（我是理科生，语文不好。请原谅。）
从这里俯看下去，我可以看到它比瓦里亚号要小多了……我想就是一般所说的卡拉维尔型小飞船。
大概最多能载六个船员，速度很快，但武器很少。
防御力低下。
它在这儿干啥？它在这儿多久了？
引擎看起来就像是……从船体上撕裂下来的。撞上太空垃圾了？
天呐，我大概有一百万个问题。
是应该下去探索一下残骸呢？
还是应该留在高处，离这艘船和不管是什么把它弄下来的东西远远的？
<<choice [[去探索残骸。|explorecaravel]]>> | <<choice [[离它远一点。|stayclearofcaravel]]>>


:: brokenlegdaytwo
我这辈子都没断过根骨头，一来就是个这么猛的
哎哟，痛死了，我怎么还没休克啊？
[[delay 4s|bad advicedaytwo]]



:: bad advicedaytwo
嘿，无意冒犯，但是我不得不说，你的建议真是弱爆了。
[[delay 4s|connectionlost]] 



:: craterenthusiasm
……嗷！兄弟啊，我想我已经用光了我所有的激情，可是还有好远的路要走。
等我走了一半的路再联系你。
要真出了什么奇人奇事，我保证，我会告诉你的。
[[delay 45m|anythingamazingperimeter]]


:: anythingamazingperimeter
哦，天啊！说了你也不信！
我依然在陨石坑里跋涉，而且这里依然无聊至极！
……不好意思，这儿太安静了，我自说自话，都要疯了。
<<choice [[没事儿，想聊聊吗？|noworrieswannachatperimeter]]>> | <<choice [[对不起，没时间聊。|sorrycantchatperimeter]]>>


:: noworrieswannachatperimeter
好啊！求你了。我是说，我不想打扰你，但我基本上就只有你了。
（哇喔，我怎么混得这么惨了？）
那么，嗯……你有没有沿着巨型陨石坑边缘转悠过？
<<choice [[当然啦，经常的事。|allthetimeincratersperimeter]]>> | <<choice [[从来没走过这种狗屎运。|neverincratersperimeter]]>>



:: allthetimeincratersperimeter
哦，是吗？我可是头一回。
你说不定能给我讲点旅行小贴士。
你觉得这附近哪里能找到杯提神的摩卡咖啡？早上不来一杯，我一天都没精神。
让我没精神的事还包括：在一个巨型陨石坑里乱转。
哎。
<<choice [[加油，别松劲儿！|keepwanderingcraterperimeter]]>> | <<choice [[我想喝咖啡去了。|iwantcoffeeperimeter]]>>



:: iwantcoffeeperimeter
王八蛋。
去给自己拿一杯吧。也许还可以倒出来点，分给你那在宇宙十字路口游荡的小伙伴泰勒。
总之，我能看到陨石坑的另一边了。我要咬紧牙关，倾尽全力，潇洒地朝那边去了。
谢谢你给我打气。我一会儿跟你联系。
[[delay 40m|oppositeedge]]


:: stayclearofcaravel
对啊，这明显最合情合理。
如果这是部恐怖科幻烂片的话，我一定会冲着屏幕大叫，让那个脑残孩子不要下到残骸里去的。
我看片儿时应该还嚼着爆米花。
比起困在荒无人烟的星球上决定要不要探索一艘废弃的飞船，我更想吃爆米花。
说实话我从来没想过会面临这种抉择。
不过没错，这就是所谓明智，对吧？就待在这上头，离那艘飞船远远的，继续向山峰前进。
<<choice [[对，前往山峰。|postcaravelassessment]]>> | <<choice [[要不去飞船看看吧。|checkcaravelafterall]]>>


:: explorecaravel
我简直是疯了。我居然在朝那艘废弃的太空船进发，而不是躲得越远越好……真是疯了。
哪一部恐怖片里没这么一个白痴……
我每回看的时候还会感叹一番：“换了我，打死也不干。”
只不过现在，我就是那个白痴。
说真话，在那个躲在桂花树后面戴着太空曲棍球面具的杀手跳出来干掉我之前，我是不是应该先跑路啊？
那个杀手说不定会留下我的小命，让我陪着玩太空曲棍球？还不如让我死呢！体育课对我来说就是酷刑！
<<choice [[好吧，回头吧。|turnbackfromcaravel]]>> | <<choice [[不，你应该继续探索。|goexplorecaravel]]>>


:: goexplorecaravel
好吧，好吧，不过要是我在那里遇到了连环杀手，那都是你的错。那怕我只是残了，我也会怒的啊。
给我一点点时间下去。
[[delay 3m|atcaravel]]


:: atcaravel
太令人吃惊了，爬下山脊的过程好顺利。简直过！于！顺利了。
我实在是好恨我生命这一刻发生的一切。
嗯，船体已经严重受损。
这里有些不太好认的字迹……看起来像中文，或者是日语？漆掉了太多，辨认不出来了。
不过，铁皮还没有完全生锈。这艘船在这里没多久。
啊。光看看飞船的外壳都够让我的胃里翻江倒海了。我满头大汗，同时又寒毛直竖。
<<if $rations is 1>>要么因为早上的甜香肠不是这世界上最美味的东西（不可能！）……
……要么因为这是我遇到过的最毛骨悚然的事。<<endif>>
气闸门的开口好像刚好够我爬进去。
不过我在满地找借口不进去呢。
<<choice [[你必须继续探索。|goinsidecaravel]]>> | <<choice [[听起来怕怕的，回去吧。|dontgoinsidecaravel]]>>


:: goinsidecaravel
好吧，深呼吸。飞船里伸手不见五指，我在打开宇航服的头灯。
（这里面当然漆黑一片。要不黑，我会被吓尿吗？）
<<if $hurtshoulder is 1>>我跟你说吧，带着一根脱臼的肩膀爬过那道门可一点也不好玩。
<<elseif $hurtshoulder is 0>>为了挤进来我扭了下肩膀，那滋味……不是盖的。<<endif>>
看起来这艘船坠落时晃得很厉害。满地都是柜子里筐子里掉出来的东西……
只靠一个头灯在一片狼藉里探路，这得花不少时间。
所以，我现在能告诉你的是：仪表盘碎成片片了，然后这些片片又碎成渣渣了。
我曾经幻想这里面有一个能用的失事信标，不过幻想果然是幻想。
好伤心，现在我站的地方像是一条走道。你觉得我该去东边的大厅还是西边的呢？
<<choice [[试试东边的大厅。|eastcaravel]]>> | <<choice [[去西边的大厅。|westcaravel]]>>


:: checkcaravelafterall
你说真的吗？我满脑子都想着“不要下去”。我真觉得那里有什么东西。
啊唷！你真的觉得我应该去看看？
[[goexplorecaravel]]


:: turnbackfromcaravel
对，对，这个计划最安全。明智之选。
我还没听说过谁因为不进星际杀手的老巢，丢了小命的。
至少我看过的星际谋杀的电影里面没有这种剧情，而且我可是看过不少了。（你懂的，体育运动不是我的菜。）
我还是把这令人毛骨悚然的残骸抛在脑后，向着那座令人毛骨悚然的山峰继续前进吧。
（……我在这边大声念这句话，感觉更是毛骨悚然啊！）
[[postcaravelassessment]]


:: dontgoinsidecaravel
对，对，这个计划最安全。明智之选。
我还没听说过谁因为不进星际杀手的老巢，丢了小命的。
至少我看过的星际谋杀的电影里面没有这种剧情。而且我可是看过不少了。（你懂的，体育运动不是我的菜。）
我还是把这令人毛骨悚然的残骸抛在脑后，向着那座令人毛骨悚然的山峰继续前进吧。
（……我在这边大声念这句话，感觉更是毛骨悚然啊！）
好了，我要忘掉曾经见过这玩意儿（不大可能），继续往前走（很有可能）。
[[postcaravelassessment]]


:: eastcaravel
<<silently>><<set $compassweird = 1>><<endsilently>>
向右走，我们就进入可爱而宽敞的（真心不觉得）东边走廊。
我两边的门都紧闭着。
没有动力驱动气动阀的话，我根本不可能把门打开，所以我想……
唔，奇怪。
我刚刚注意到我的宇航服上的指南针还显示着我面朝北方，可是我已经转了个直角弯朝右边走了啊。
这……有点乱套了。
<<choice [[继续直走。|aheadincaravel]]>> | <<choice [[往回走。|turnaroundincaravel]]>>


:: westcaravel
<<silently>><<set $compassweird = 1>><<endsilently>>
向左走，我们就进入可爱而宽敞的（真心不觉得）西边走廊。
我两边的门都紧闭着。
没有动力驱动气动阀的话，我根本不可能把门打开，所以我想……
唔，奇怪。
我刚刚注意到我的宇航服上的指南针还显示着我面朝北方，可是我刚才已经转了个直角弯朝左边走了啊。
这……有点乱套了。
<<choice [[继续直走。|aheadincaravel]]>> | <<choice [[往回走。|turnaroundincaravel]]>>


:: getoutofcaravel
好吧，那我就假装没发现我的方向读数出了问题……
……假装没发现我的头灯莫名其妙地挂掉了……
……假装没想到，现在这一幕要是有背景音乐的话，那配的一定是尖锐的小提琴声和沉重的呼吸声……
……我只要沿着来的路回去就好——
[[delay 4s|justleavingcaravel]]


:: turnaroundincaravel
对，有道理。这要是部我正在看的电影的话，我现在一定在冲着屏幕大叫。
我跟你说个有意思的小细节吧，我转了个180度的大弯，踏上了来时走的路……
结果指南针转了几圈，然后又指回了“北”。
也就是说，我的指南针在这堆废墟里失灵了。
实话实说，这可真没我让对这个地方有一点点好感啊！
<<choice [[离开这个鬼地方。|getoutofcaravel]]>> | <<choice [[继续在里面探索。|keepexploringcaravel]]>>


:: aheadincaravel
如果我是在看一部恐怖片，里面出现我这个角色还在朝里头走，我肯定会觉得那个我是个大白痴。
我几乎可以听见屏幕外的我在对着继续前进的我尖叫。
“你的指南针是渣渣！”我在对自己咆哮，“你连路都看不见！”
“你的头灯开始闪了，因为你是恐怖烂片里的烂角色！”
哦，杯具了。我的头灯开始闪了。
<<choice [[好吧，回头吧。|turnaroundincaravel]]>> | <<choice [[继续前进，走快点。|fasterincaravel]]>>


:: justleavingcaravel
<<silently>><<set $zombierats = 1>><<endsilently>>
我嘞个去！那是什么？
哦，天啊。哦，天啊，
唔，有什么……我说不上来，好像是什么嗖嗖窜的声音。从我后面，跑到走廊外面去了。
然后，我转了个身……我，我……我看到了一些东西。会动。
在发光，是绿光。
有很多，那种小东西。贴着地面。在发光、在移动，跑得飞快。
实话实说，我今天见到了最！不！想！见到的东西。
我不认为在这艘飞船上我是孤身一人。
我要离开这个鬼地方。
<<choice [[我同意。马上离开！|leavecaravelnow]]>> | <<choice [[看看是什么东西。|findscuttling]]>>


:: fasterincaravel
呵呵，你的想法真妙哈！“喂，你在干什么蠢事？好吧，那就快点干！”
这种逻辑特别灵，对谁都适用。
大厅的尽头有一扇开着的门。我朝那儿去了。
我的头灯在狂闪，简直跟迪厅一样。我觉得随时都会来一声贝斯重响。
也许今晚会有个DJ救我一命。
噢哟。要挤进这道门进到后面房间实在不太容易。
我最好指望里面不要有自助餐。如果我再多长一两肉，我就再也不能挤回去了。
[[delay 1m|insidemedroom]]


:: takenopill
对，好吧，说不定这才是明智之举。我会暂时留住这些药的。
如果有谁问我这两天学会了什么道理，那就是一切没有最糟，只有更糟。
杯具了……我的头灯在狂闪。我想趁灯没全坏，我得赶快闪人。
<<choice [[好吧，离开那里。|leavecaravelmedroom]]>> | <<choice [[冒险试试，继续探索。|riskitcaravelmedroom]]>>


:: insidemedroom
<<silently>><<set $pills = 3>><<endsilently>>
哎呀……他们该送个练过缩骨功的来，而不是我这种理工宅男。
好了，我进来了。这里像是个非常简陋的医务室。我这么说是因为我在这里找到了医疗包。
医疗包里有个瓶子——很大——上面标着中英文，所以我敢肯定这里头是止痛药。
我本来想说：哇塞！太棒了！我的肩膀痛得像活不到明天了。
（字面意义的：活不到明天。我也是醉了。）
可惜，瓶子快空见底了。要是装满了药片，这玩意儿倒是动听的乐器——哎但只剩下三片药片了。
我不想在别人背后说坏说，不过，这艘飞船上肯定有人麻烦不小。
光顾着嗑药，也不管管飞船，看吧，掉下来了吧。
（不对，那样的想法太黑暗了。不能这样想了。）
<<if $hurtankle is 1>>肩膀痛、脚脖子痛，我决定现在就用一片止痛药让自己好过一点。
<<elseif $hurtankle is 0>>肩膀疼死我了。我决定现在就用一片止痛药让自己好过一点。<<endif>>
但话说回来，一切没有最糟，只有更糟。我还是先留着这些药吧。
<<choice [[现在吃一片药。|takepillone]]>> | <<choice [[暂时先保留。|takenopill]]>>



:: leavecaravelmedroom
好吧，现在，我要返回主走廊，挤过来时的门，出到这艘怪船的外面去了，免得……
[[delay 15s|whatthehellwasthat]]


:: takepillone
<<silently>><<set $pills = $pills - 1>><<endsilently>>
对，没错儿！别自讨苦吃了。
我手上还有<<$pills>>片药，以后可以用。
杯具了……我的头灯在狂闪。我想趁灯没全坏，我得赶快闪人。
<<choice [[好吧，离开那里。|leavecaravelmedroom]]>> | <<choice [[冒险试试，继续探索。|riskitcaravelmedroom]]>>


:: riskitcaravelmedroom
<<silently>><<set $glowrods = 10>><<endsilently>>我真的要吓尿了，不能再多呆——
嘿！瞧瞧！这有一整抽屉的荧光棒！
好吧，你绝不可能知道这里会有荧光棒，但我还是要感谢你劝我留下来……果然是正确的选择。
我折亮了几根荧光棒狂甩，简直像它们欠我一辈子的命一样。现在真的很像在开演唱会了！
而且我还有十多根荧光棒以后能用。简直完美！
突然之间我不再那么害怕黑暗了——
[[delay 4s|whatthehellwasthat]]


:: keepexploringcaravel
绝对是馊主意。记下来啊。
那样的话，要是有人发现我被太空电锯什么的锯成两截的尸身，我可以来一句“我说了吧”。
（嘿，我都要死了。能得意几回算几回。）
好了，好了，我回到了主走廊，一片狼藉，我尽量小心，免得
[[delay 1m|ginnyincaravel]]


:: findscuttling
你逗我呢？我可没有那种见啥追啥的天性！
说真的，你是没听到。我可听到了。那种恐怖的声音，似乎就在说“不要在黑暗中追着我来”。
吓人指数：五颗星！要是你想看评分的话。
<<if $ginnycaravel is 0>><<choice [[说真的，你去瞧瞧吧。|keepexploringcaravel]]>> | <<choice [[好吧，离开那里。|leavecaravelnow]]>>
<<elseif $ginnycaravel is 1>>我打算带上发电机离开这儿。 [[delay 1m|leavecaravelnorats]]<<endif>>


:: whatthehellwasthat
<<silently>><<set $zombierats = 1>><<endsilently>>
我嘞个去！那是什么？
哦，天啊。哦，天啊，
嗯，有什么……我说不上来，好像是什么嗖嗖窜的声音。从我后面，跑到走廊外面去了。
然后我转了个身……我，我……我看到了一些东西。会动。
在发光，是绿光。
有很多，那种小东西。贴着地面。在发光、在移动，跑得飞快。
实话实说，我今天见到了最！不！想！见到的东西。
我不认为在这艘飞船上我是孤身一人。
我要离开这个鬼地方。
<<choice [[我同意，马上离开！|leavecaravelnow]]>> | <<choice [[看看是什么东西。|findscuttling]]>>


:: liedownanddievaria
唔。听着。我只是……只是太累了。我现在就想躺下。
也许我只是，你知道的，只是喜欢夸张。也许，我在这里不会有事儿。
只要睡一觉，我就会像洗了八个钟头的冷水澡那样清醒。
只是，嗯……万一我没醒过来的话……
我想对你说声谢谢。感谢你用声音在黑暗中一直陪着我。
这是我所经历过的最深沉的黑暗，所以……我真的很感激你。
这里真的好美，看天上这些星星。成千上万，成千上万的星星。
你要是能看到该有多好。
我……稍微合会儿眼。
[[delay 8s|connectionlost]]


:: ginnyincaravel
<<silently>><<set $ginnycaravel = 1>><<endsilently>>
对不起。我被什么东西绊倒了，摔得疼死我了。
唔，不知道你从我的字里行间能读到什么，在太空中，你摔倒了是绝对听得到自己的尖叫的。
确实有点囧。
<<if $ginny is 1>>但好消息是，绊倒我的东西？是另一台小型发电机！
这跟我在瓦里亚号上给<<$power>>供电的那台型号不同，要小些轻些。
不过不管怎么样，现在我的电量供应加倍啦！
<<elseif $ginny is 0>>但好消息是，绊倒我的东西？是一台小型发电机！
这意味着我终于有电啦！<<endif>>
所以我可以说勇闯夺命船取得成功，可以收兵了！
我要带着发电机离开这个鬼窟了。
<<if $zombierats is 1>>[[delay 1m|leavecaravelnorats]]
<<else>>也许我看的恐怖片太多了。所以瞎操心了！[[whatthehellwasthat]]<<endif>>



:: leavecaravelnorats
啊，出来了！要不是怕水土不服，我简直要亲一口这里的泥了。
我不知道那是什么。可能……可能什么都没有。只是我的幻觉。
要是我好不容易从飞船坠毁中挺过来，结果被暗影里跳出来的东西吓得心脏病发而死了，就太杯具了，对吧？
我想在这里坐一会缓口气，好吧？
[[delay 10m|caravelrecovery]]


:: leavecaravelnow
我出来了！我出来了！
恐惧真是个给力的激励因素，能让你穿越狭小的空间，还能让你忘记肩膀的伤痛。
恐惧让你顾不上那破指南针。
恐惧让你忘记一切，一心回到外面……
尽管外头也是一片蛮荒，却是整个宇宙中最舒适的所在了。
啊，天哪！要不是怕水土不服，我简直要亲一口这里的泥了。
我不知道那是什么。可能……可能什么都没有。只是我的幻觉。
要是我好不容易从飞船坠毁中挺过来，结果被暗影里跳出来的东西吓得心脏病发而死了，就太杯具了，对吧？
我想在这里坐一会缓口气，好吧？
[[delay 10m|caravelrecovery]]


:: deathmarchvaria
嘿，天仓星很快就要落山了。是死寂的日落。
在这颗卫星上的死寂的日落时分，我孤苦伶仃。
天色变得非常暗了。
<<if $glowrods gte 1>>我刚折了几根荧光棒。我觉得有点用。
但是亮光持续不了多久。照明时间可能都不够我找个安全的地方。<<endif>>
我觉得……我觉得我快玩完了。
我现在离瓦里亚号远着呢，新发现的小飞船也不近，想去的山峰也没走到，到哪里都很远！
我现在完全暴露在星空下。星空确实很美……但气温真的很低。
<<if $bringginnytwo is 1>>我身边只有一个发电机，但是没有能连发电机的设备。好无助啊。<<endif>>
气温在降低了，还会降得更低，然后……
我多半要死在这里了，死在这片美得令人窒息的星空下。
该死。我渴得连哭都哭不出来了。
这颗作孽的卫星连我哭的权利都剥夺了。
[[delay 2m|liedownanddievaria]]


:: dehydrated
嘿，天仓星很快就要落山了。是死寂的日落。
在这颗星球上死寂的日落时分，我孤苦伶仃。
真是好美啊，如果我没
啊
嘿
呼
有点……抽……
搐……
嗯嗯嗯…
[[delay 8s|connectionlostdehydrated]]


:: caravelrecovery
好了，好多了。
我要先评估一下当前的状态再考虑继续前进。
[[postcaravelassessment]]


:: pushontovaria
我现在也分不清倔脾气和失心疯的区别了。
哈！“失星疯”。我在星星上呢。
不管你身在何处，我都希望你笑一下。这可能是我讲的最后一个笑话了。
[[delay 15m|deathmarchvaria]]


:: variaalltheway
感觉好无助。
我现在就只是机械地将一只脚迈到另一只脚前面，哪儿都走不到。
只是在走。向南走。我觉得我在向南。
这颗卫星已经剥夺了我的一切，甚至基本的方向感。
一步，再一步。
一步接一步。
不行，这太蠢了。我觉得我应该回到那个小飞船里，再晚就来不及了。
<<choice [[返回小飞船。|returntothecaravel]]>> | <<choice [[向瓦里亚号前进。|pushontovaria]]>>


:: firstcheckin
感觉……原路回去比来时花的时间更久。
<<if $rations is 0 and $ratpellets is 0>>[[nofoodcollapse]]
<<else>>天仓星正越来越低。也许返回瓦里亚号太不明智了。
<<choice [[向瓦里亚号前进。|variaalltheway]]>> | <<choice [[返回小飞船。|returntothecaravel]]>><<endif>>


:: nofoodcollapse
嘿，嘿，我的腿，已经没劲了。
它们罢工了。我说“快走”，它们说“不”。
我亲爱滴腿啊！
[[delay 4s|dehydrated]]


:: postcaravelassessment
感觉我已经朝北走了很远了，可是还没走到山峰。
从这里，我看得到前进的方向上还有几个陨石坑，小一些，不过跟我之前经过的那个完全不一样。
<<if $rations is 1>>可能该先停下来吃点东西喝口水，补充点能量，然后再动身。
<<elseif $rations is 0 and $ratpellets is 1>>可能该先停下来填下肚子，然后再动身，
——要是你觉得鼠粮和老鼠饮水器里的水能填肚子的话。
<<elseif $rations is 0 and $ratpellets is 0>>我还没有找到一点吃的——甚至连更要紧的水也没有，真是作死啊。
我头痛欲裂，肌肉也要快要不听使唤了。
<<if $pills gte 1>>我还有<<$pills>>片止痛药。我可以干吞一片，但是我真心不想空腹吃。<<endif>><<endif>>
<<if $hurtshoulder is 1 or $compassweird is 1>>肩膀的伤势非常严重。
<<if $hurtankle is 1>>而且我的脚脖子也不见起色。有谁比我惨啊？！<<endif>><<endif>>
<<if $ginnycaravel is 1>>我现在有一个小发电机。问题是，我要一直拖着它去山峰吗？
还是把它留在小飞船这里，等我返回瓦里亚号的时候再拿？
（我真是乐观啊，居然觉得我能活着回瓦里亚号呢。泰勒加油。）
<<choice [[把发电机留下。|leaveginnyatcaravel]]>> | <<choice [[把发电机带上。|takeginnywithyou]]>>
<<else>>[[finishassessment]]<<endif>>



:: returntothecaravel
必须相信掉头返回，才能走到对的地方。
我的指南针的精度完全靠它自己的心情。
好了，我要回小飞船去了。累成狗，侃不动了。到那里之前我先把通讯器关了。
“不！不要！”我听到你抗议了。
你是不是觉得，没了我这奇葩的人品和精彩的解说，你一个钟头都撑不下去啦？
我看，你可以试试再找个流落在别的蛮荒星球上的人，聊一聊。
但要记住，我是第一个，啊？
[[delay 60m|oncemoreatcaravel]]



:: keepgoingtovaria
对，我知道那里有一台能取暖的反应器。
我得快点。有四个多钟头的路要赶呢。
<<if $rations is 0 and $ratpellets is 0>>特别还是饿着肚子走。<<endif>>
好了，我出发了。一会儿再联系。
[[delay 60m|firstcheckin]]


:: leaveginnyatcaravel
<<silently>><<set $bringginnytwo = 0>><<endsilently>>
对，好主意。我也觉得没必要浪费体力拖着这玩意儿到处跑。
况且它又没长脚……
……况且我还不一定能活着回来，那样我就更用不上发电机了。
（还有一种可能，就是我回来之后发现发电机不见了——那样的话，我要担心的可就不止是发电机了。）
[[finishassessment]]


:: takeginnywithyou
<<silently>><<set $bringginnytwo = 1>><<endsilently>>
对，有道理，虽然我的速度会减慢一点，但长远看可能值得这么做。
比方说，到达山峰后，我可能会需要用到它……
……或者——我尽量不这么想——如果我把它留在小飞船里，
万一我回去后，发现发电机不见了。要是那样的话,我就有大！麻！烦！了。
[[finishassessment]]


:: finishassessment
哎，现在操心这个没用。
我简直是浪费时间，而且我还不知道我总共有多少点时间。
算了……我先闪了。
这一路可要花不少时间。如果你不反对的话，我要关一会儿通讯器专心行进了。
过一会再和你聊。
[[delay 90m|stillhikingtowardpeak]]



:: oncemoreatcaravel
我回这艘恐怖的鬼船来了。不知道为什么，这居然变成了最有吸引力的选项。
[[makecampatcaravel]]


:: backatcaravel
我又能看到小飞船了。我从没有想过，又看到这鬼屋一样的星际飞船，我居然会觉得庆幸。
<<if $ginnycaravel is 1 and $bringginnytwo is 0>>发电机就在我原来放的位置——它正该在的地方。
因为——还有谁会拿它呢？这颗难看得要死的白石头星上，除了我又没有别人了。对吧？就是。<<endif>>
我要累趴了。
<<if $rations is 1>>按说瓦里亚号上的食物还够我维持生存……
……不过，我觉得，那些东西可不是为我这种高强度户外生活方式而准备的——自从流落到这里，我一直享受的就是这种生活。
<<elseif $rations is 0 and $ratpellets is 1>>鼠粮提供的卡路里仅够保证我苟延残喘……
……但绝对不是为人的生存或者口感而设计的。
更不是为我这种高强度户外生活方式而准备的——自从流落到这里，我一直享受的就是这种生活。
<<elseif $rations is 0 and $ratpellets is 0>>我连着两天除了燃烧卡路里啥也没吃。
我嗓子快冒烟了，肚子像填了砂纸，脑袋要痛死了。<<endif>>
如果我没记错的话，从这里出发回瓦里亚号要四个多钟头呢。
起码我来的时候花了那么久。而且我当时还有个靠谱的指南针。
现在我宇航服的头灯已经完全罢工了。
<<if $glowrods is 1>><<set $plural = "">><<else>><<set $plural = "">><<endif>>
<<if $glowrods gte 1>>但我还有<<$glowrods>>根荧光棒<<$plural>>，所以还没成睁眼瞎。
<<else>>一旦天仓星下山，我就抓瞎了。<<endif>>
我想我要么向瓦里亚号前进，同时祈祷能在天黑前到……
……或者在这里扎营，不管这听起来有多恐怖，要在夜里取暖有多难。
<<choice [[继续向瓦里亚号前进。|keepgoingtovaria]]>> | <<choice [[扎营。|makecampatcaravel]]>>


:: turnbackwhileyoucan
对，也许这样最保险。从天仓星在天空中的位置判断，今天没啥时间了。
只要我能看到我留在沙里的靴子印，就应该没问题。
要是脚印消失的话，我就得用重要地标来导航了，比如小飞船和巨型陨石坑什么的。
如果运气不错的话，我能在日落之前回到瓦里亚号。
等我回去后，应该能找些零配件来重新做一个指南针。我想我还记得怎么造指南针。
好，回聊。没时间侃。甚至没时间多说个“了”字。
你应该听得出来，我真没时间。
好，上路。
[[delay 90m|backatcaravel]]


:: stillhikingtowardpeak
有怪事要跟你报告。有件事，我观察了好久，终于证实了，所以才告诉你：
我的宇航服上的指南针完全报废了。
刚才我走到了个小陨石坑，决定沿着边缘走。
但是指南针并不随着我的方向做出相应的改变。
要么是乱转，要么根本不转，不管我朝哪边走都没用。
<<if $compassweird is 1>>之前我在小飞船的残骸里时，我的指南针就很怪了……
当时我觉得那只是局部事件，大概是飞船掉下来时引发了极性混乱。
我还以为只要我离开那里，就会恢复正常。
<<elseif $compassweird is 0>>事实上，我也不确定是不是自从我到了那艘小飞船以后，指南针就不正常了。
我当时也没有刻意去留心指南针，但是现在回想起来，觉得就是有问题。
小飞船坠毁时会不会引发了极性混乱呢？
更重要的是，为什么我离开了小飞船，指南针还是没能恢复正常呢?<<endif>>
显然，只要认准了山峰走，我还是能走到的。
但是考虑到返回时还需要找路，就有点麻烦了。
还好这一阵儿风不是很大。
要是我想沿着来时的脚印回去，就得赶紧了。
<<choice [[继续向着山峰前进。|onwardtothepeak]]>> | <<choice [[趁还回得去，赶紧回头吧。|turnbackwhileyoucan]]>>


:: onwardtothepeak
你说得对。我已经走了那么远了，现在没有选择，只能继续前进了。
<<if $bringginnytwo is 1>>不过，我真心希望我没一路带着发电机。<<endif>>
好吧，继续向北进发。
不管指南针说啥。
[[delay 60m|noclosertopeak]]


:: noclosertopeak
你会觉得我疯了……现在我多半是疯了。
不过我真觉得，从我上次跟你报告到现在，那座山没有变近一点点。
我已经朝那里走了好久了，而且是拼了命地走——
但是凭目测，我一点进展都没有。
我敢肯定这只是个光的错觉。
我都累成狗了，但是还要接着走。除非……你认为，我该算了？
<<choice [[继续向着山峰前进。|keepgoingpeak]]>> | <<choice [[掉头回瓦里亚号。|turnbackwhileyoucan]]>>


:: makecampatcaravel
我绝不可能睡在外面的星空下……听起来好像很浪漫，但气温下降得太快了。
就跟在瓦里亚号上一样，这艘小飞船明显没有动力把门关上……
不过与瓦里亚号不同的是，我有办法解决。
小飞船只有舱门开着，大小勉强够我挤进去。
<<if $hurtshoulder is 1>>我跟你说吧，带着一根脱臼的肩膀爬过那道门可一点也不好玩。
<<elseif $hurtshoulder is 0>>为了挤进来我扭了下肩膀，那滋味……不是盖的。<<endif>>
进来以后，就可以看到走廊里掉的满地杂物，可以用来堵门。
弄几层太空毯，再加些客舱里的床上用品，应该就能暖暖和和的了。
<<if $glowrods is 1>><<set $plural = "">><<else>><<set $plural = "">><<endif>>
<<if $glowrods gte 1>>我还留有<<$glowrods>>根荧光棒<<$plural>>，我想折亮一根，继续去搜索这里。
<<silently>><<set $glowrods = $glowrods - 1>><<endsilently>>
<<else>>不过，由于我宇航服上的灯报废了，我在黑暗里基本上要靠摸了。
我尽量乐观一点，祈祷我不会……
我勒个去！说了你也不信！
我刚刚撞到了个工业用荧光棒的箱子，你知道那种东西吗？就是里面灌满了荧光液，折断能挥着发亮的那种？
箱子里只剩下几根了，所以我要省着用到最需要的地方……
不过话说回来，你缺什么就绊倒在什么上的情况，真不多见啊？
（我的意思是，我倒是经常被东西绊倒，撞上想要的东西的机会真是不多。）
<<silently>><<set $glowrods = 2>><<endsilently>><<endif>>
好了，我打算四下转转。
<<if $compassweird is 1>>看起来我可以选条走廊去东区或者去西区，或者我可以去驾驶舱……
……虽然在这边都看得驾驶舱的控制面板已经完全碎了。
<<choice [[选择一条走廊。|tryahallwayagain]]>> | <<choice [[检查驾驶舱。|backtomainhallfirst]]>>
<<else>>看起来我可以选条走廊去东区或者西区，或者我可以去驾驶舱……
……虽然在这边都看得驾驶舱的控制面板已经完全碎了。
<<choice [[选择一条走廊。|tryahallwayfirsttime]]>> | <<choice [[检查驾驶舱。|backtomainhallfirst]]>><<endif>>


:: nooasisnopeak
我还是没有觉得山峰近了哪怕那么一点点。
天仓星要下山了。那座山峰看起来——我知道这听起来很荒谬——但它真的看起来居然有点……发光。
闪闪发绿。就像在散发着低亮度的光一样。
可惜，我貌似是怎么都走不到那座破山去，瞧瞧那破光的究竟了。
我觉得……我觉得我应该往回走了。我觉得在天黑之前我是到不了那里的。
魂淡啊，我都不知道我能不能在天黑前赶回瓦里亚号。不过幸运的话，应该可以赶回小飞船那里。
我对“幸运”的定义还真是浅薄啊。
<<choice [[转身回去。|turnbackwhileyoucan]]>> | <<choice [[继续前进。|pushaheadeveningtwo]]>>


:: keepgoingpeak
对，没错，不能止步。
<<if $rations is 1 or $ratpellets is 1>>我只是歇口气，我太渴了，脑子都不转了。
我补充些水分，然后继续前进好了。
<<elseif $rations is 0 and $ratpellets is 0>>不过说实话，我可能很快就真的要止步了……
因为我实在太渴了，脑子都不转了。
我头痛欲裂，要死啦。
不过，算了，除非翻过下一座山正好有座绿洲，否则我觉得我就是倒霉透顶了。<<endif>>
等我更靠近山峰一点之后再和你联系吧。
显然嘛，总归能走得到的。你不可能一直朝着什么走还永远走不到的。显！然！
（真的吗？）
[[delay 60m|nooasisnopeak]]


:: connectionlostdehydrated
[宇航服传感器显示低血容量性休克]
[血压水平低于最低阈值]
[氧气水平低于最低阈值]
[[delay 8s|connectionlost]]


:: tryahallwayfirsttime
有道理，东厅还是西厅？
我太累，连点兵点将都唱不全了，还是你决定吧。
<<choice [[东厅。|eastcaravelfirsttime]]>> | <<choice [[西厅。|westcaravelfirsttime]]>>


:: tryahallwayagain
有道理，走廊里转转总是好玩得很。
<<if $pills gte 1>>我之前已经把医务室里的宝藏都卷走了，所以我要去对面的厅看看。
[[oppositehall]]
<<else>>我上次临阵退缩，没有走太远，这次不管是上刀山还是下火海，我都要把这条走廊走完。
（实际上，要是这里真有刀山火海我都不会吃惊。哥也是过来人了。）
[[insidemedroomfirsttime]]<<endif>>


:: pushaheadeveningtwo
这真是馊主意。
我现在就只是机械地将一只脚迈到另一只脚前面。哪里也走不到。
只是在走。朝北走，我觉得我在向北。
这颗卫星已经剥夺了我的一切，甚至基本的方向感。
一步，再一步。
一步接一步。
[[delay 15m|deathmarch]]


:: deathmarch
嘿，天仓星很快就要落山了。这是死亡的日落。
在这颗卫星上的死亡日落时分，我孤苦伶仃。
<<if $rations is 0 and $ratpellets is 0>>真是好美啊，如果我没
啊哦
嘿
我
有点……抽……
搐……
嗯嗯嗯
[[delay 4s|connectionlostdehydrated]]
<<else>>我现在离山峰远着呢，瓦里亚号也不近。连小飞船都到不了。
我现在完全暴露在星空下。星空确实很美……但气温真的很低。
今晚没有反应堆可以靠着取暖了。
<<if $bringginnytwo is 1>>我身边只有一个发电机，但是没有能连发电机的设备。好无助啊。<<endif>>
气温在降低了，还会降得更低，然后……
我多半要死在这里了，死在这片美得令人窒息的星空下。
该死。我渴得连哭都哭不出来了。
这颗作孽的卫星连我哭的权利都剥夺了。
[[delay 2m|liedownanddie]]<<endif>>


:: liedownanddie
嗯。听着。我只是……只是太累了。我现在就想躺下。
也许我只是，你知道的，只是喜欢夸张。也许，我在这里不会有事儿。
只要睡一觉，我就会像洗了八个钟头的冷水澡那样清醒。
只是，嗯……万一我没醒过来的话……
……我想对你说声谢谢。感谢你用声音在黑暗中一直陪着我。
这是我所经历过的最深沉的黑暗，所以……我真的很感激你。
这里真的好美，看天上这些星星。成千上万，成千上万的星星。
你要是能看到该有多好。
我……稍微合会儿眼。
[[delay 8s|connectionlost]]


:: eastcaravelfirsttime
向右走，我们就进入可爱而宽敞的（真心不觉得）东边走廊。
在我的两边是密封门。
没有电力驱动背后的气动阀的话，门是打不开的。
走廊尽头有扇门开着，我朝那儿去了。
噢哟，要挤进到后面房间去实在不太容易。
我最好指望里面不要有自助餐。如果我再多长一两肉，我就再也不能挤回去了。
[[delay 1m|insidemedroomfirsttime]]


:: westcaravelfirsttime
向左走，我们就进入可爱而宽敞的（真心不觉得）西边走廊。
在我的两边是密封门。
没有电力驱动背后的气动阀的话，门是打不开的。
走廊尽头有扇门开着，我朝那儿去了。
噢哟。要挤进到后面房间去实在不太容易。
我最好指望里面不要有自助餐。如果我再多长一两肉，我就再也不能挤回去了。
[[delay 1m|insidemedroomfirsttime]]


:: oppositehall
<<silently>><<set $trycaravelgalley = 1>><<endsilently>>
对面走廊的侧门完全密封了，但是尽头正对着的门是半开的。
嗯……门开得不够大……我挤不过去。
但我能看到房间里，是厨房。
比瓦里亚号上的厨房要小得多，而且像是被人倒过来使劲晃过一样。
一片狼藉。即食口粮看上去真是倒胃口啊。
<<if $rations is 1>>我从瓦里亚号上已经拿到了足够的食物，能让我支撑一段时间……
……但是知道这里有食物也挺好，如果我饿疯了，我可以想办法通过这扇卡住的门找到吃的。
我保证会的。
<<elseif $rations is 0 and $ratpellets is 1>>今晚实在是累得不行，没法撞开这道门进去……
……但不用吃鼠粮这个念头实在是太吸引人了。明天又要练撞门了。
<<elseif $rations is 0 and $ratpellets is 0>>我得进去那间厨房。我都没吃没喝好久了，这要死人的啊。
但是这扇门纹丝不动。空着肚子，我哪有力气开门啊。
《阴阳魔界》的讽刺意味也不过如此吧。
罗德∙赛林，要是你想往电视里再加点什么精辟的独白，就快点，别磨磨蹭蹭的。
然后我们俩可以一起搞定这扇门。
<<endif>>
<<choice [[继续砸厨房门。|tryandfailgalley]]>> | <<choice [[回到主厅。|backtomainhallfirst]]>>


:: insidemedroomfirsttime
<<silently>><<set $pills = 3>><<endsilently>>
哎呀……他们该送个练过缩骨功的来，而不是我这种理工宅男。
好了，我进来了。这里像是个非常简陋的医务室。我这么说是因为我在这里找到了医疗包。
医疗包里有个瓶子——很大——标着中英文，所以我敢肯定这里头是止痛药。
我本来想说：哇塞！太棒了！我的肩膀痛得像活不到明天了。
（字面意义的：活不到明天。我也是醉了。）
可惜，瓶子快空见底了。要是装满了药片，这玩意儿倒是动听的乐器——哎，只剩下三片药了。
我不想在别人背后说坏说，不过，这艘飞船上肯定有人麻烦不小。
光顾着嗑药，也不管管飞船，看吧，掉下来了吧。
（不对，这想法也太黑暗了。不能这样想了。）
<<if $hurtankle is 1>>肩膀痛、脚脖子痛，我决定现在就用一片止痛药让自己好过一点。
<<elseif $hurtankle is 0>>肩膀疼死我了。我决定现在就用一片止痛药让自己好过一点。<<endif>>
但话说回来，一切没有最糟，只有更糟。我还是先留着这些药吧。
<<choice [[现在吃一片药。|takepillonefirsttime]]>> | <<choice [[暂时先保留。|takenopillfirsttime]]>>



:: takepillonefirsttime
<<silently>><<set $pills = $pills - 1>><<endsilently>>
对。没错儿！别自讨苦吃了。
我手上还有<<$pills>>片药，以后可以用。
糟了，荧光棒快要熄了。得趁荧光棒还没有熄完，赶快闪人。
[[delay 1m|caravelmain]]


:: takenopillfirsttime
对，也许那样才明智。我会暂时留住这些药的。
如果有谁问我这两天学会了什么道理，那就是一切没有最糟，只有更糟。
糟了，荧光棒已经不太亮了。得趁荧光棒还照得亮，赶快闪人。
[[delay 1m|caravelmain]]


:: caravelmain
<<silently>><<set $trycaravelgalley = 1>><<endsilently>>
好了，现在我回到主走廊了。对面走廊的侧门完全密封了，但是尽头正对着的门是半开的。
嗯，门开得不够大，我挤不过去。
但我能看到里面，是厨房。
比瓦里亚号上的厨房要小得多，而且像是被人倒过来使劲晃过一样。
一片狼藉。即食口粮看上去真是倒胃口啊。
<<if $rations is 1>>我从瓦里亚号上已经拿到了足够的食物，能让我支撑一段时间……
 ……但是知道这里有食物也挺好，如果我饿疯了，我可以想办法通过这扇卡住的门找到吃的。
我保证会的。
<<elseif $rations is 0 and $ratpellets is 1>>今晚实在是累得不行，没法撞开这道门进去……
……但不用吃鼠粮这个念头实在是太吸引人了。明天又是撞门的一天。
<<elseif $rations is 0 and $ratpellets is 0>>我得进入那间厨房。我都没吃没喝好久了，这要死人的啊。
但是这扇门纹丝不动。空着肚子，我哪有力气开门啊。
《阴阳魔界》的讽刺意味也不过如此吧。
罗德∙赛林，要是你想往电视里再加点什么精辟的独白，就快点，别磨磨蹭蹭的。
然后我们俩可以一起搞定这扇门。
<<endif>>
<<choice [[继续尝试打开厨房。|tryandfailgalley]]>> | <<choice [[回到主厅。|backtomainhallfirst]]>>



:: tryandfailgalley
我真希望能说，要是你对我信心满满助威呐喊的话，我就跟太空叮当猫一样会让梦想成真。
可惜不是真的。我拿了块碎片来撬门，可是门没有动一丝一毫。
毫米。不管了。
我先睡一会，明天再说了。今天够了。回主厅。
(另外，要是我能活着回地球，太空叮当猫的版权就属于我了。这个点子在那边能赚几百万美元呢。)
[[delay 1m|backtomainhallfirst]]


:: backtomainhallfirst
太好了，这里到处是垃圾，随便找点零配件可能都可以组装一个活泼的机器人小伙伴了。
——如果我知道怎么装配机器人的话。哦，还得知道怎么编程让它“活泼”点。
<<if $ginnycaravel is 0>>我现在找到啥都不会吃惊……[[trippedonginny]]
<<elseif $ginnycaravel is 1>>说不定哦，我的意思是，之前我不是还走运地被发电机绊倒吗？
我相信，只要我一直刨，我一定会找到一辆时光旅行车。[[afterginny]]<<endif>>


:: trippedonginny
<<silently>><<set $ginnycaravel = 1>><<endsilently>>
对不起。我刚刚被什么东西绊倒了，屁股都摔烂了。
唔，不知道你从我的字里行间能读到什么，在太空中，你摔倒了是绝对听得到你自己的尖叫的。
确实有点囧。
<<if $ginny is 1>>但好消息是，绊倒我的东西？是另一台小型发电机！
这和我在瓦里亚号上给<<$power>>供电的那台型号不同，要小些轻巧些。
不过不管怎么样，现在我的电量供应加倍啦！
<<elseif $ginny is 0>>但好消息是，绊倒我的东西？是一台小型发电机！
这意味着我终于有电啦！<<endif>>
看到了吧？在恐怖的废弃飞船上，傻人有傻福。
[[afterginny]]



:: afterginny
除了那些走廊和打不开的门……
……小飞船上只有驾驶舱（的废墟）能瞧瞧了。
我说过了，仪表操纵板毁了。
不光是因为“引擎失踪”这种问题导致这艘小飞船飞不起来了。
而且，这里看来就没一件完好的东西：导航仪、通讯系统、远程操纵器……整九码的操纵板全毁了。
（八米多呢。）
好吧，我可能有点言之过早。
这儿有个……像是个近距警报器，面板似乎完好无损。鬼知道剩下的哪个部分还能用。
<<if $zombierats is 0>>我真不觉得需要一个警报器，这附近犯罪率又不高。
<<elseif $zombierats is 1>>我真不觉得需要一个警报器，这里就我一个人，我深信不疑。
好吧，是有奇怪的嗖嗖嗖的声音，不过我觉得那是“我在瞎想”。<<endif>>
要用上警报器，还需要想办法连上发电机呢。 
而且前提是，这玩意儿还能用的话，我是说“如果”。
你觉得呢？
<<choice [[给警报系统通电。|hookupproximityalarm]]>> | <<choice [[直接去睡觉。|dontbotherwithalarm]]>>


:: dontbotherwithalarm
我同意，我认为这是在浪费时间、精力和光线，尤其在各项资源都如此短缺的情况下。
我要睡了。
[[flightdecksleepytime]]


:: hookupproximityalarm
<<silently>><<set $glowrods = $glowrods - 1>><<endsilently>>
喔，我真心觉得这完全是在浪费时间精力和光线。尤其在各项资源都如此短缺的情况下。
好吧，折另一根荧光棒。只剩下<<$glowrods>>根了。
不知道这算不算走运，这上头的电线大多暴露在外，所以连上发电机应该不是件难事儿。
给我几分钟摆弄一下。
[[delay 5m|hookedupalarm]]



:: hookedupalarm
<<silently>><<set $proximityalarm = 1>><<endsilently>>
这简直就是奇迹，我把警报系统连上线进行监控了。
不知道这玩意儿能监测多远，不过好像覆盖了大部分地方。
（监控区域显示小飞船后部“无数据”，但这可能只是因为小飞船根本没有后部了。）
好了，行了，等下我打呼噜时没人能偷我的包了。
[[flightdecksleepytime]]


:: snipehunt
<<silently>><<set $glowrods = $glowrods - 1>><<endsilently>>
又折了一根荧光棒，现在我只剩下<<$glowrods>>根了，希望没有浪费。
你怎么知道这乱窜的怪物不是等到猎物昏昏沉沉时才下手，
把猎物引进堆满垃圾的走廊，等反应迟钝的猎物自己摔断脖子呢?
我打99%的包票，接下来几分钟，我就那个摔断脖子的猎物了。
好吧，我要进去厅里了。要是有什么东西跳到我身上的话，我会飚海豚音的。
[[delay 15m|nosnipeshere]]


:: flightdecksleepytime
从这艘小飞船设计来看，只准备载几个人。这里只有一个飞行任务专家的座位，已经脱落了一半。
这个座位大概是这里最适合于睡觉的地方了。
<<if $glowrods is 1>><<set $plural = "">><<else>><<set $plural = "">><<endif>>
我觉得差不是算是晚上了。只剩下<<$glowrods>>根荧光棒了<<$plural>>，别浪费了。
我把舱门堵上了，我还裹了一堆毯子，希望能御寒了。
<<if $rations is 0 and $ratpellets is 0>>要是我这双抖抖缩缩的手里有点吃的喝的就好了……
也许夜里好好睡一觉起来会感觉好点。
说不定到时候头也不痛了。
好了，我要休息了。晚安吧，不管你在哪儿。不管我在哪儿。 [[delay 180m|starvenighttwo]]
<<elseif $rations is 1 or $ratpellets is 1>>我要休息了。晚安吧，不管你在哪儿。不管我在哪儿。 [[delay 180m|scuttlingnighttwo]]<<endif>>


:: starvenighttwo
头
痛死了
嘿
我
有点……抽……
搐……
嗯嗯嗯
[[delay 4s|connectionlostdehydrated]]


:: scuttlingnighttwo
嗯哼？什么声音？
你说话了吗？
<<if $zombierats is 0>>我嘞个去！那是什么？
天啊，好了，天啊，我醒了，我醒了。
嗯，有什么……我说不上来，好像是什么嗖嗖窜的声音。从我后面，跑到走廊外面去了。
然后，我转了个身……我，我……我看到了一些东西。会动。
在发光，是绿光。
有很多，那种小东西。贴着地面。在发光、在移动，跑得飞快。
实话实说，我今晚见到了最不想见到的东西。
<<elseif $zombierats is 1>>我发誓我刚才……我真的又听到了那奇怪的嗖嗖声，和之前的一样。
我从座位上转过身来，我又看到了：在主走廊里小小的发着绿光的会动的东西。<<endif>>
其实……我其实什么都没听到，什么都没看到，对吧？
我只是太累了，出幻觉了。“太空癫痫症”，我们小时候看的动画片《莱恩和史丁比》里就有，你记得吧？
我该……回去接着睡，对吧？
<<choice [[对，回去睡觉。|backtosleep]]>> | <<choice [[去看看哪来的声音。|gochasenoise]]>>


:: gochasenoise
哎，这是我最不想做的事了。
在这冰冷漆黑的夜里转悠，就为了去找根本就不存在的东西。
荧光棒又不是树上长的，要多少多少，就算是树上长的，这个卫星上也没有树啊。
说真的，我觉得回去接着睡才是正道。你难道不觉得睡觉比到处乱窜好吗？
<<choice [[好吧，回去睡觉。|backtosleep]]>> | <<choice [[不，追查声音来源。|snipehunt]]>>


:: nosnipeshere
这艘小飞船里我能去的地方都去过了，什么都没有。
没有噪音，没有东西动，没有什么在发绿光。
我肯定是在做梦，我现在想做的就是回去睡觉继续做梦。
然后再把自己吓醒，不过下次我就不会再去找床底下的怪物了。
[[backtosleep]]


:: alarmnighttwo
糟糕！警报响了！
感觉就像在外太空里心脏骤停一样！
监控屏幕上……什么都没有啊。
我是说，真的，屏幕上什么都没有。那到底是什么？我可是鬼知道多少光年内唯一的生物啊。
该死的玩意儿，我要把这个弱爆了的设备的电源拔掉。真后悔当初把这玩意儿连上发电机。
今晚可不是来拍我“史上最佳睡眠”的时候。
回床上睡觉。就算是有床吧。
[[delay 150m|dawnofdaythree]]


:: backtosleep
想象这张椅子很舒服（压根儿不舒服），然后我要回去接着睡了（不太睡得着了）……
当我早上醒来的时候，就会有一艘救援飞船准备好了自助早餐等着我（好像有可能）。
再说声晚安。
<<if $proximityalarm is 1>>[[delay 90m|alarmnighttwo]]
<<else>>[[delay 240m|dawnofdaythree]]<<endif>>


:: youareloopy
是的，有道理。
只是……你熟悉“幻想性错视”这个术语吗？我最近学到这个词。
人类大脑倾向于识别人形的东西。
比如说人们会看到水渍的形状像是圣母，车头的形状像是笑脸。
现在我在这里孤身一人，绝望地寻找熟悉的身影，比谁都更容易出现这种人形识别的错觉。
所以，我不管倒带多少次都觉得是看到了一个人，不过我很清楚，那绝对不可能是人。
深呼吸，别害怕。
<<choice [[去吃些早餐。|breakfastdaytwo]]>> | <<choice [[继续观看监控记录。|keepstudyingfootage]]>>


:: keepstudyingfootage
不管看多少次都一样，每次都看到。
每次，我都得告诉自己，我真的没看见。
我了解自己。我可以这么看上一天，不过我真心不觉得这样有利于身心健康。
我要吃些早餐，不去管我到底看到的是什么了。反正现在没看到。
[[breakfastdaytwo]]


:: dawnofdaythree
早上好，太阳晒屁股了。所以我起来了，而且我神采奕奕。
就跟杰克·尼克尔森在电影《闪灵》结尾那时候一样。超级冷酷，相当疯狂。
还有——又来了，我不是想恶心你——我嘴巴里有超极恶心的味道，而且我的唾液是绿的。
昨晚我睡在室内，又不是室外，而且身边也没有辐射源……
大气中肯定有什么东西，这是呼吸的副产品，真想知道究竟是什么。
<<if $proximityalarm is 1>>我觉得我该去看看警报器昨晚到底出了什么问题。
尽管我很肯定那只是点故障，而且我更想先吃早餐。
<<choice [[检查警报器。|checkoutalarm]]>> | <<choice [[吃些早餐。|breakfastdaytwo]]>>
<<else>>总之，考虑到现在是早上，我又很饿，我觉得应该先吃些早餐。[[breakfastdaytwo]]<<endif>>


:: checkoutalarm
警报器接上电开始运行了。
现在我可以查阅警报器的历史记录，看看昨晚是什么东西触发了警报。
你猜会是什么？狂风卷起的尘暴或者石头？
前晚不就刮过大风嘛，还把我的逃生舱都吹开了十英尺。
我跟你赌这儿也只是刮了阵风而已。我是说——
[[delay 1m|whattrippedalarm]]


:: whattrippedalarm
不可思议。
我已经看过好几遍录像了，翻来覆去地看。
要是你仔细看，仔仔细细地看，会看到看第二个象限（总共有三个象限），在非常靠近边缘的地方……
有什么东西在动，只是稍微碰到了防御带一下。
看起来像是……
……不会吧！瞎猜！不可能的，一定是我寂寞得犯糊涂了。
<<choice [[“看起来像”什么？|lookslikewhat]]>> | <<choice [[没错，你多半是。|youareloopy]]>>


:: lookslikewhat
好，听好了，我知道这听起来非常疯狂。记住连我自己都不敢相信。
但是这个东西看起来很像是……一个人。
我是说，只是阴影。要从监控屏幕上看出隔了老远的东西究竟是什么几乎不可能。
人类大脑倾向于识别人形的东西。我最近学了个词：“幻想性错视”。
比如说人们会看到水渍的形状像是圣母，车头的形状像是笑脸。
现在我在这里孤身一人，绝望地寻找熟悉的身影，比谁都更容易出现这种人形识别的错觉。
所以，我不管倒带多少次都觉得是看到了一个人，不过我很清楚，那绝对不可能是人。
深呼吸，别害怕。
<<choice [[去吃些早餐。|breakfastdaytwo]]>> | <<choice [[继续观看监控记录。|keepstudyingfootage]]>>


:: mmmmtasty
讽刺的话变成文字，意思就掉了一半，不过我打算吃去了，同时把你的话当做讽刺。
原因很简单，因为我不觉得讨论“太空里早饭吃鱼”这种话题还能不带点讽刺意味的。
[[delay 5m|afterbreakfastthree]]


:: breakfastdaytwo
早餐，一般人都吃。
虽然流落外太空，孤苦伶仃，我还是要尽量过点正常人的生活。
<<if $rations is 1>>所以，我要像正常人一样享用一份即食快餐：金枪鱼，还是柠檬胡椒味的呢。
一流早餐。
<<choice [[嗯！好吃极了！|mmmmtasty]]>> | <<choice [[听起来好恶心。|soundsgross]]>>
<<elseif $rations is 0>>我要像正常人一样，享用臭水臭鼠粮。
一流早餐。
<<if $trycaravelgalley is 1>>噢，天啊！今天我真得去看看能不能进到小飞船的厨房里了。
<<elseif $trycaravelgalley is 0>>不知道这艘船会不会有厨房。我觉得，应该会有吧~~~~
当然前提是厨房没有脱落。<<endif>>
我想享用完这份丰盛的早餐后，这就该是我今天的任务了，是吧？
<<choice [[对，去试一试。|surecaravelgalley]]>> | <<choice [[还是吃鼠粮吧。|nocaravelgalley]]>><<endif>>


:: surecaravelgalley
没错！要的就是这股劲！吞下这些实验室的饲料之后，就闯荡一番厨房吧！
<<if $trycaravelgalley is 1>>我知道从门缝里是挤不过去的了……
但我还知道里面的柜子里掉了不少东西下来。我有一个主意。
<<elseif $trycaravelgalley is 0>>返回主厅，走到其中一条侧廊……
……然后，嘿，又到了好消息/坏消息时间了。小飞船里确实有厨房！而且门还开了一条缝！
不过……门开得不够大，我挤不过去。
比瓦里亚号上的厨房要小得多，而且像是被人倒过来使劲晃过一样。
一片狼藉。即食口粮看上去真是倒胃口啊。
这扇门纹丝不动。不过嘿，还有希望。我有个主意。<<endif>>
要是能在这艘船的垃圾里找到足够的零件的话，应该能组装出一根操纵杆，
再装一个我能控制的简易抓手也不难。
给我几分钟，我要在这附近好好找找，没准能找到可以用的东西。
[[delay 15m|reachintogalley]]


:: reachintogalley
<<silently>><<set $rations = 1>><<endsilently>>
好了！我的操！纵！杆！做好了。我之所以打这么多感叹号是因为这绝对是个价值百万美元的点子。
（在没地方花百万美元的外太空想出这么个价值百万美元的点子，太讽刺了。）
反正肩膀也伤了，索性破罐子破摔，使劲将操纵杆尽可能地伸到门里面去呢？
好了，操！纵！杆！……我们行动吧！
稳住！稳住！
该死！还是不够稳。
好了，慢慢地，稳稳地……我用操纵杆的抓手抓到了个闪着银光的东西，应该可以吃吧……
快了……别掉……你个笨手笨脚的矬人，千万别掉！
哈！太棒了！谢天谢地，我成功了！我给我自己抓到了一份鸡肉香蒜意大利面！
我有点想马上撕开袋子吃……可是，我是不是应该再多抓点，拿够了为止呢？
<<choice [[马上吃掉！这是你应得的！|eatyouearnedit]]>> | <<choice [[继续抓食物。|keepfishing]]>>


:: keepfishing
你说得对，如此万灵的操纵杆，此时不用，更待何时！
（我以后一定要将这句话用作操！纵！杆！的广告语。你这可是在见证历史啊，朋友！）
小心……好了……我又抓到了份红烧牛肉！
然后那边的那个……是……法式炖菜？
我得承认，我根本不懂什么是法式炖菜，只在动画片《料里鼠王》里见过。哦，该不会是一份急冻干燥的耗子吧？
一会儿就知道了。
<<choice [[对，就是那只耗子。|cgirat]]>> | <<choice [[是炖菜啦，傻瓜。|stewedveggies]]>>


:: stewedveggies
嗯，没错，我觉得就该是这种味道。
虽然味道跟耗子差不多，但是我现在也不知道什么是真的什么是假的了。
[[delay 5m|afterbreakfastthree]]


:: soundsgross
哦，是啊，然后你还会很高兴地听说，它尝起来也这样。
[[delay 5m|afterbreakfastthree]]


:: nocaravelgalley
好吧，也许没错儿。好处不多，但是出力少。省点力气好出远门。
不过出远门估计也捞不到啥好处。等我累得半死，答案自动会揭晓。
[[delay 5m|afterbreakfastthree]]


:: eatyouearnedit
哇哟，居然有这么难吃的鸡肉香蒜意大利面。
当然也算上是我这辈子吃过的最好吃的东西。
我记得科尔比提过，这些即食口粮的保质期是五年……
——如果保存在阴凉、干燥的地方的话。再找不到比这颗卫星更阴凉干燥的地方了。
哈！这个包装上写的出厂日期是四个月前。明显是印刷错误。
不过倒是提醒了我这个业余侦探一条线索：这艘小飞船最近一次补给是几时呢？
那么……这艘小飞船在这里积了多久的灰了？
<<choice [[查看日志？|checkthelogs]]>> | <<choice [[算了，别管了。|dontworryaboutit]]>>



:: morefishing
小心……好了……我又抓到了份红烧牛肉！
然后那边的那个……是……法式炖菜？
我得承认，我根本不懂什么是法式炖菜，只在动画片《料里鼠王》里见过。哦，该不会是一份急冻干燥的耗子吧？
一会儿就知道了。
<<choice [[对，就是那只耗子。|cgirat]]>> | <<choice [[是炖菜啦，傻瓜。|stewedveggies]]>>


:: cgirat
嗯，尝起来像是炖菜。
确实没有耗子的味道。厨艺不错嘛，即食口粮的厨子们！
[[delay 5m|afterbreakfastthree]]


:: checkthelogs
是啊，我也想过，不过我觉得不太靠谱。
电脑系统都毁了，要找到什么信息很难。
即使我能恢复黑匣子，也没办法读取里面的数据。
除非有人觉得好玩在纸上记了日志——可是原则不需要啊（ 我也没辙了。）
没什么事干了，还是回去继续捞厨房里的即食口粮吧。
[[morefishing]]


:: dontworryaboutit
对，有道理……万一我翻出什么不喜欢的内容了呢？
我想像不出会有什么特别让人欣慰的内容。
而且，电脑系统都毁了，要找到什么信息很难。
即使我能恢复黑匣子，也没办法读取里面的数据。
除非有人觉得好玩在纸上记了日志——可是原则不需要啊（ 我也没辙了。）
没什么事干了，还是回去继续捞厨房里的即食口粮吧。
[[morefishing]]


:: afterbreakfastthree
好了！早餐搞定（老话说得好，饥不择食）。
看来，今天一天的任务就是朝山峰行进。
我希望昨天走不到只是筋疲力尽和光影幻象共同作用的结果。
现在我吃饱喝足，还有一整天的时间，我觉得今天的成功率要高很多。
而且知道回瓦里亚号的路上有这艘小飞船，还有个小发电机在等我，感觉不错。
<<if $power is "pod">>这样我可以给失事信标供电，说不定就能得救了……
不然的话，给小炮塔供电也行，万一这里除了我还有别的什么东西在活动呢。
<<elseif $power is "turret">>这样我可以给失事信标供电，说不定能得救了。
<<elseif $power is "beacon">>这样我可以给小炮塔供电，万一这里除了我还有别的什么东西在活动呢。
<<elseif $power is 0 and $frontofship is 1>>这样我可以给失事信标供电，说不定就能得救了……
不然的话，给小炮塔供电也行，万一这里除了我还有别的什么东西在活动呢。<<endif>>
你怎么想？你觉得我今天到得了那座山峰吗？
<<choice [[能，快去吧！|peakdaythree]]>> | <<choice [[真心不觉得。|sincerelydoubt]]>>


:: sincerelydoubt
好像我就没有顾虑一样？
昨天为了去那座山峰我差点没把自己弄死。也许我应该知难而退。
我有那么一点点想直接回瓦里亚号。
至少我还认识瓦里亚号……自从我周围充满着了不确定性后，真心感觉到了它的亲切。
但是有什么……我说不上来，有什么让我身不由己地想去山峰瞧瞧。
<<choice [[好吧，那就去吧。|peakdaythree]]>> | <<choice [[可能很危险啊。|couldbedangerous]]>>


:: peakdaythree
对，我同意，我努力探索了这么久，怎么能空手而返呢？
我要带点干粮，把发电机藏好……
……然后就要上路了。我出发时会联络你的。
[[delay 15m|headingouttopeak]]


:: headingouttopeak
我把头伸出小飞船外面，然后，我看到了这辈子最印象深刻的日出。
（这里太阳是天仓星，不是地球的太阳，不过，我打算还是管它叫太阳。）
所以，首先，先赞叹一番我居然在第一缕阳光照耀之前起床了——这简直太不像我了！
然后，我要说我又看到山峰上那诡异的淡绿色光晕。
那些光跟山峰的形状一样，就是不太自然。吓煞我也。
（“吓煞我也”是这种场合下的标准科学用语。你去查查就知道了，这可是“吓死人”的古语。）
<<if $compassweird is 1>>噢，还有一点值得一提，我的宇航服的指南针还在吓死人的状态。
<<elseif $compassweird is 0>>噢，还有一点值得一提，我注意到我的宇航服的指南针还在吓死人的状态。
指南针没有随着我改变位置而改变指向。有时候乱转，有时候又指着一个方向完全不动。
多半是小飞船有什么诡异之处，影响了指南针的极性……我不知道。<<endif>>
我觉得我倒并不需要靠它走到山峰。只要我认准了走，走到山峰那儿应该不是个问题。
不过我完全可以在出发前，先花几分钟，用零配件组装一个指南针。
<<choice [[不带指南针出发。|nocompass]]>> | <<choice [[造一个指南针。|buildcompass]]>>


:: nocompass
对啊，造指南针多半是浪费时间。不过我肯定这艘船里的垃圾里就能刨出来足够的材料。
要是有时间的话，这里的垃圾都够再造个新的小飞船了。
（吐槽！说得好像我什么都有，就是没有时间一样。）
好吧，我向北方前进了。登狗屁峰还是变条疯狗。过一会儿我再和你联络。
[[delay 90m|stillpeakbound]]


:: stillpeakbound
好，我现在到了昨天沿着走的小陨石坑的边缘了。
当时我发现虽然离小飞船有五英里地，宇航服的指南针还是失灵。
（嗯，大概是……八公里？我真的很努力了。）
今天还是这样，宇航服的指南针依然不给力。
<<if $homemadecompass is 1>>我自己做的指南针倒是挺灵的。
他们应该让我来建瓦里亚号的，说不定就不会坠毁了。
……噢，说着说着觉得笑不起来了。不好意思。<<endif>>
跟昨天一样，我还是没觉得离山峰更近了。跟昨天一样，我还是在惆怅是不是该继续走。
<<choice [[对，继续走。|keeppeakdaythree]]>> | <<choice [[算了吧，回去。|turnbackdaythree]]>>



:: keeppeakdaythree
说得对，白天的时间还长着呢。这颗卫星很小的。
只要我走得够久，总会到的。
——或者死在路上。
[[delay 60m|notdeadyet]]


:: notdeadyet
好消息是，我没死在路上。
坏消息是，我还是看不出来我和那座山之间的距离缩短了。
就像那座怪山让时空和透视都去休假了一样。
有时候，我抬头看一跟，刚想：“嘿，我要到了！”然后眨了眨眼，那座山又变成了十英里开外。
我觉得在继续上路之前得吃点东西。
<<if $rations is 1>>
<<choice [[对，你辛苦这么久了。|snacktime]]>> | <<choice [[别浪费时间吃东西。|notsnacktime]]>>
<<else>>
<<choice [[对，你辛苦这么久了。|ratsnacks]]>> | <<choice [[别浪费时间吃东西。|notsnacktime]]>><<endif>>


:: notsnacktime
不懂为什么补充我路上消耗的卡路里也能叫做“浪费时间”……
不过，好吧，我等一会儿再吃，等我我确定快到那座蠢山再说。
要是你听到了咕咕的声音，那只是我的肚子在叫。
[[delay 40m|progressatlast]]


:: couldbedangerous
我只是个胡子都没长出来的学生兵，流落到这么个荒凉星球，还穿着一身失灵的宇航服……
……地球好遥远啊，夜空中细细密密的星星，我都认不出哪一颗是我的太阳……
（我是在那个冻！死！人！的夜里看到的星空，顺便说一句，我还是冒着辐射的风险熬过的那晚）——
而且我刚刚还在一艘有不明生物发着绿光四处乱窜的鬼船上过了一晚。
连我的手指头都长倒刺了。
总而言之，这趟旅行到底哪里不危险了！？
兄弟，我要么就是混得“登峰”造极，要么十八年后还是一条好汉。
<<choice [[我们还是祈祷“登峰”造极吧。|peakdaythree]]>> | <<choice [[我赌你不会死的。|dontgobust]]>>



:: dontgobust
你真是个乐天派，你知道吗？
我跟你说，要是我十八年后又成一条好汉……
不管我是辐射过量死的，还是被野蛮机器人用激光剑杀死的……
我回来一定会找你跟你说，“我说了吧”。
那感觉岂不是很好？
好了，我要带点干粮，把发电机藏好……
……然后就要上路了。我出发时会联络你的。
[[delay 15m|headingouttopeak]]


:: buildcompass
嘿，好吧。这简直太好玩儿了！
（大概应该给好玩加个双引号。严肃认真地说，这应该是宅男版的“好玩”）
医药包里有些针，正好可以用。
把磁化的针放进一碗水里就能当指南针，我以前就干过。不过，要在一整天都徒步的情况下，端着一碗水可不好保持平衡啊……
不过，要点确实是一样的，就是指针转动时不能有阻力。
所以，我打算做些改动，让指针平衡在图钉或者大头针上面转。做种类似的东西。
当然，要想做指南针，最重要的是先找到磁铁，最好是稀土磁铁——
运气真好，小飞船的驾驶舱里有一堆电脑的硬盘没用了！
稍微拆开来（好吧，我打算暴力地随手捡个东西狂砸一通）
这样一来，我就能拿个硬盘，磁化一根针还不是小菜一碟！
[[delay 5m|madeacompass]]


:: madeacompass
<<silently>><<set $homemadecompass = 1>><<endsilently>>
瞧！我磁化了这些针，安到了一小块轻巧的即食口粮的塑料包装上面……
再别到一枚拉开的回形针上，然后把整个装置放进一根塑料水管里。
看起来不打眼，不过倒是挺好用的。
具体地说，就是它能指着山峰，告诉我那是北边，比我原先配备的指南针要好太多了。
这就是阿宅的力量！
好吧，我跟我亲爱的指南针在朝北方进发了。登奇峰还是变奇疯？我过会儿告诉你。
[[delay 90m|stillpeakbound]]


:: turnbackdaythree
你以为啊？我知道我对我的进展的描述有点悲观……
……但是我悲催地觉得，我反方向走也不会离那座山更远。
你真觉得我现在就该放弃？
<<choice [[至少先休息下。|takearestyouloser]]>> | <<choice [[不，继续向北走。|keeppeakdaythree]]>>


:: snacktime
好了！我要闭着眼睛从包里拿一份即食口粮，拿到啥就吃啥！
猜一猜……恭喜你，墨西哥辣豆子！
（在太空中，你就是屁神也没关系。）
有人会说加了豆子的辣椒根本就不是辣椒。 
这个问题关系重大啊，你站在哪一边？
<<choice [[我喜欢有豆子的。|chiliwithbeans]]>> | <<choice [[豆子太恶心了。|chilinobeans]]>>


:: ratsnacks
好吧，我们又见面了，老鼠饲料。
实话实说，我已经烦死吃鼠粮了。
我一直在想，这一路上，我们一定有什么决定没做对……
……所以才没找到点好吃的东西。
我以前老是抱怨瓦里亚号的即食口粮太难吃了，可是也比这个好多了。可能没好上多少，但总归好点。
不管怎么样，我想我们永远也得不出答案了。
我要赶紧吞下去，然后接着上路。一会儿再联系你。
[[delay 40m|progressatlast]]


:: takeonepill
<<silently>><<set $pills -= 1>><<endsilently>>
对，我觉得这个主意不错。
特别是当我说了这是个“永不结束的故事”的时候，那片儿的主题曲就开始在我的脑海里盘旋不去了。
要是止痛片不是专治金曲头痛的，那我也没辙了。
要休息了。我会尽快和你联系的。
[[delay 30m|neverendingstory]]


:: takearestyouloser
好，有道理。
<<if $hurtshoulder is 1 or $hurtankle is 1>>我浑身上下伤痕累累，疼得要命。<<endif>>
<<if $pills gte 1>>我觉得我该吃片止痛药，等药效发挥比较好。
这药真能帮我撑到“永不结束的故事”的最后？
<<choice [[吃一片药。|takeonepill]]>> | <<choice [[先不吃。|nopillrightnow]]>>
<<else>>向山峰进军都成了一个永不结束的故事了。
现在我要坐下来歇半个钟头，开唱《永不結束的故事》的金曲。
哦，我真是作茧自缚啊。
[[delay 30m|neverendingstory]]<<endif>>


:: chiliwithbeans
好吧，你真该尝一下这种即食口粮，马上毁三观！
不管怎样，我还是很高兴谜底揭晓了。要是，我乌鸦嘴一下，要是我不幸在这个荒凉星球上挂掉……
清明时记得给我供上“有豆子”的墨西哥辣椒。
亲，我先吃去了，吃完再接着走。一会儿再和你联系。
[[delay 40m|progressatlast]]


:: chilinobeans
吃了一口后，我觉得还是你说得对。他们应该把豆子去掉。
不对，他们应该把辣椒去掉。
不管怎样，我还是很高兴谜底揭晓了。要是，我乌鸦嘴一下，要是我不幸在这个荒凉星球上挂掉……
清明时记得给我供上“没有豆子”的墨西哥辣椒。
亲，我先吃去了，吃完再接着走。一会儿再和你联系。
[[delay 40m|progressatlast]]


:: progressatlast
好吧，我总算有点实质性的进展了。
我在另一个陨石坑边上，非常壮观，有很庞大的石壁，我可以垂降下去。
山峰正好坐落在陨石坑的中心，在凹面的最深处。
只要我把它盯紧了，它就跑不掉。（起码理论上是这样。）
坑挺深的。而且我身上也没有什么登山工具（明显嘛）。
那我能安全下去吗？我还回得来吗？
你觉得呢？要知道答案吗？
<<choice [[没问题，下去吧。|gointocrater]]>> | <<choice [[不行，现在返回。|dontgointocrater]]>>


:: nopillrightnow
好吧，我再想想。但我得告诉你，你简直想象不到我处在怎样的水深火热中。
因为我一说“永不结束的故事”这几个字，那片儿的主题曲就开始在我的脑海里盘旋不去了。
你试试看呢，都成这样了还不吃止痛药。搞不定的。
好了，我要休息了。我一会儿跟你联系。
[[delay 30m|neverendingstory]]


:: backatcaraveldaythree
哇，累成狗了！我回到小飞船了！
要不要再进去呢，还是别再冒险了？太大胆了？这一天是不是冒太多险了？
<<choice [[进去看看。|gobackinsidecaravel]]>> | <<choice [[绕开，前往瓦里亚号。|bypasscaravel]]>>


:: headbackforreal
对，好吧，听你的。
我得说你简直是在让我白费劲去城墙上赶麻雀，而且比字面意义还无聊。
至少城墙上还有麻雀。
这里？我刚刚朝一个方向上走了这么久。现在我又要掉头回去了。
我把通讯关了。等我回到小飞船那里，再给你消息。
希望你不要特别想我。
[[delay 190m|backatcaraveldaythree]]


:: seriouslygiveup
你是认真的吗？哇哟。
要知道，我在小飞船里刨垃圾时没找到字典……
……要是真有，我也不奇怪。里面肯定还有你的照片。
紧挨着“虎头蛇尾”这个词。
好吧，那我还是掉头回去吧，要是你真确定的话。
<<choice [[我确定，回头吧。|headbackforreal]]>> | <<choice [[开玩笑的！进陨石坑。|justkidding]]>>


:: dontgointocrater
大哥……表开玩笑！我好不容易来了。你不是真的要我放弃吧？
<<choice [[开玩笑的！|justkidding]]>> | <<choice [[我是认真的。放弃吧。|seriouslygiveup]]>>


:: neverendingstory
好，休息够了，一辈子都不想再听到利玛的歌了。
白天的时间还长着呢。这个星球很小的。
只要我走得够久，总会到的。
——或者死在路上。
[[delay 60m|notdeadyet]]


:: bypasscaravel
笑话！那里头有个那么好的发电机，我肯定要去拿。
嘿，你吃饱了吗？你是吃饱了饭没事干想拿我开涮是吗？
<<choice [[拿发电机。|gobackinsidecaravel]]>> | <<choice [[不管发电机。|forgetthatginny]]>>


:: gobackinsidecaravel
对，无论如何我都要去。
把发电机留在这儿，明显没道理。
（既然现在看来你也做不出有道理的决定，所以这个问题上我就自己拿主意吧。)
我从船里找到需要的东西后会再联系你的。
[[delay 10m|gotitall]]


:: justkidding
哦，哟！
我还以为我们俩过去几天建立了良好的共识呢，我差点被你骗了！
对啊，我当然要去。我不就是为了去才来的吗。
[[delay 2m|gointocrater]]


:: gointocrater
我已经侦察过附近的坑壁了，找到了一处山石突起能搭脚爬的地方。
没有尼龙绳攀岩钢环的情况下，岩石上有几块缺口还是挺管用的。
再说了，还能倒霉到哪里去？顶多是拿把山寨瑞士军刀锯掉条胳膊，就像《127小时》里演的一样？
……噢哟，可能真！的！会倒霉成这样。
祝我好运吧！
<<choice [[祝你好运。|goodluckclimbing]]>> | <<choice [[别摔断了腿。|breakaleg]]>>


:: goodluckclimbing
谢了。我觉得我真的需要点好运。
[[descentintocrater]]


:: descentintocrater
我觉得，要是不和你说话可能会容易一点。别介意，我只是得集中注意力。
如果之后你联系不上我，那可能我真的像《127小时》里演的一样遇到麻烦了。
不知道127小时在这个宇宙里相当于多久，希望听着高大上，至少得是“49087兆分钟”之类的吧。
好了，我要下去了。我很快会联系你的。希望还能。
[[delay 30m|downincrater]]


:: variayetagain
要是我关一会儿通讯，你别介意啊。
我觉得这个空无寒冷的宇宙比你说话更理性。
我过会儿联系你。希望还能。
[[delay 190m|wenttowardpeakanyway]]


:: forgetthatginny
感觉这样好怂啊。
下一步是什么？把身上所有吃的都扔了？还是脱了宇航服在荒漠里裸奔？
迄今为止，你都帮了我大忙。你给我打气，帮我活下来。我真的很感激。
就因为这样，我现在还在跟你联络，不过我打算不听你的了。
我回瓦里亚号去了。是很怂，不过就这么办了。
<<choice [[那你趁早。|variayetagain]]>> | <<choice [[等等，停下来。我错了。|iwaswrong]]>>


:: iwaswrong
啊啊啊啊啊，你在拿我开涮啊？！
你觉得这很好玩是吧？又饿又疯地在荒凉的白石头上来回跑？
我告诉你，一点也不！好！玩！点儿都没有。
你坐在高大上的地方……管你是在哪，我觉得都高大上得很……
心血来潮地指挥我东跑西跑，想干啥？你觉得很好玩是吗？你觉得很呆萌是吗？
好了哥受够了。一点也不好玩，一点也不可爱……
……而且不止把我逼得半疯，而且整个人都气炸了。
我要回山峰了。我才不管你怎么想怎么说。
也许我到了那里会通知你。也许不会。
要看我心情了。
[[delay 190m|bigoldcrater]]


:: bigoldcrater
我回到这个巨型陨石坑了，就是中间有那座山的这个。
我要下去了，懒得理你怎么想的。
（归档需要，你怎么想的？）
<<choice [[好吧，那就试试吧。|shrugokay]]>> | <<choice [[不，别这么做。|reallydont]]>>


:: shrugokay
“那就试试吧。”我听出来了，你现在一副事不关己的表情。
按我说吧，等我下去途中摔断了脖子——现在看起来灰常灰常有可能，只要看看那坡度——
那会儿你就能得意地对我说，“我说了吧”。
你就又赚了一把，挺好的。
[[delay 2m|gointocrater]]


:: breakaleg
哈哈。
如果真的摔断条腿，我会开着通讯器，吼上几个钟头。你等着瞧吧。
[[descentintocrater]]


:: wenttowardpeakanyway
嘿。唔，我要跟你道歉。
记得你怎么劝我别去山峰？怎么劝我返回瓦里亚号？
然后我怎么说你不可理喻？
说你乱指挥，帮倒忙？
然后我心烦意乱，有很长一段时间都切断了通讯？
<<choice [[是的，我记得。|irememberthat]]>> | <<choice [[我很担心你。|imissedyou]]>>


:: downincrater
我下来了！这一路不容易，但谁不喜欢时不时来点惊险刺激？
<<if $hurtankle eq 1 and $hurtshoulder eq 1>>不过我的肩膀和脚踝疼死了。我也是蛮拼的，这就是代价。
<<elseif $hurtshoulder eq 1>>我的肩膀现在非常疼，我也是蛮拼的，这就是代价。<<endif>>
<<if $pills gte 1>>我大概只剩下吃药的一口气了，再不吃就撑不住了。
<<choice [[当然，来一片药。|takepainkiller]]>> | <<choice [[忍忍就好。|nopainkiller]]>>
<<else>>不吃止痛药的话，那我真心希望能有人给我一把瑞士军刀。
只要能把胳膊和脚砍下来，问题就能迎刃而解了，对吧？
<<choice [[我相当怀疑。|highlydoubtit]]>> | <<choice [[试试咬下来吧。|chewthemoff]]>><<endif>>



:: takepainkiller
<<set $pills = $pills - 1>>
<<if $pills eq 1>><<set $plural = "">><<else>><<set $plural = "">><<endif>>
好，现在只有<<$pills>>片药<<$plural>>了，但愿有效，也算没白吃。
[[longslowtrek]]



:: reallydont
好吧，我说过了，我懒得管你是怎么想的。
不过，等我在下去的路上摔断了脖子——现在看起来灰常灰常有可能，只要看看那坡度——
那会儿你就能得意对我说，“我说了吧”。
你就又赚了一把，挺好的。
[[delay 2m|gointocrater]]


:: variayetagainginny
<<silently>><<set $bringginnytwo = 1>><<endsilently>>
要是我关一会儿通讯，你别介意啊。
我觉得这个发电机比你说话更理性。
我过会儿联系你。希望还能。
[[delay 190m|wenttowardpeakanyway]]


:: gotitall
感觉这样好怂啊。
到现在为止，你都帮了我大忙。你给我打气，帮我活下来。我真的很感激。
就因为这样，我现在还在跟你联络，不过我打算不听你的了。
我要回瓦里亚号去了。是很怂。不过就这么办了。
<<choice [[那你趁早。|variayetagainginny]]>> | <<choice [[等等，停下来。我错了。|iwaswrong]]>>


:: iforgiveyou
谢了。我现在只有你，我可不想和你争。
但我必须得这么做。
总之，你听我给你讲讲这段时间发生的事情。
[[bigoldcrater]]


:: chewthemoff
<<if $rations is 1>>要知道，虽然我觉得我的胳膊腿会比我一直吃的干粮好吃点……
<<elseif $rations is 0 and $ratpellets is 1>>要知道，虽然我觉得我的胳膊腿会比我一直吃的鼠粮好吃点……<<endif>>
……但我想现在就吃自己还为时尚早。
等几个钟头再问我吧。
[[longslowtrek]]


:: highlydoubtit
好吧，我也表示怀疑。但是我尽量有些前瞻性。
[[longslowtrek]]


:: nopainkiller
好吧，我只好默默忍受了。
（吐槽！我坠毁在这里后还没有默默做出什么事来呢。）
[[longslowtrek]]


:: whatkey
只是种感觉，我也说不上来。
但是山上环绕的光晕，摇曳的光亮……既让我毛骨悚然，又让我神魂颠倒。
我一定得去那边，去看看究竟是什么。
可能什么都不是，然后你可以说“我说了吧”，早该回瓦里亚号之类的话。
但是……我觉得那边有东西。所以我希望这次你要原谅我，我要跟着我自己的感觉走。
<<choice [[我原谅你。|iforgiveyou]]>> | <<choice [[还是有点生气。|stillkindapissed]]>>


:: stillexplaining
为了好好想想你为什么会让我做这么不可理喻的事情。
我终于平静下来，打开通讯器跟你说话。
只是……我还是有点不想听从你的建议，打算去山峰。
其实我的“有点”就是“完全”的意思。
真的很抱歉。
但是这座山……就好像是天生吸引着我一样。我无法对它视而不见，我必须得去看看。
我想它可能就是关键吧。
<<choice [[什么意思？|whatkey]]>> | <<choice [[回家的关键？|whatkey2]]>>

:: whatkey2
[[whatkey]]

:: irememberthat
当时我需要点时间，好好想想。
[[stillexplaining]]



:: imissedyou
噢，谢谢你。其实我也挺记挂你的。
所以就更不好意思告诉你，刚才挂断通讯给了我点时间，好好想想。
[[stillexplaining]]


:: stillkindapissed
好吧……我现在只有你，我可不想和你争.
但我必须得这么做。
你愿意的话就一直这么吐槽下去吧，不过希望结局会证明我是对的。
总之，你听我给你讲讲这段时间发生的事情。
[[bigoldcrater]]


:: longslowtrek
我前面还有好远好难的一段路要走……
我是真没有夸大这个陨石坑的规模，尤其我现在还就在它的里面……
不过我现在能看到那座山了，它又不会跑，只要我一直朝着那个方向走就一定能到那儿。
<<if $homemadecompass is 1>>我自制的指南针显示我还在向北前进，所以我觉得我完全可以凭着这个指南针拿奖呢。
<<elseif $homemadecompass is 0>>我的宇航服的指南针还找不到北，无所谓了。<<endif>>
<<if $hurtankle is 1>>我现在真不想让脚踝负重……但是我看也没别的办法了。<<endif>>
我快到的时候再联系你。直到……
[[delay 60m|neartopeak]]


:: neartopeak
事情……有点奇怪。我给你说说？
<<choice [[说吧。|layitonme]]>> | <<choice [[不用了。|preferredsilence]]>>


:: layitonme
谢了。话说……你知道为什么这个山峰看起来有点，我也不知道怎么说了，遥不可及吗？
我本来以为这只是视觉上的错觉……
比方说，我确实接近了，但这个卫星上的沟沟壑壑欺骗了我的眼睛……
但是现在我也不太确定了。我知道这听上去挺疯狂的，但是……
……算了，无所谓吧——这事听上去挺疯狂，是因为它本身就很疯狂。算了，当我没说。
<<choice [[好吧，不提这事。|yeahforgetit]]>> | <<choice [[等等，告诉我！|waittellme]]>>


:: preferredsilence
哎哟，好吧，那算了。
我本来想说件你肯定闻所未闻的事情……不过还是算了。
<<choice [[对不起。请告诉我吧。|waittellme]]>> | <<choice [[随你便。我无所谓。|shutupalready]]>>


:: shutupalready
那么我关了通讯器继续走了。这样对你我都好。
到那里再联系你。如果我能到的话。
[[delay 45m|finallyatpeak]]


:: waittellme
听我说，这简直是疯了，但是……感觉那个山峰有点变化。怎么说呢，连存不存在都说不清。
我一直盯着它，稍微一眨眼，就不在了。
然后，又出现了，我怀疑我的脑袋坏了。
自从我进入陨石坑，这种情况发生好多次了。
<<choice [[好吧，确！实！很！奇怪。|reallyweird]]>> | <<choice [[你肯定是出现幻觉了。|justanillusion]]>>




:: yeahforgetit
好，你说得对。我这是瞎操心，根本就没啥可担心的。
只是……算了，不想了。再也不去想了。
<<choice [[继续啊，说给我听。|waittellme]]>> | <<choice [[没错。|shutupalready]]>>


:: reallyweird
我知道，对吧？
不过真正要把我逼疯的是：我已经走了大概一个多小时了，基本上都是直走的。
可就在大约一分钟前，我一低头……
……在沙子中发现了另外一串脚印。
我完全没有那些心灵鸡汤中讲的“追随前人足迹”的感觉。
我是说，那些脚印和我自己的形状大小完全一致。就好像我又重新走上了同样的路一样。
这怎么可能呢？
<<choice [[这当然不可能。|itisnt]]>> | <<choice [[你确定是你自己的脚印？|sureyours]]>>


:: justanillusion
我知道，那是唯一说得通的解释。时空里的东西是不会无故消失的，绝对不会！
不过真正要把我逼疯的是：我已经走了大概一个多小时了，基本上都是直走的。
可就在大约一分钟前，我一低头……
……在沙子中发现了另外一串脚印。
我完全没有那些心灵鸡汤中讲的“追随前人足迹”的感觉。
我是说，那些脚印和我自己的形状大小完全一致。就好像我又重新走上了同样的路一样。
这怎么可能呢？
<<choice [[这当然不可能。|itisnt]]>> | <<choice [[你确定是你自己的脚印？|sureyours]]>>


:: sureyours
好吧……我是说……不是我的还能是谁的呢？
这些脚印和我的尺寸相同，连鞋底花纹都一样。
搞不懂，怎么我既在直走，同时又在兜圈子。
我不想再想了，我打算一门心思往前走。
[[delay 45m|finallyatpeak]]


:: itisnt
我也是这么想的。不过……我还是要吓得尿裤子了。
这些脚印和我的尺寸相同，连鞋底花纹都一样。
搞不懂，怎么我既在直走，同时又在兜圈子。
我不想再想了，我打算一门心思往前走。
[[delay 45m|finallyatpeak]]


:: finallyatpeak
我到了，我终于到了。我简直不敢相信。
我知道，人们经常说“绝不”认为他们会实现某个目标……
按我说，那都是扯淡。不过，我不一样。字面意义的，我本来“绝不”认为我真的能走到。
这座山太大了，比我想的大多了！不只是更高，而是整体更大。
在我左侧大概50码远处，有块凹下去的地方。 
我应该首先检查那个还是侦察周边呢？
<<choice [[侦察周边。|scoutperimeter]]>> | <<choice [[检查凹处。|checkopening]]>>


:: checkopening
<<silently>><<set $peakdoorway = 1>><<endsilently>>
好吧。我正在努力保持镇静，但我得告诉你，这可真是难办到啊。
因为在山壁上的凹处：
是一个门道。
我的意思是，这不像是什么天然洞的入口。
所以我管它叫门道。到处都规规整整的，高低大小符合人体的比例，
我简直不知道应该怎么处理这个信息！
<<choice [[穿过那道门。|gothroughdoor]]>> | <<choice [[继续探索。|exploresomemore]]>>


:: scoutperimeter
好的，我先绕到右边去看看。等我从另一个方向过去，再看看那个入口。
第一印象：就像我第一天说的，从瓦里亚号那里眯眼看这个……
从几何角度上讲，它简直奇妙得鬼斧神工。
不像地质气候变迁天然形成的，简直像有人用这颗星球上又厚又重的白岩石雕出来的。
是的，我知道，说是这座山是人建的，这想法也太疯狂了……不过今天，我也是过来人了，什么疯狂没见过。
（哎，本来想说安慰自己的话的，但显然没达到效果。）
要记得——
我滴神啊。
<<choice [[出什么事了？|whatdidyoufind]]>> | <<choice [[你还好吧？|areyouokay]]>>


:: exploresomemore
好的，对，这样也许能让我有点时间思索一下。
我要绕着走了。等我从另一个方向过去，再看看这个门道。
第一印象：就像我第一天说的，从瓦里亚号那里眯眼看这个……
从几何角度上讲，它简直奇妙得鬼斧神工。
不像地质气候变迁天然形成的，简直像有人用这颗星球上又厚又重的白岩石上雕出来的。
是的，我知道，说是这座山是人建的，这想法也太疯狂了……不过今天，我也是过来人了，什么疯狂没见过。
（哎，本来想说安慰自己的话的，但显然没达到效果。）
要记得——
我滴神啊。
<<choice [[出什么事了？|whatdidyoufind]]>> | <<choice [[你还好吧？|areyouokay]]>>


:: areyouokay
还好，我是说，从生理上说，我还好。
我只是遇到了此生最害怕的时候。
[[whatdidyoufind]]


:: checkforhbo
虽然我觉得现在要是能看《我是歌手》什么的就太妙了……
……不过我觉得火快要烧到眉毛了……
当然，你要有时间，想看就看，我也不逼你。
[[scannerscreen]]


:: whatdidyoufind
你绝对不会相信的，但是……
……山壁上有字！
<<choice [[字？有没有搞错？！|writingseriously]]>> | <<choice [[写的是什么？|whatsitsay]]>>


:: broadcastingsos
<<silently>><<set $sendingsos = 1>><<endsilently>>
太棒了！我成功中断了通讯信号，输入了一段SOS信息……
……真怂！为什么我不懂用多种语言写SOS，但愿这三个字母宇宙通用吧……
……然后重新进行广播。
只要有人能进入这片星区——哎，我也不知道这信号有多强，没准只要靠近……
他们就会知道我在这里了。
虽然我也不知道“这里”究竟是哪里。
还好，折腾了这么久，我终于头一回有希望了……
[[disappearance]]


:: scannerscreen
如果这个地方能广播（确实可以），而且我可以进入广播中心（确实可以）……
……那我不发送SOS的每一秒都是浪费！
给我点时间，研究下能不能搞定。
[[delay 5m|broadcastingsos]]


:: trycomputers
哦，大哥！撞大运了！
这好像是个全星区扫描仪。而且大多数电线都是连在这上面的。
这些，哦……哇喔，我觉得这些电线构成了广播天线。
这就意味着，这个山峰，这整个结构设置……可能是什么广播塔。
坐落在这个巨坑中心……难道这个巨坑就是个人造卫星天线的锅盖？
那真是个超大型锅盖了！直径五英里。
如果不是因为我现在脑子里一团浆糊，我肯定会去找找有没有好看点的电视。
<<choice [[检查那个屏幕。|scannerscreen]]>> | <<choice [[看看他们有没有电影频道。|checkforhbo]]>>


:: buildathing
不像小飞船那边，这里什么都是完完整整的。
连椅子都结结实实地固定在地上。没法移个什么东西来垫脚站高点。
而且最好别这么干，像我这么倒霉的人，可不能走了这么远，就是为了来摸天花板摔断脖子吧。
我还是去检查一下电脑吧。
[[explorecomputers]]


:: explorewiring
这里真没什么好瞧的了。
这捆线粗得就跟树干一样，直径得有十八英寸吧？
它们都是从天花板里一个大小刚好合适的洞里伸进去的。
要想弄清楚连着的是什么，唯一的方法砸开天花板上的石头……
我既没锄头又够不着那么高，显然这个想法行不通。
<<choice [[要不要搬点东西来垫脚？|buildathing]]>> | <<choice [[去看看电脑。|explorecomputers]]>>


:: breathableair
对，这里的空气可以呼吸……至少对我来说可以。我的宇航服说空气的主要成分是氮气。
不过氧气的浓度足够让我的肺满意，另外二氧化碳的含量几乎为零。
还有，我承认我没想过之前在走廊里从我身边跑过去的究竟是什么东西。
不过那些东西吧……很小。要用寸和两来计量。昆虫？或者别的节肢动物？
但是，现在接近山峰不管是什么……都跟我差不多大。
说不定就是人……
不过，万一不是人呢？这个想法，让我没法夺门而出张开双臂热情地欢迎他们。
我打算安定下来，做自己弄得舒服一点（注：我一辈子从没有这么不安定不舒服过）……
……等着客人上……
[[scuttlingrats]]


:: goodlogic
谢了。我之前完全没有应对这种情况的经验（我觉得大多数人都没有吧）……
……所以听说我没扯淡，果然信心大涨啊。
听着——可能你听起来觉得是胡话，但是我不能排除这些人可能……不是人。
而是什么把我当威胁的本土生物。
或者，更糟……把我当美餐。
哎，我知道听起来不太现实。但这个星球还没证明适合生命存在。
我没看见任何水存在的证据……没有河流，没有云，也从没见过一棵植物。
<<choice [[不过，空气能呼吸。|breathableair]]>> | <<choice [[早先那些生物是什么呢？|creaturesearlier]]>>


:: whatsitsay
我读不懂。
写的是……我99%确定这是中文。
之前那艘小飞船看上去像是中国来的。会不会……有幸存者？有人到过这个山峰？
我们怎么会从没听说过有中国飞船坠毁在这颗卫星上过——更何况还有幸存者？！
难道是他们的国家空间管理局故意掩盖了这件事？那为什么呢？出了……
出了什么事?
除了围着这东西转，我还能做什么呢？
我忘了准备本汉英词典。真是要啥没啥。
好吧，继续侦测。
<<if $homemadecompass is 1>>[[delay 10m|compasscraziness]]
<<else>>[[delay 10m|nocompasscraziness]]
<<endif>>


:: writingseriously
对，真的。
我觉得在这里的每时每刻都在刷新我的疯狂指数。
不知道还能高到哪里去……
还有，这些神秘文字最糟的地方是：
[[whatsitsay]]


:: donttrycomputers
根据至今为止的发现来判断，我对电脑上可能会出现的东西还很有点紧张呢。
也许，在这种情况下，傻人有傻福。我还是……
[[disappearance]]


:: staywhereyouare
对，可能这是最佳方案。给我自己一点时间做心理准备。
（简直像真有办法做心理准备一样。）
毫无疑问，他们是在朝我在的地方来。
那么之后……会？
我得救了吗？
<<if $power is "beacon">>瓦里亚号的信标真带人来救我了吗？<<endif>>
我有点时间。可以看看这些电脑还能怎么用。
<<choice [[对，仔细瞧瞧。|trycomputers]]>> | <<choice [[别乱动。|donttrycomputers]]>>


:: areyousure
在我短暂——最近，还很疯狂——的生命里，我从来没有这么确定。
那是一个人。
并且那个人正朝这！里！来！
<<choice [[出门迎接。|gomeetthem]]>> | <<choice [[留在原地。|staywhereyouare]]>>


:: whatproximity
在警报器的显示屏上……
……有什么东西在动。
山峰外面，有什么东西。在向山峰移动。在向我移动。
有点像……
……人。
<<choice [[你确定吗？|areyousure]]>> | <<choice [[那怎么可能？|howisthatpossible?]]>>


:: awakecomputer
好了，我开机了……
……有个语言选择菜单？真没想到。
选英文吧，英文在最上面，而且我懂。
好啦，屏幕上显示：
[所有系统：运行正常]
[正在广播]
[脉冲倒计时：]
然后有一大串数字滚得飞快，我来不及读。
无论这“脉冲倒计时”指的是什么，我们都是在倒计时。
可是这个想法没给我带来什么信心。
旁边还有一台闲置的显示器。把它也打开吧。
<<if $proximityalarm is 1>>这个屏幕看着甚是眼熟：和我之前在小飞船上用过的近距警报器很像。
<<elseif $proximityalarm is 0>>哦，我在瓦里亚号上见过这玩意儿。它是个近距警报器。<<endif>>
型号有点点不同，铃声和警报声不一样，但至少我认识这玩意儿。
也在运转，那个……
不！
不可能！
<<choice [[你看见了什么？|whatproximity]]>> | <<choice [[你还好吧？|okayproximity]]>>



:: howmanytotal
我已经数到了29、30……这个卫星的表面我还没看全呢。
我的天啊，这里到底坠毁了多少艘飞船啊？
为什么新闻上从来没播过？这里简直是外太空百慕大三角！
这里还有很多显示器。这台没工作。
<<choice [[启动它。|awakecomputer]]>> | <<choice [[别管它。|asleepcomputer]]>>


:: whatscrolling
这里还有一个明亮的绿块块。在瓦里亚号的西南方。
并且……哦，不会吧。
还有一个。
拉远镜头，尽量拉远。
我滴神的妈哟！
几十艘！
在这颗卫星上，有几十艘遇难船！
怎么……怎么会？
这……这不可能。
<<choice [[一共多少？|howmanytotal]]>> | <<choice [[离开那里。|getoutnow]]>>


:: explorecomputers
这……这太疯狂了。
这个屏幕显示的是区域地形图。虽然有些像素化，但是还是能看出来究竟是什么的。
它聚焦在这个山峰，在我只走了一半的巨坑里面。
不过我可以放大缩小，还可以拖拽。我要是往……对了。是小飞船的残骸。
在屏幕上看像是黑暗背景上的一块明亮的破砖。
如果我继续向南移，越过巨坑……越过峡谷……
……就是瓦里亚号，从屏幕上看到的这两个发光的亮块。
这简直超现实。这些影像到底是从哪儿来的？上头有什么人造卫星吗？人造卫星覆盖网？
不知道这个影像覆盖这颗星球的多大范围？比方说，要是我接着拖，那……
哦，天啊。
<<choice [[什么？是什么？|whatscrolling]]>> | <<choice [[你还好吧？|okayscrolling]]>>


:: tellmenow
好了。我站在一间屋里，别忘了，这是在山肚子里。
这里是……我觉得可以称为“控制室”。
我不知道，这里到底控制着什么……我可想不出让人松口气的答案。
这个房间本身并不大，不过我站在里面没有问题，还能稍微走走，也就这么大了。
但是在墙上……有电脑系统。
正在运作的电脑系统。
有些我认得的品牌，各种显示器和CPU组装起来的。
电脑前面甚至还有几把椅子……
……很明显是从什么旧飞船的驾驶舱里弄出来的，然后改装建造出这么个恐怖的工作站。
这是个外星的月亮啊！
这些肯定都是飞船上捡来的，全都是：电脑、发电机、接线……
哦，对，还有这个东西，接线。
很多很多根线。捆在一起。全都从电脑上通到房间天花板上的一个洞里。
然后就不知道通到哪里去了。
<<choice [[检查电脑。|explorecomputers]]>> | <<choice [[检查接线。|explorewiring]]>>


:: interiorpeak
……除非这里有，嗯，电灯。
什么鬼！
我……我都不知道应该怎么描述。
我不知道本来指望来这个山峰找什么，但我保证……一定不是这个。
<<choice [[告诉我！|tellmenow]]>> | <<choice [[慢慢来。|takeasecond]]>>


:: followthosethings
那些……生物（这么说很怪，我本来还以为不会在这里用到这个词呢）——
总之，那些生物在我前面，正朝亮光跑去。所以，我猜我也该去那里。
为什么在山体里面，越深的地方反而越亮？无论怎么都说不通啊……
[[delay 3m|interiorpeak]]


:: whatjusthappened
<<if $zombierats is 1>>好吧，我知道，之前我在外头也遇到过这种情况。但绝没有这么近。
<<elseif $zombierats is 0>>不知道那究竟是什么鬼玩意儿。但是真的离我很近。<<endif>>
我听到了回声，从那些石墙上弹回来的回声，我甚至没注意到声音本来的源头。
我环顾四周，想找出那嗖嗖的声音从哪里来的……
……然后我看见了这接近地面的微弱的绿光。
才一秒钟，然后它就分解成……我发誓，一堆小眼睛。
就在我还在目瞪口呆之时，其中一只……不管是什么……
……碰到了我的腿。
它没有伤害我，只是马上弹开了，然后和其他的同伴，跑进了山体深处。
不过……这说明……我在这里不是孤零零的一个人。
我还是更喜欢就我一个人。
<<choice [[跟着那些……东西。|followthosethings]]>> | <<choice [[快离开那里。|getoutofthere]]>>


:: gothroughdoor
进去了。
穿过门道，是一条普通的过道，四周都是岩壁。
虽说“普通”，但是符合人的尺寸，这点值得一提。
随着户外的光线逐渐消失，这里就漆黑一片，一点都缓解不了我的紧张神经……
……但前方有光，在远处。
<<if $glowrods gte 1>>现在，我觉得我可以点亮一根荧光棒——
<<else>>我要狂奔了，我不可想困在暗处……<<endif>>
噢呀，我嘞个去！
<<choice [[出了什么事？|whatjusthappened]]>> | <<choice [[你还好吧？|whatjusthappened2]]>>

:: whatjusthappened2
[[whatjusthappened]]

:: creaturesearlier
哎，我承认我没想过之前在走廊里从我身边跑过去的究竟是什么东西。
不过那些东西吧……很小。要用寸和两来计量。昆虫？或者别的节肢动物？
但是，现在在接近山峰的不管是什么……都跟我差不多大。
这里的空气可以呼吸……至少对我来说可以。我的宇航服说空气的主要成分是氮气。
不过氧气的浓度足够让我的肺满意，另外二氧化碳的含量几乎为零。
也许外面那些也是人，也在呼吸空气……
……不过，万一他们不是人呢？这个想法，让我没法夺门而出张开双臂热情地欢迎他们。
我打算安定下来，做自己弄得舒服一点（注：我一辈子从没有这么不安定不舒服过）……
……等着客人上……
[[scuttlingrats]]


:: somethingworse
听着，你多半会觉得我疯了，但是我不能排除这些人可能……不是人。
而是什么把我当威胁的本土生物。
或者，更糟……把我当美餐。
哎，我知道听起来不太现实。但这个星球还没证明适合生命存在。
我没看见任何水存在的证据……没有河流，没有云，也从没见过一棵植物。
<<choice [[不过，空气能呼吸。|breathableair]]>> | <<choice [[早先那些生物是什么呢？|creaturesearlier]]>>



:: stayrightthere
没错，我肯定要待在这里。
如果这是一支救援队的话，那我就待着这里坐等获救就好了。我已经望眼欲穿了。
如果是其他坠毁飞船的船员的话……他们可能已经被几天，甚至几周的跋涉逼疯了。
（我是说，我还只是从前天开始游荡，就已经半疯了。）
我最好等着，看看他们是怎么应对这个地方的，并且确保他们知道我也是这里的外来人员。
话说……如果他们不是救援队或者幸存者的话……如果他们是更险恶的别的什么。
……那我肯定不会傻乎乎地把自己送到他们手上的。
<<choice [[分析得不错，很全面。|goodlogic]]>> | <<choice [[“更险恶”？|somethingworse]]>>


:: tryforgoodnews
对，我也是这么想的。
我这么说吧：如果事情往“好消息”方面发展的话，那我就要真的，真的得救了。
因为本来只有一个人形生物在朝我这边来……
<<if $capalive is 1 and $power is "pod">>现在是四个。
<<else>>现在是五个。<<endif>>
<<choice [[出门迎接。|gooutandmeetthem]]>> | <<choice [[留在原地。|stayrightthere]]>>


:: wegotdisconnected
我一点都不惊讶我们失联了。我觉得……应该怎么说呢？我瞬间偏离了一下现实？
[[checkthetranscript]]


:: justhappened
我不知道从你那边看是什么样，但是从我站的地方看过来……
（好了，还是说实话吧，从我紧张地缩着的那张从飞船上拆来的椅子上看）——
突然！到处！传来嗡嗡的声音，然后我的视线变得有点模糊……
……我只来得及看了一眼显示器，之前倒数的那些数字都已经归零了……
……然后我感觉整个世界……都崩塌了。
就好像所有的东西都在离我而去一样，同时我也在迅速地离它们而去。
我还没明白在发生什么，就结束了。一切都恢复“正常”了（这个词我用得真是太轻率了）。
不过……我感觉我刚刚经历了之前在山峰外遇到过的那种感觉，那种闪一下，消失一下的感觉……
……只不过这次我是在山峰里。
而且我也和它一起闪了一下，消失了一下。
<<choice [[我们刚刚失联了。|wegotdisconnected]]>> | <<choice [[你还好吗？|feelokay]]>>


:: checkthetranscript
对，通讯器显示了“时间戳无效”的讯息。
那么……它读取的时间到底是什么？
这真是……
哦，嘿。我，唔……我有消息要报告。
不确定是好消息还是坏消息。
<<choice [[说吧。|sharethenews]]>> | <<choice [[试下“好”。|tryforgoodnews]]>>


:: compasscraziness
咳！我嘞个去！
我手里拿着我自己做的指南针沿着山脚走，看着它和我一起慢慢移动，就像它该做的那样……
……突然指针开始前后乱指，像个趴上喝高了乱扭的小屁孩。
然后，整个山峰……闪着绿光，消失一下。
我既没有眨眼，也没有看别处。我把手伸向空中刚才它在的地方，什么也没有，只有空气。
然后我把手猛地抽了回来，因为它又出现了。
现在我的指南针又正常了。
我嘞个去啊！
<<choice [[简直疯了。|thatsinsane]]>> | <<choice [[肯定是你的错觉。|mustvebeenillusion]]>>


:: nocompasscraziness
咳！我嘞个去！
我刚才正在绕着山脚走，无所事事地拿手指摸着山壁。
然后，整个山峰……闪着绿光，消失一下。
我既没有眨眼，也没有看别处。我把手伸向空中刚才它在的地方，什么也没有，只有空气。
然后我把手猛地抽了回来，因为它又出现了。
我嘞个去啊！
<<choice [[简直疯了。|thatsinsane]]>> | <<choice [[肯定是你的错觉。|mustvebeenillusion]]>>


:: scuttlingrats
那个嗖嗖的声音又出现了！
哦，大哥！这还是有灯的时候我第一次听到呢。
我说不定能看到是什么东西在响。
无论是什么碰到了我的腿。无论那个绿眼睛发亮的是什么。
（难道我真的想知道这玩意儿到底是什么？）
那个声音好像是来自……等等……好像是在显示器后面。在接线里面到处跑。
<<choice [[去找找。|lookforscuttling]]>> | <<choice [[避开那里。|stepbackscuttling]]>>


:: disappearance
[通讯中断]
[时间戳无效]
[正在搜索……]
[正在重新获取信号]
[正在建立连接]
[正在接收消息]
……怎么了？喂喂喂？喂？
<<choice [[没事，我在这儿。|stillhere]]>> | <<choice [[刚才出什么事了？|justhappened]]>>



:: gomeetthem
对，是可以那么做。我是说，要走到还有一会儿。我可以边走边做好心里准备。
（简直像真有办法做心理准备一样。）
毫无疑问，来的人是在朝我在的地方来。
那么之后……是什么呢？
我得救了吗？
<<if $power is "beacon">>瓦里亚号的信标真带人来救我了吗？<<endif>>
还是别的坠毁飞船的幸存者？比我还惨的家伙？
好吧，我要去会会。管他来的人是谁哦。
我只要……
[[disappearance]]


:: howisthatpossible?
这怎么可能？！
我怎么会在一个山肚子里的电脑控制室里？还是在一个到处是坠毁飞船的卫星上？！
全都说不通，但这是真的。
那是一个人。
并且那个人正朝这！里！来！
<<choice [[出门迎接。|gomeetthem]]>> | <<choice [[留在原地。|staywhereyouare]]>>


:: okayproximity
我真的不知道。
[[whatproximity]]


:: asleepcomputer
听着，我也很害怕。不过我也是过来人了，什么吓人的没见过？
（这简直可以当我的“临终金句”。）
我得去查看一下，你觉得呢？
<<choice [[我想你应该去。|awakecomputer]]>> | <<choice [[小心点。|computercareful]]>>


:: keepatit
我不断拖动地图，不断找到更多飞船。
[[howmanytotal]]


:: getoutnow
不……我……一定得弄清楚这是怎么回事。对吧？
这里一定有什么大！事！难道我不应该知道究竟是什么吗？
<<choice [[对，继续探索吧。|keepatit]]>> | <<choice [[不，从那里出来！|reallygetout1]]>>


:: okayscrolling
唔……不太好。
不好，我整个人都不好了！
[[whatscrolling]]


:: takeasecond
谢了。也许有一天我会见怪不怪的……
……不过我现在还没有修炼出那种定力。
[[delay 5m|tellmenow]]


:: getoutofthere
相信我，这是我的第一直觉。
不过我都走到这步了，现在放弃不是很没有意义吗？
我有没有吓得魂飞魄散？肯定，当然。
不过，不入虎穴，焉得虎子！
<<choice [[你说得对。继续前进吧。|followthosethings]]>> | <<choice [[你疯了。快离开那里。|reallygetout]]>>


:: igetitnow
可能吧。不过要是我死了，你多半也只能遥祭了。
我希望你会为我说点好话，换作我，也会对你这样做的。
好了，我要跟着那些东西。不管是些什么。
[[followthosethings]]


:: goodluckgilligan
我真的很感激你。
我现在要出发了。如果你没有收到我的信息……
[[scuttlingrats]]


:: seriouslymeetthem
如果你真觉得我应该那样做的话，那我就听你的。
<<if $capalive is 1 and $power is "pod">>但是我得告诉你，一挑四，我心里有点不安。
<<else>>但是我得告诉你，一挑五，我心里有点不安。<<endif>>
不过好吧。我还是去吧。
如果我对这幅地图的比例尺没理解错的话，他们大约二十分钟就到了。
所以如果我现在离开，在中途与他们碰面的话……
……那我十分钟以后就能再见到人了。
（希望是人啊。)
我既激动兴奋又惶恐不安。难道我马上就能离开这个星球安全回家了？
还是我要把《鲁滨逊漂流记》一直演下去呢？
祝我好运吧。
<<choice [[祝你好运。|goodluckgilligan]]>> | <<choice [[祝你成功，鲁滨逊。|breakaleggilligan]]>>


:: gooutandmeetthem
老实说，我持怀疑态度。
如果这是一支救援队的话，那我就待着这里坐等获救就好了。我已经望眼欲穿了。
如果是其他坠毁飞船的船员的话……他们可能已经被几天，甚至几周的跋涉逼疯了。
（我是说，我还只是从前天开始游荡，就已经半疯了。）
我最好等着，看看他们是怎么应对这个地方的，并且确保他们知道我也是这里的外来人员。
话说……如果他们不是救援队或者幸存者的话……如果他们是更险恶的别的什么。
那我肯定不会傻乎乎地把自己送到他们手上的。
<<choice [[说真的，出去见他们。|seriouslymeetthem]]>> | <<choice [[“更险恶”？|somethingworse]]>>


:: sharethenews
好吧，我不知道这该怎么解读。
最理想的情况是，我要真的！真的！得救了。
因为本来只有一个人形生物在朝我这边来……
<<if $capalive is 1 and $power is "pod">>……现在是四个。
<<else>>……现在是五个。<<endif>>
<<choice [[出门迎接。|gooutandmeetthem]]>> | <<choice [[留在原地。|stayrightthere]]>>


:: feelokay
我觉得我还好，就像我说的，事情发生得太快了，我都没来得及反应。
如果我停下来思考刚刚出了什么事，我多半要得晕动病了。
你跟现实不同步的时候，真会得晕动病吗？
[[checkthetranscript]]


:: stillhere
哦，哇哟，听到你这么说我就踏实多了。因为有那么一分钟吧，你都不在。
或者说“我”不在。
[[justhappened]]


:: thatsinsane
确实。确确实实。不过！我！清醒得很！我百分百确定经历过这些。
我可以把之前看到的那些都当成光学错觉。
一场光的恶作剧，我疲惫的大脑对我耍的诡计。
但是，这个发生的时候，我就在现场。
绝对超现实，
好啦，好……吧，我要接着绕这座会神奇消失的人造山转转。
正常人会这么说吗？
[[delay 10m|backatdoorway]]


:: mustvebeenillusion
真不是，听我说。我可以把之前看到的那些都当成光学错觉。
一场光的恶作剧，我疲惫的大脑对我耍的诡计。
但是，这个发生的时候，我就在现场。
绝对超现实，绝对不是错觉。
好啦，好……吧，我要接着绕这座会神奇消失的人造山转转。
正常人会这么说吗？
[[delay 10m|backatdoorway]]


:: lookforscuttling
疯了，疯了，疯了。
我干嘛这么起劲儿地在抓窜进接线里的吓死人的外星生物呢？
我告诉过你，我看过超多科幻和惊悚片。
这两天，种种迹象都表明我看得太！多！了！过犹不及，有弊无利。
而且我知道，身上标着“你好啊，我叫泰勒”的人，肯定没有好下场。
好了，那边的声音……更大了，在那台脉冲倒计时的电脑后面。
我觉得，要是能把手指头伸进这层金属板后面的话，那我就能
我的妈
<<choice [[出了什么事？|whatsupscuttling]]>> | <<choice [[你还好吧？|allrightscuttling]]>>


:: stepbackscuttling
对，没错。不管窜进接线里的吓人外星生物是啥，都没有必要起劲儿地去抓，是吧?
我告诉过你，我看过超多科幻和惊悚片。
这两天，种种迹象都表明我看得太！多！了！过犹不及，有弊无利。
而且我知道，身上标着“你好啊，我叫泰勒”的人，肯定没有好下场。
那边的声音……更大了，在那台脉冲倒计时的电脑后面。
我在离脉冲倒计时电脑远点，而不是冒冒失失地冲进
我的妈
<<choice [[出了什么事？|whatsupscuttling]]>> | <<choice [[你还好吧？|allrightscuttling]]>>


:: computercareful
好，我听你的。我弄坏过的电脑比一般人拥有过的还多。
有几台是故意弄坏的（为了瞧瞧它们的工作原理）。几台是意外。一台浇了饮料。（太囧了。）
不过我保证，对这些电脑我会超级小心的，毕竟一个不小心就能把一艘飞船炸落地呢。
我真心不想让我未来的老板知道我这么怂。
[[awakecomputer]]



:: igetitnow1
谢谢你。我知道有时候我像个怪胎，不过……这对我来说真的很重要。
所以谢谢你尊重我的意见。
好了，我要继续在这里探索。
[[keepatit]]


:: reallygetout1
好，我听见你说了，但是……我只是不能，你懂吗？
不管我本来期望要在这里找到什么，现在都完完全全超出了我的想像。
现在——在地图上看到了所有这些飞船——我从没有现在这么确信……
……确信这个地方就是导致瓦里亚号坠毁在这个卫星上的原因。
也是全体船员死亡的原因。
<<if $capalive is 1 and $power is "pod">>阿雅船长现在也只是命悬一线（我祈祷她还活着）。<<endif>>
他们都是好人。好得不能再好的人。
只要是有一丝机会，能在这哪里，找到他们受难的原因……他们死的原因……
……那么，我没有答案是不会走的。
我只是不能。
<<choice [[好吧，我理解。|igetitnow1]]>> | <<choice [[理解，你继续研究地图。|keepatit]]>>



:: reallygetout
我……我不能。还不行。
毫无疑问，确实很吓人，但我还受得了。我知道，这里还值得进一步调查。
我都走到这一步了，就算我想放弃，这里也有一股吸引力……强拉着我，不准我回头。
我还不能离开。我要继续。
<<choice [[好吧。你活该倒霉。|igetitnow]]>> | <<choice [[明白了。跟着那些东西。|followthosethings]]>>



:: breakaleggilligan
谢谢！
我现在就出发。如果你收不到我的信息——
[[scuttlingrats]]


:: backatdoorway
好了，我已经绕山一周了。
我在墙上找不到其它的字迹，也没有时空跳转的闪光了。
（我从没想过我一生中会说这样的话。那怕中了宇宙之旅的大奖时也没有。）
我要回到凹处了。
<<if $peakdoorway is 0>>[[justreachedpeakdoor]]
<<elseif $peakdoorway is 1>>[[reachedpeakdooragain]]<<endif>>


:: whatsupscuttling
有一个把头探出来了！
这是，那些之前发怪声音……从我身边跑过去的东西……
哦，天啊！
是我实验用的小白鼠。
<<choice [[你确定？|certainrats]]>> | <<choice [[那怎么可能？|possiblerats]]>>


:: allrightscuttling
不！不，一点也不好！
这是，那些之前发怪声音……从我身边跑过去的东西……
哦，天啊！
是我实验用的小白鼠。
<<choice [[你确定？|certainrats]]>> | <<choice [[那怎么可能？|possiblerats]]>>


:: makeabreak
好，就这么定了。
<<if $glowrods gte 1 and $hurtankle is 0>>我要折断一只荧光棒照亮走廊准备冲刺。
我左手拿着发光棒，右手握拳。我也就只能准备到这个程度了。
<<elseif $glowrods gte 1 and $hurtankle is 1>>我要折断一只荧光棒照亮走廊准备冲刺。
我左手拿着发光棒，右手握拳。我也就只能准备到这个程度了。
光是想想“冲刺”这两个字我都觉得脚脖子更痛了，不过我还是会冲的。我必！须！这么做。
<<elseif $glowrods is 0 and $hurtankle is 1>>在黑暗中冲过走廊可不好搞。
要做到即快又稳，两者兼顾我从没有成功过。哪怕只快或者只稳，都没做到过。
光是想想“冲刺”这两个字我都觉得脚脖子更痛了，不过我还是会冲的。我必！须！这么做。
<<else>>在黑暗中冲过走廊可不好搞。
要做到即快又稳，两者兼顾我从没有成功过。哪怕只快或者只稳，都没做到过。<<endif>>
虽说……也许我这完全是庸人自扰呢，是吧？
难道来者就一定不善吗？
万一他们是救援队（拜托，拜托，拜托）……
或者和我一样身处困境的幸存者？或者比我还倒霉的幸存者呢？
<<if $rations is 1>>我说不定能给他们些吃的，免得他们饿倒。<<endif>>
<<if $pills gte 1>>说不定他们比我更需要止痛药，这方面我能帮上忙。<<endif>>
说不定其实是我拯救了他们。我们持续广播SOS信号直到有人接收到。
一定，好啦，我越说心里越踏实了。这些都很有道——
哦！天！啊！野小子
<<choice [[野小子？你的死老鼠？|wildboydead]]>> | <<choice [[出什么事了？|wildboydead2]]>>

:: wildboydead2
[[wildboydead]]

:: getoutbecauserats
相信我，我一直在想办法。
只有一条走廊进出控制室。
<<if $capalive is 1 and $power is "pod">>近距警报器显示，外头那四个不知是谁或者什么东西（拜托是人）……
<<else>>近距警报器显示，外头那五个不知是人还是什么东西（拜托是人吧）……<<endif>>
再有几分钟就要进来这儿了。
如果现在脚底抹油，我还可以在他们到达门道之前，冲到外面去。
然后……呢？我只有几秒钟时间估计状况。判断他们是否构成威胁……
<<if $capalive is 1 and $power is "pod">>……如果他们构成威胁……我肯定得选“跑”，因为“打”不是一对四的选项。
<<else>>……如果他们构成威胁……我肯定得选“跑”，因为“打”不是一对五的选项。<<endif>>
我的心现在跳到嗓子眼了。
至少比老鼠的声音响。
那么……你觉得我该怎么做？
<<choice [[逃出去。|makeabreak]]>> | <<choice [[呆在原地，看看会发生什么。|stayputandsee]]>>


:: poorlittlerat
我知道，它从前这么可爱。它不该……天啊，谁都不该轮得如此下场。
（现在一想到我可能也会如此下场心里就相当不安。）
[[getoutbecauserats]]


:: wildboydead
有绿色的……东西……从它的身体里爬出来。
我滴神啊。
这是，哦，这不是地球上的东西。这，这像是，你懂的，同样的那种发光的，荧光绿……
……像是全身都是老鼠眼睛构成的。哦，天啊，它还……我要如何形容……
……它正在将肌肉和血管卷回身体，像是，像是把这些从野小子身体里、四肢里收回……
……就像这，这东西是，我说不清楚，把它当成一个傀儡，一个提线木偶，不过是从身体内部控制的！
<<choice [[是个外星生物吗？|itsanalien]]>> | <<choice [[你必须马上离开！|gottogetaway]]>>


:: itsanalien
是！我从没见过这样的东西！
或者……我猜它是这个星球上的原生生物……也就是说，我！倒成外星人了。
而且它貌似对陌生人不怎么友好。
噢哟，我该怎么办？
<<choice [[你必须杀掉它。|killtherat]]>> | <<choice [[你需要离开。|gottogetaway]]>>


:: justreachedpeakdoor
<<silently>><<set $peakdoorway = 1>><<endsilently>>
好吧。我正在努力保持镇静，但我得告诉你，这可真是难办到啊。
因为在山壁上的凹处：
是一个门道。
我的意思是，这不像是什么天然洞的入口。
所以我说这是个门道。到处都规规整整的，高低大小符合人体的比例，给我这样大小的人建的。
给我建的？
我简直不知道应该怎么处理这个信息！
<<choice [[冷静思考一下呢？|processtime]]>> | <<choice [[穿过那扇门。|gothroughdoor]]>>


:: reachedpeakdooragain
看上去还是像是从岩石里挖出来的门道。给我这样大小的人建的。
给我建的？
不知道是不是只是看着像而已，但我现在怕得头皮发麻。
<<choice [[冷静思考一下呢？|processtime]]>> | <<choice [[你必须进去。|gothroughdoor]]>>


:: possiblerats
我怎么知道？我没在瓦里亚号的事故中丧生，那些老鼠可能也幸存下来了。
<<if $ratpellets is 1>>它们的笼子烂成那样了，但我从没见到过它们尸体。<<endif>>
……这些绝对是我！的！老鼠。第一个探出头的，是眼罩。
起这个名字是因为它全身都是白色的，除了右眼上方有块棕色。
像是画了个钩，或者说像是个海盗的眼罩。非常独特，这绝对是它，毫无疑问。
不同的是，那块斑曾长在一只粉红色小眼睛的上方——所有这类实验用小白鼠都有这种粉红小眼。
可现在……那只眼睛变成了绿色的，荧光绿，闪闪发亮。
正看着我。
<<choice [[你所有的老鼠都在那里吗？|allratsthere]]>> | <<choice [[别靠近。|donotgetclose]]>>


:: certainrats
非常确定。第一个探出头的，是眼罩。
起这个名字是因为它全身都是白色的，除了右眼上方有块棕色。
像是画了个钩，或者说像是个海盗的眼罩。非常独特，这绝对是它，毫无疑问。
不同的是，那块斑曾长在一只粉红色小眼睛的上方——所有这类实验用小白鼠都有这种粉红小眼。
可现在……那只眼睛变成了绿色的，荧光绿，闪闪发亮。
正看着我。
<<choice [[你所有的老鼠都在那里吗？|allratsthere]]>> | <<choice [[别靠近。|donotgetclose]]>>


:: stayputandsee
我……好吧。我本来想顶个嘴呢，但也许这才是万全之策。
我是说，也许我这完全是庸人自扰呢，是吧？
难道来者就一定不善吗？
万一他们是救援队（拜托，拜托，拜托）……
或者和我一样身处困境的幸存者？或者比我还倒霉的幸存者呢？
<<if $rations is 1>>我说不定能给他们些吃的，免得他们饿倒。<<endif>>
<<if $pills gte 1>>说不定他们比我更需要止痛药，这方面我能帮上忙。<<endif>>
说不定其实是我拯救了他们。我们持续广播SOS信号直到有人接收到。
好啦，我越说心里越踏实了。这些都很有道——
哦！天！啊！野小子
<<choice [[野小子？你的死老鼠？|wildboydead]]>> | <<choice [[出什么事了？|wildboydead2]]>>


:: swearalot
谢了。要是再出别的乱子的话，我保证一定拿你出气。
现在另一个重要问题是，我能不能找到办法，警告那艘前来的飞船？
很不幸，答案是：
[[warnshiptwo]]


:: sickfromtherats
对不起。我没能控制我的呕吐反射。
眼罩刚刚撕开了野小子的内脏，我转身吐了。一般人都会。
我的呕吐物也有些绿色。我正绞尽脑汁思考这件事的含义。
它们怎么还尖叫个不停？
<<choice [[再扔些食物给它们。|throwratsmorefood]]>> | <<choice [[快离开那里。|getoutbecauserats]]>>


:: throwratsmorefood
<<if $rations is 1>>对，没错，有道理。一般情况下我都很节约粮食的……
……但我知道小飞船里还有很多可以拿呢。
（而且，很明显，这个卫星到处是坠毁的飞船，就跟多动的小屁孩们的房间里乱扔的玩具一样，还有的是地方找吃的呢。）
<<elseif $ratpellets is 1 and $rations is 0>>对，没错，有道理。我只要……我要把剩下的鼠粮给它们。
这就是说，我自己没吃的了，——所以我真心希望无论如何救援队赶快到来。<<endif>>
不管怎么样，这些老鼠之前吃东西时挺安静的。
而现在，我唯一希望的就是它们不要再怪叫了。
给，伙计们。祝你们吃得开心。
<<choice [[有用吗？|isitworking]]>> | <<choice [[你得赶快离开了。|getoutbecauserats]]>>


:: isitworking
好像真的有用。
它们疯狂地抢着食物，不过安静下来了，好歹不再纠结于野小子的尸体了。
<<choice [[那可怜的小家伙。|poorlittlerat]]>> | <<choice [[你得赶紧离开才是！|getoutbecauserats]]>>


:: gottogetaway
相信我，我在跟你讲述情况的同时就退到了相反的那个角落里。
我是说，我体内流着的科学家的血液让我对见到的未知生命体有着浓厚的兴趣——
……我的宇航服上的相机要不是在第一天就罢工了的话，那我就要将这一切都记录下来了……
……但这并不是说，我没准备好随时脚底抹油。
<<choice [[想办法捉住它。|capturetherat]]>> | <<choice [[你必须杀掉它。|killtherat]]>>


:: killtherat
杀掉它？用什么杀啊？说的好像我现在手里就卷着份报纸可以当作武器一样。
而且，A，这是一种未知物种，人类初次见面的时候是不会上来就起杀心的。
B，我要是对受伤的那只发起攻势的话……谁能跟我保证剩下的三只不会反过来攻击我呢？
<<choice [[你说得对。那就想办法捉住它吧。|capturetherat]]>> | <<choice [[你还是杀了它比较好。|killratforreal]]>>


:: onlyifsure
不，我当然不确定。我怎么可能对这种事确定呢？
要我说，里面一定有一门巨大的激光炮，上头刻着“泰勒”两个字……
只要我踏进那扇门，它就会轰掉我的脑袋。
经过今天的这些事，还有什么不可能？
不过我都走到这步了，还是继续前进吧。
[[gothroughdoor]]


:: readytogoin
我觉得我准备好了。我是说尽可能地准备好了。
你觉得呢？
<<choice [[是时候进去了。|gothroughdoor]]>> | <<choice [[除非你真的确定了。|onlyifsure]]>>


:: processtime
好啦，那我就稍微在这里坐一会儿，冷静一下。
我可以吃点东西，不过我没胃口。
我这辈子都没有这么紧张过。我的鸡皮疙瘩都起了几层了。
给我几分钟，然后……搞不懂，也许我就能做好进去的准备了吧。
或者尖叫着跑开。
[[delay 5m|readytogoin]]


:: donotgetclose
嗯，我不会太接近他们，这样就好。
一方面，我对这些老鼠了如指掌，比起实验对象我更愿意把它们当作是朋友……
……但是，另一方面，那是在它们的眼睛变成吓人的荧光绿之前。
且不必说……它们和我在这星球上跋涉的距离差不多。
<<if $zombierats is 1>>它们发现了小飞船的残骸，然后它们找到了这里。<<endif>>
难道它们是在跟着我？还是……是山峰把它们吸引过来的？真是可怕的想法……
但同时，要知道，它们那发光的绿色眼睛和发光的绿色山峰确实有……
“发光的绿色”，这个共同点。
<<choice [[你所有的老鼠都在那里吗？|allratstwo]]>> | <<choice [[老鼠们在做什么？|whatareratsdoing]]>>


:: allratsthere
我……我想是的。眼罩和小矮子最先出现，我往后退躲开它们时差点没把自己绊倒。
之后小山子在它俩之间挤了出来，我能听到显示器后方还有吱吱声……
我猜那是野小子，那么我所有的老鼠应该都在这里了。
这个卫星上……连着发生了这么一串不可思议的事情，但这绝对是个中之最。
<<choice [[是的，毫无疑问。|nodebating]]>> | <<choice [[别靠近。|donotgetclose]]>>


:: hopeitworks
希望如此。话说你要是爱祈祷的人的话，那帮我祈祷一下吧。
<<if $warnship is 0>><<choice [[遵命！|yougotprayers]]>> | <<choice [[你能警告那艘船吗？|warnshiptwo]]>>
<<elseif $warnship is 1>><<choice [[遵命！|yougotprayers]]>> | <<choice [[你的访客离你还有多远？|howclosearevisitors]]>><<endif>>


:: yougotprayers
非常感谢。但愿祈祷能弥补我昨晚冷得要死的时候爆粗口的大不敬。
<<choice [[想骂就骂。|swearalot]]>> | <<choice [[你能警告那艘船吗？|warnshiptwo]]>>


:: hardtosayoverwarn
难说。这里的一切都很难说。
我要是能活着回家的话，我觉得我一定会跟去威利旺卡的巧克力工厂参观的那些小屁孩一样患上创伤后应激障碍的。
<<choice [[不过我的虫牙会比他们少一些。|fewercavitiesoverwarn]]>> | <<choice [[你的访客离你还有多远？|howclosearevisitors]]>>


:: fewercavitiesoverwarn
哈！那还真是超大灾难中的一丝丝欣慰啊，哥们儿。
[[howclosearevisitors]]


:: horrifyingrats
真的，你简直想像不到。
我是说，我从前做科研实验也会出差错，多到数都数不过来。
用泡打粉而不是小苏打制造惨烈的烂纸糊的火山。
用玉米淀粉加水配制非牛顿流体，沾满校车椅子。
给我的朋友达里尔的仓鼠喂巧克力观察喂多少才能喂死。
（不过那是个意外！我们当时没有留心到大块头和巧克力蛋糕。哇，科学。）
只是这个——
我的天，等下，等下，等下
[[delay 1m|sickfromtherats]]


:: sureyouwont
我要是有你那种信心就好了。
我是说，我知道这完全是心理作祟，但我一想到这种空气可能会导致……
我感觉我的呼吸比之前困难了十倍。我的肺部开始疼了起来……而且还变绿了。
（我知道这听起来很荒谬，但是我真有这种感觉。）
不过我说不定会撞大运。毕竟我比那些老鼠大多了。
或许我还要吸入很多空气才会出现副作用。
而且，都感觉不到……我正在被干掉。
我只希望……
我的天，等下，等下，等下
[[delay 1m|sickfromtherats]]


:: breathedummy
是啊，我知道。我说不定会撞大运。
毕竟我比那些老鼠大多了。或许我还要吸入很多空气才会出现副作用。
而且，都感觉不到……我正在被干掉。
我只希望……
我的天，等下，等下，等下
[[delay 1m|sickfromtherats]]


:: capturetherat
用什么抓？我告诉过你，这里不像在小飞船，周围没有随手能用的垃圾。
我要早知道需要个鼠笼，或者盖子上有小洞的密封瓶子，就打包带来了。
还要带个烟灰缸、大球拍、遥控器、火柴、台灯、椅子，就这些吧。
……啊哦。电脑闪瞎我了。
<<choice [[它说什么？|whatdoescomputersay]]>> | <<choice [[肯定不是好事。|thatcanonlybebad]]>>


:: killratforreal
我身上唯一的武器就是我的鞋底子！
你要是觉得我会直接用脚去把那种小绿东西跺成绿泥的话……
……那你真是多虑了。虽然我流落这里，历尽千辛万苦，但我是不会那么做的。
我只是……哎，我希望这座山峰内外的一切，无论是人还是物，还有马上要到的那些家伙，都是有人性的……
……我要把野小子的尸体埋起来。
我知道这听起来很蠢，但我对这些陪伴着我旅行的小家伙们真的很有感情。
它们也有离奇的经历，应该体面地下葬。
……啊哦。电脑闪瞎我了。
<<choice [[上面说什么？|whatdoescomputersay]]>> | <<choice [[肯定不是好事。|thatcanonlybebad]]>>


:: allratstwo
我……我觉得是。眼罩和小矮子最先出现，我往后退躲开它们时差点没把自己绊倒。
之后小山子在它俩之间挤了出来，我能听到显示器后方还有吱吱声……
我猜那是野小子，那么我所有的老鼠应该都在这里了。
这个卫星上……连着发生了这么一串不可思议的事情，但这绝对是个中之最。
<<choice [[是的，毫无疑问。|nodebating]]>> | <<choice [[老鼠们在做什么？|whatareratsdoing]]>>


:: nodebating
我要是能活着回去，那我的回忆录一定会被归类为“科幻”类书籍……
因为根本不会有人相信这是我在这里的真实经历啊。
[[whatareratsdoing]]


:: tendtotherats
我就跟你说实话吧：我知道这些曾经是我的小宝贝，但我实在不想再靠近它们了。
它们实在是……有点不对劲。
<<if $ratpellets is 1>>不过我能在墙角撒些鼠粮给它们吃。
<<elseif $ratpellets is 0 and $rations is 1>>不过我能拆一包即食口粮给它们吃。
芝士意大利面？没问题。听上去像是太空鼠会吃的东西。我要在墙角撒一些。<<endif>>
我觉得，它们现在多半已经饿坏了。我真是不知道这几天它们靠什么活过来的。
给它们点吃的，它们可能就不会挡路了，然后我就能去看看那些电脑了。
<<choice [[想法不错。|goodthinking]]>> | <<choice [[你也应该吃一些。|youeattoo]]>>


:: wasittherats
嗯？不。唔，和这些老鼠没关系。这是……另一台电脑上的东西。
[[pulsetargeting]]


:: overridetarget
<<silently>><<set $overridetarget = 1>><<endsilently>>
不知道，我也想啊，不过——不像我钟爱的科幻烂片……
这里没有写着“手动操纵”的红色大按钮。
我对电脑的了解仅限于下载部大片或编个冒险小游戏玩……
但是黑进这个，改写武器系统？
除非密码就是“mima”，否则我是无能为力的。
不过死马当活马医吧。
<<choice [[但愿能成功。|hopeitworks]]>> | <<choice [[你能警告那艘船吗？|warnshiptwo]]>>


:: warnshiptwo
<<silently>><<set $warnship = 1>><<endsilently>>
不知道，不过我可以试试看。就像之前发送SOS信息那样再发一条普通信息。
但愿他们能看得懂英文。话说……我这真不知道应该警告他们些什么。
我完全不知道是对准他们的是什么东西。导弹什么的？还是……
不，当然不是。泰勒，你个蠢货。是脉冲啦。
多半是一股电磁脉冲。用种什么能定向能量的武器，粒子束武器？
我滴神啊。一股强大的能量波，在真空中的破坏力，那可真是超乎你的想象啊。
你完全可以将瓦里亚号那样的飞船打成两截。
<<choice [[一定就是这样的。|mustbeitoverwarn]]>> | <<choice [[你的访客离你还有多远？|howclosearevisitors]]>>


:: mustbeitoverwarn
对，瓦里亚号，还有鬼知道多少艘船……所有在地图屏幕上闪着绿光的小块块。
可是为什么呢？将飞船从天上打下来，然后置之不顾，这又是何必呢？
<<choice [[不懂。|hardtosayoverwarn]]>> | <<choice [[你的访客离你还有多远？|howclosearevisitors]]>>


:: keepwatchingrats
我登上瓦里亚号飞船，就是为了研究这些老鼠和它们的行为……对，我觉得我确！实！应该接着观察。
只是……哦，天啊，看着眼罩为了一点残渣撕扯野小子还真是……
……真是闹心啊。
这都不是一场打斗了。这完全是一场活体解剖。
<<choice [[听起来好吓人啊。|horrifyingrats]]>> | <<choice [[冷静点，深呼吸。|calmandbreathe]]>>


:: calmandbreathe
说到点子上了。我现在几乎不敢呼吸。
我睡醒时总会有绿口水——和老鼠眼睛的颜色一样的绿色，我怎能不注意到——
我顶多也只能猜这是大气中什么物质产生的副作用。
是我呼进去的什么东西。
它们难道是因此变成这样子的吗？
我也会变成这样吗？如果一直呼吸这儿的空气的话？
我真不敢往那方面想。我不知道自己是不是应该呼吸，这物质会不会将我变成……那样。
<<choice [[你必须保持呼吸。|breathedummy]]>> | <<choice [[我想你不会有事的。|sureyouwont]]>>


:: whatdoescomputersay
……哇喔！有点难以置信！
[[herecomesrescueship]]


:: thatcanonlybebad
不，信不信由你，我觉得至少这一次有戏！
[[herecomesrescueship]]


:: pulsetargeting
记得写着脉冲倒计时的那台吗？它刚刚也亮了起来。
屏幕上出现了新的文字。我不喜欢那上面说的：
[星区攻破]
[目标允许]
[正在追踪……]
我完全有理由认为这说的是新来的飞船，那艘可能是来营救我的飞船。
真！不！希！望！它被当成“目标”。
<<choice [[你能改写它吗？|overridetarget]]>> | <<choice [[你能警告那艘船吗？|warnship]]>>


:: howclosearevisitors
<<if $capalive is 1 and $power is "pod">>那四个人形生物（是的，还是四个，没有变多，我现在有点庆幸数量没有变多了）……
<<else>>那五个人形生物（是的，还是五个，没有变多，我现在有点庆幸数量没有变多了）……<<endif>>
大概离山峰只有十分钟的距离了。
我就呆在原地。没必要出去和他们遭遇；无论如何，他们都是冲着我来的。
而且，我既要看着那些老鼠……
又要尽可能不要让那些可能的救援人员被炸出天空……
我手头的事情已经够多了，拜托。
<<choice [[继续研究电脑。|keepatcomputers]]>> | <<choice [[转而观察老鼠。|tendtotherats]]>>


:: whatareratsdoing
这些老鼠的行为……和普通老鼠一样。
真是闹心啊。我是指，除了眼睛的问题，它们看上去就和正常时候一样……
……但它们转过头盯住我的眼神，看上去非常……诡异。像异世界来的。
像外星怪物。
这，还有所有发生的怪事，这山里的电脑、前来的访客、时空的消失。
我需要几分钟时间想想。
[[delay 5m|closinginonpeak]]


:: closinginonpeak
一艘飞船。
哦，我的天。是一艘飞船！一艘飞船出现在这个星区！
我不知道……我是说，要弄清他们是否要到这颗卫星还为时过早，也不知道他们有没有收到我发的SOS。
但他们已出现在了星区图上，显示屏像圣诞树一样亮了起来。我的眼睛多半也这么亮了。
我在房间中小心移动，以和老鼠保持一定距离……
……但我冲过去查了查。去死吧，太空怪鼠。我们有大新闻了！
拜托，拜托了。拜托让他们收到SOS信号吧。拜托让他们把我从这片岩石荒地里解救出去吧。
我只求——
唔，这不太对劲。
<<choice [[怎么了？|whatsnotgood]]>> | <<choice [[是老鼠做了什么吗？|wasittherats]]>>


:: goodthinking
谢了。我想，倒霉了这么久，我总得有个好主意了吧。
好了。老鼠们去吃午餐了，所以我现在要施展我（非常不高超的）的黑客技能了。
[[keepatcomputers]]


:: youeattoo
听着，我很感激你这么关心我……
……但我现在超级不饿。我的肠胃都打成结了。我相当怀疑我还有没有消化功能。
所以，我现在要做的就是控制住想要干呕的冲动，开始施展我（非常不高超的）的黑客技能了。
[[keepatcomputers]]


:: whatthehell
嗯……事情原来是这样子滴，我被人在下巴上狠打了一拳。相当狠。
是救援队的一个队员打的。
因为我在满嘴胡话，说实话，我完！全！不记得我说过话。
我在说什么……在各个时空捕获宿主什么的？
（我一头雾水。）
我现在在救援飞船上……我怕我自己的乌鸦嘴会诅咒它，所以没有说……
……但我觉得会一切顺利。
<<if $capalive is 1 and $power is "pod">>趁还清醒的时候，我跟他们说了阿雅船长在瓦里亚号的医疗舱里面。
他们好心地停下来并带上了阿雅船长，现在这艘船上的医生已经开始了对她的救治。
医生说我做的是当时那种情况下能对船长采取的最佳的措施了……
而且由于我处理得当，阿雅船长很有可能完全（也许很缓慢地）康复。谢天谢地。
所以我可以说我“赢”了吧？<<endif>>
现在坡道门安全地关紧了，全舱扫描显示飞船没有附带任何外星生命体……
……所有那些外星怪物和他们发光的绿眼睛都留在那个卫星上，而我们则在全力升空远离了。
他们打算对这颗该死的石头星进行核打击。
我想这就轮不到我来表态了，但如果能我真能投票……
我非常乐意看见这玩意儿炸成灰。
还有，太好了，他们给我拿来了一些吃的，还有佳得乐，让我吃慢点、喝慢点……
他们还说我需要好好休养一段时间，而且可能一辈子都要去看心理创伤科医生了。
不过好在我还活着。
我挺过来了。
如果没有你，我是不会走到今天的。
所以……谢谢你。衷心谢谢你。
希望未来更加光明。
这里是士官生泰勒，原属瓦里亚号，通话结束。
[连接结束]
[[delay 2s|gameover]]


:: whatsnotgood
唔……另一台电脑上出现了什么东西。
[[pulsetargeting]]


:: warnship
<<silently>><<set $warnship = 1>><<endsilently>>
不知道，不过我可以试试看。就像之前发送SOS信息那样再发一条普通信息。
但愿他们能看得懂英文。话说……我真不知道应该警告他们些什么。
我完全不知道是对准他们的是什么东西。导弹什么的？还是……
不，当然不可能。泰勒，你个蠢货。是脉冲啦。
多半是一股电磁脉冲。用种什么能定向能量的武器，粒子束武器？
我滴神啊。一股强大的能量波，在真空中的破坏力，那可真是超乎你的想象啊。
你完全可以将瓦里亚号那样的飞船打成两截。
<<choice [[一定就是这样的。|mustbeit]]>> | <<choice [[你能改写它吗？|overridetargettwo]]>>


:: overridetargettwo
<<silently>><<set $overridetarget = 1>><<endsilently>>
不知道，我也想啊，不过——不像我钟爱的科幻烂片……
这里没有写着“手动操纵”的红色大按钮。
我对电脑的了解仅限于下载部大片或编个冒险小游戏玩……
但是黑进这个，更改武器系统？
除非密码就是“mima”，否则我是无能为力的。
不过死马当活马医吧。
<<choice [[但愿能成功。|hopeitworkswarnover]]>> | <<choice [[你的访客离你还有多远？|howclosearevisitors]]>>


:: tellmewhatyousee
它们……它们在相互残杀！
我语气一点也不夸张。它们曾是实验组中最可爱最温顺的两只，而且……
哦，天呐。我看不了这个。
这声音，哦，天啊，它们在撕扯对方的血肉。应该会出……
唔。
应该会出很多血才对，但是没有。
一滴血都没有。
到底是怎么回事？！为何从我到山峰来以后，每！件！事！都这么变态？
<<choice [[冷静点，深呼吸。|calmandbreathe]]>> | <<choice [[你得接着观察。|keepwatchingrats]]>>


:: hopeitworkswarnover
希望如此。话说你要是爱祈祷的人的话，那帮我祈祷一下吧。
<<choice [[遵命！|yougotprayerswarnover]]>> | <<choice [[你的访客离你还有多远？|howclosearevisitors]]>>


:: herecomesrescueship
是之前飞进这个星区的那艘飞船吗？它改变了航线！它在减速！
飞船的新航线指向这个卫星！
倒不是说我能从这些老式笨重的显示器和像素化的内容上能读懂多少遥测信息……
……但是要是让我猜的话？它打算降落在这个陨石坑里！来将我从这个诡异的山峰中带出去！
把我救！出！去！
<<choice [[大好消息！|excellentnews]]>> | <<choice [[他们还在被瞄准吗？|stillbeingtargeted]]>>


:: keepatcomputers
那么，好消息是，我已经知道如何入侵广播系统并发送自己的消息了。
坏消息？现在还不清楚那艘飞船会不会回应我。
我不知道他们有没有接收到我最早发的那条SOS信息（他们可能只是碰巧进入这片星区罢了）……
……并且我也没办法知道他们能不能收到我发出的警告。
所以，我所有这些“吐血重写神秘电脑系统”的工作可能都是无用功。
<<choice [[那何苦呢？|sowhybother]]>> | <<choice [[继续破解，以防万一。|doitanyway]]>>


:: crazypants
他们想我呆在控制室。给他们带来更多的飞船，更多的宿主。
这个卫星上的宿主数量不足，所以我们要把他们从别的空间引来。
这个空间里的宿主数量不足，所以我们要把他们从别的时间引来。
为了生存，我们前往未来，回到过去，只为了寻找宿主。
我们从各个时间各个空间拖拽回飞船中，满足我们的需要。
但仍然不够。
我们得将他们从所有时间和所有空间中都找来。
直到所有生物都成为我们的宿主。
直到我们在每一个生物体内生存。
我们是——
嘿！哎哟！
<<choice [[出了什么事？|whatthehell]]>> | <<choice [[你刚刚在唠叨些什么？|babblingabout]]>>


:: closertorats
你要是觉得我会用手将它们分开的话，那你真是疯了。
我不知道你对暴怒饥饿的太空老鼠有什么了解，但我相信肯定不如我多。
作为这里的专家，我决定了：我是不！会！接近它们的。
如果是在瓦里亚号上，我肯定会把这一切都加到笔记里面，这些小家伙出现这种情况绝对不正常——
但我这里没有任何纸笔之类
哦，我滴神哪
我看到的是什么鬼东西
<<choice [[怎么了？！|tellmewhatyousee]]>> | <<choice [[告诉我你看见了什么！|tellmewhatyousee2]]>>

:: tellmewhatyousee2
[[tellmewhatyousee]]

:: awayfromrats
你不用提醒我，我已经退开了。
如果是在瓦里亚号上，我肯定会把这一切都加到笔记里面，这些小家伙出现这种情况绝对不正常——
但我这里没有任何纸笔之类
哦，我滴神哪
我看到的是什么鬼东西
<<choice [[怎么了？！|tellmewhatyousee]]>> | <<choice [[告诉我你看见了什么！|tellmewhatyousee2]]>>


:: mustbeit
对，瓦里亚号，还有鬼知道多少艘船……所有在地图屏幕上闪着绿光的小块块。
可是为什么呢？将飞船从天上打下来，然后置之不顾，这又是何必呢？
<<if $overridetarget is 0>><<choice [[不懂。|hardtosay]]>> | <<choice [[你能改写它吗？|overridetargettwo]]>>
<<elseif $overridetarget is 1>><<choice [[不懂。|hardtosay]]>> | <<choice [[你的访客离你还有多远？|howclosearevisitors]]>><<endif>>


:: hardtosay
真搞不懂。这里的一切都搞不懂。
我要是能活着回家的话，我觉得我一定会跟去威利旺卡的巧克力工厂参观的那些小屁孩一样患上创伤后应激障碍的。
<<choice [[不过虫牙会少一些。|fewercavities]]>> | <<choice [[你能改写脉冲吗？|overridetargettwo]]>>


:: yougotprayerswarnover
非常感谢。但愿祈祷能弥补我昨晚冷得要死的时候爆粗口引起的大不敬。
<<choice [[想骂就骂。|swearalotwarnover]]>> | <<choice [[你的访客离你还有多远？|howclosearevisitors]]>>


:: swearalotwarnover
谢了。要是再有别的乱子的话，我保证一定拿你出气。
[[howclosearevisitors]]


:: stillbeingtargeted
哦，唔，对……我猜是的。不管这个系统上连接着啥武器，我都不知道应该怎么改写。
但愿他们开启了防御系统——但愿他们能！够！抵抗住电磁脉冲或者粒子波之类的攻击。
但愿他们不只
[[disappearancetwo]]


:: excellentnews
对吧？现在我只要想办法绕过那些肚子里装着绿色外星怪物的小白鼠就好……
<<if $capalive is 1 and $power is "pod">>希望那四个来到我门口的人没有恶意……
<<else>>希望那五个来到我门口的人没有恶意……<<endif>>
……然后再想办法
[[disappearancetwo]]


:: babblingabout
我刚刚叽里呱拉了些什么？这真是个好问题。
我也不知道。不过我知！道！我停下来的原因。
[[whatthehell]]


:: doitanyway
对，应该这样。我现在太紧张了，得想法子转移一下。
我最好去积极地鼓捣下电脑，总好过，我不知道……
英勇地玩拇指，还是华丽地挖鼻孔，又或者……
总之，给我一两分钟的时间。
[[delay 3m|warningsent]]


:: sowhybother
何苦呢？我是说，那我还“何苦”费老大劲儿，在这个蛮荒星球上活了这么久啊……
……感觉还是应该积极应对。
我显然不想我的回忆录的最后几章是这么写的：
“然后我把脚翘在一张桌子上，开始英勇地玩我的指头，直到不知怎么的，一切都好起来了！”
总之，给我一两分钟的时间。
[[delay 3m|warningsent]]


:: absolutelynot
我不想……不想登上那艘飞船。
就算……就算我登上去……我也不想让它离开。
它应该是一件藏品。整个收藏从这里开始，但将延伸到永恒。
通往飞船的路上满是眼睛。
[[ignoretheireyes]]



:: ignoretheireyes
我没法忽视那些眼睛。这里没什么可看的了，除了那些眼睛。
从没谁像他们一样能看到我内心的深处。
[[crazypants]]


:: whoaindeed
出现了一条巨大的裂缝，就在我面前，在山峰的入口前。
我转弯想避开它的时候差点把膝盖弄脱臼。
只是其中一个小缝隙里，就有——就冒出了几十只那种怪物。
不行……要死啊！他们完全堵住了我返回的路！
<<choice [[另外找一条路？|anotherwayin]]>> | <<choice [[跑向救援船。|runforrescueship]]>>


:: cantoverride
我要是知道应该怎么做的话，我早就做了！我现在连脉冲倒计时都看不清，数字变得飞快。
我的内心有一部分——不怎么科学的一部分——想试试直接把线拔了，会发生什么事情。
如果那样还行不通的话，那我就只好开始乱打乱砸了。
这多半是馊主意，就我的运气看来，一定什么问题都解决不了，还会搞坏我想要用的广播。
不过告诉你个秘密，不要告诉别人啊。
有时候，就连科学呆子中的极品也会有“乱打乱砸”的冲动，而且还相当强烈呢。
[[ratsgocrazy]]


:: whatisit
我听到吱吱的叫声，是从老鼠吃东西的那个角落里传出来的。
它们一定饿极了。它们将我给食物一扫而光，超级迅速。
然后，野小子和眼罩正在为最后一丝残渣争斗。但，啊啊啊啊！——争夺越来越惨烈！
从普通老鼠吱吱声变成这种，啊啊啊的声音，像是尖！叫！耳朵好痛！
说真的，我的填牙材料都在打颤！
其它两只也开始打了。它们只是围着打转，像是……像是给拳击手加油的观众们。
<<choice [[你需要走近些。|closertorats]]>> | <<choice [[你需要退后。|awayfromrats]]>>


:: fewercavities
哈！那还真是超大灾难中的一丝丝欣慰啊，哥们儿。
现在还有一个重要问题，我能不能在它开火前重写瞄准系统？
很不幸，答案是：
[[overridetargettwo]]


:: anotherwayin
没有别的路了。
我现在唯！一！的选择就是用尽全力冲刺跑向救援船，可是……
[[runforrescueship]]


:: disappearancetwo
[通讯中断]
[时间戳无效]
[正在搜索……]
[正在重新获取信号]
[正在建立连接]
[正在接收消息]
……又来了。不！我最不想要的就是这个！
如果那艘船来营救我怎么办……
……如果他们还没来，这座怪山就带着我诡异地消失了该怎么办？
<<choice [[别在里面呆着了。|dontbeinsideit]]>> | <<choice [[你很快就会再出现的。|youreappearquickly]]>>


:: ratsgocrazy
哦，天啊，访客们就要来了。
我一身冷汗，嗓子里也堵了一团东西，但我确定这跟我吃下肚的难吃的东西没有关系。
我觉得我这辈子都没有这么紧张——
哦，又怎么了？!
<<choice [[发生什么事了？|whatisit]]>> | <<choice [[没出什么事吧？|everythingallright]]>>


:: warningsent
然后……发送！好了，不管那艘飞船上是谁，希望你能接收到我的信息。
希望你正在赶来救我的途中。
不过希望你的防御系统有所准备，因为你已经被当成目标锁定了。
希望你能够抵挡未知的武器。因为我对它的原理和能量一概不知。
<<choice [[愿望真够多的。|lotsofhope]]>> | <<choice [[没法改写吗？|cantoverride]]>>


:: runbackinsidepeak
对……这里到山峰不远。
并不是说呆在那里头一定更安全更合理……
不过，至少我到了那里，会有点时间琢磨一下这周围到底在出什么怪事。
天哪！……到处都是裂隙
咳！
<<choice [[你还好吧？|whoaindeed]]>> | <<choice [[出什么事了？|whoaindeed2]]>>

:: whoaindeed2
[[whoaindeed]]

:: youregonnamakeit
救援队明白了我的意思！他们都戴上了头盔！
就是说，我是这里唯一可能的宿主。
千万只眼睛在盯着我，很难不被吸引。
他们需要我。
他们在召唤我。
他们在尖叫，全部，在尖叫。
但我开始听懂了，尖叫声开始变得很美妙，像乐声。
看着这片绿色之海越久……
我便越确定我该留在这里。
<<choice [[绝对不要！快上飞船。|absolutelynot]]>> | <<choice [[别管这些眼睛。|ignoretheireyes]]>>


:: haulinass
我在跑！我在叫！还……
[[youregonnamakeit]]


:: lotsofhope
是啊，嗯，我奶奶总是说：
“有个法国小妞儿说，‘愿望又不要钱。’”
是的。以上引用我奶奶的原话，她学的是文学。
不过我猜，她读书也不怎么用心。
[[ratsgocrazy]]


:: everythingallright
啊，不……这可不能叫没事儿啊。那些老鼠突然癫狂起来。
[[whatisit]]


:: dontbeinsideit
是啊，说得轻巧。不过电脑就在这里。
我要是一开始没有进来的话，我就不可能知道救援飞船的存在。
我不懂的是……
（我不懂的事情列个单子能有一里长，但我得再加上一项。）
……这明显是间控制室。所以……究竟是谁在控制它呢？
是为谁建的呢？
还有他们在哪儿呢？
……哦，哦，又来了。
<<choice [[什么？|whatsupagain]]>> | <<choice [[你还好吧？|yetagainokay]]>>


:: youreappearquickly
快，是相对的说法。我是说，刚才那次对你来说有多久？
<<choice [[几秒钟而已。|onlyafewseconds]]>> | <<choice [[一点都不久。|onlyafewseconds2]]>>

:: onlyafewseconds2
[[onlyafewseconds]]

:: onlyafewseconds
是啊，只是瞬间。但在我看来？
我几乎消失了十五分钟。
明显我这边的情况不一样。你跟我说很快……
……而且外面的访客们和之前相比只前进了几步。
那么，这地方究竟被什么噩梦笼罩……我在这里，被困得束手无策！
……哦，哦，又来了。
<<choice [[怎么了？|whatsupagain]]>> | <<choice [[你还好吧？|yetagainokay]]>>


:: dyingisselfish
你说“自私”是什么意思？
<<choice [[你得警告其他人。|gottawarnothers]]>> | <<choice [[你看到了其他人没见过的事。|gottawarnothers2]]>>


:: gottawarnothers2
[[gottawarnothers]]


:: donotgiveup
但是放弃最合情合理。
无论我多努力……这恐怖的卫星总是魔高一筹。
我厌倦拼搏了。
破卫星，你想要我的命吗？拿去吧。
<<choice [[闭嘴，快跑！|shutupandrunforit]]>> | <<choice [[现在去死太自私了。|dyingisselfish]]>>


:: morecracks
整个卫星的表面。这个坑底的整块岩石，正在……正在裂开。
岩石以山峰为中心，裂成两半，就这样打开来。小裂隙正逐渐汇成大裂缝。
裂缝中射出光线。
给你三次机会猜，光是什么诡异的颜色！
哦，简直是噩梦。
<<if $capalive is 1 and $power is "pod">>我以为……我以为只要操心八个小绿外星人。
<<else>>我以为……我以为只要操心九个小绿外星人。<<endif>>
不是。
还有成千上万个。
<<choice [[往救援飞船那跑。|runforrescueship]]>> | <<choice [[跑回山峰里面。|runbackinsidepeak]]>>


:: youarewelcome
救生船还很远呢。
绝大部分地面都布满了裂缝。
绝大部分裂缝中都在往外冒外星寄生怪。
这里有上百万，我能看到的可能宿主的有四个，包括我自己。
我？我是那个在喊他们三个赶快戴上头盔的人，免得外星人钻进他们嘴里。
（说这些的时候，我还要努力捂着自己的嘴。有点用……但不怎么样，不得不说。）
救援飞船的垂直起落引擎正在点火，他们知道自己有麻烦了。
他们的麻烦是因为要等我。
我真的办不到了。
我真的办不到了。
<<choice [[你能办到。|youregonnamakeit]]>> | <<choice [[少说话，快跑！|haulinass]]>>


:: runforrescueship
我觉得……我觉得我办不到。这有100码远，而且路面超级不平。
而且这些玩！意！儿！击溃我了。我没有跑路的精神了。
去死吧。
带我一起去死吧。
<<choice [[这样也行？你要放弃？|soyourequitting]]>> | <<choice [[现在不能放弃啊！|donotgiveup]]>>


:: gottawarnothers
……该死。你说得对。
最开始，当我最早开始遇到麻烦，什么都不顺时——
就是我几天前第一次联系你的时候。
如果当时我没有联系你——如果当！时！我直接放弃——一切就简单多了。
结果，你助我不畏艰险一直活下来……
……我真的非常感激……
现在，我要回报你的善举，让别人也活下去。
所以，我想说……谢谢。
<<choice [[不用谢。|youarewelcome]]>> | <<choice [[不客气。现在……快跑！|youarewelcome2]]>>


:: youarewelcome2
[[youarewelcome]]


:: whatsupagain
又是那种声音。老鼠发出的尖叫声。
<<choice [[它们没吃的了吗？|aretheyoutoffood]]>> | <<choice [[它们是在哀悼野小子吗？|mourningwildboy]]>>


:: yetagainokay
对，我……我还好。但我很不安。
那些老鼠又在尖叫了，之前的那种尖叫。
<<choice [[它们没吃的了吗？|aretheyoutoffood]]>> | <<choice [[它们是在哀悼野小子吗？|mourningwildboy]]>>


:: shutupandrunforit
“快跑”？可是我脚下的地面正在崩塌啊？
<<if $hurtankle is 1>>要不是我的脚踝已经痛到死了，这本来也不难。<<endif>>
我实在想不到一个坚持下去的理由。
<<choice [[现在去死太自私了。|dyingisselfish]]>> | <<choice [[你真的那么怂吗？|reallythatweak]]>>


:: soyourequitting
是的，我想……我想就这样吧。
无论我多努力……这恐怖的卫星总是魔高一丈。
我厌倦战斗了。
破卫星，你想要我的命吗？拿去吧。
<<choice [[闭嘴，快跑！|shutupandrunforit]]>> | <<choice [[现在去死太自私了。|dyingisselfish]]>>


:: wontbeanother
……你说得对。太诗意了。
我想我不会在意自己临终的想法跟诗歌有关。
这些……这些生物从我脚边的地下冒出来了。
我知道他们是怎么钻进去的。我记得那味道。
我只是……在想诗歌，而不是疼痛。
[[submitlikeapoet]]



:: restrightthere
太神奇了，我多么强烈地想要将身体蜷回胎儿时的姿势！那样一定很舒适。
就躺在这里，躺在这个残酷星球的干燥、苍白、开裂的地面上哭泣，让哭声渗入裂缝。
裂缝……比一分钟前多了。
多了许多。
哦，
哦，糟了。
<<choice [[出什么事了？|morecracks]]>> | <<choice [[“裂缝更多了”？|morecracks2]]>>


:: morecracks2
[[morecracks]]


:: tryitandsee
嘿……好了……他们对此不太高兴……不过没拖住我。
我感觉像是在努力避开一群游客，想好好看看蒙娜丽莎一样。
好啦，狂冲过走廊。等一下再通话。
[[delay 2m|emergefromhall]]


:: aretheyoutoffood
是的。他们把我给的食物都吃光了，渣都不剩。
但我没有办法每过几分钟就投一次食物，以让他们保持安静。
我只是……现在太多事情要忙了。老鼠、食物、飞船、电脑，还有……
……嗯，不管怎样，我刚刚决定了接下来先处理好哪件事。不管结果好坏。
（我这是在骗谁呢？结果从来没好过。）
<<choice [[哪件事？|whichplate]]>> | <<choice [[别怕，保持冷静。|nofreakingout]]>>


:: mourningwildboy
要是他们真那么有感情有组织就好了。
说实话，我对它们还没有开始吃野小子都够惊讶了，更不会认为它们会举办一场烛光守夜。
我只是……现在太多事情要忙了。老鼠、食物、飞船、电脑，还有……
……嗯，不管怎样，我刚刚决定了接下来先处理好哪件事。不管结果好坏。
（我这是在骗谁呢？结果从来没好过。）
<<choice [[哪件事？|whichplate]]>> | <<choice [[别怕，冷静呼吸。|nofreakingout]]>>


:: emergefromhall
救援飞船着陆在大概一个橄榄球场的距离开外。（100码，我虽然不擅长运动，但这个还是知道的。）
那艘飞船小小的——可能和之前的卡拉维尔型小飞船差不多大，甚至还要再小些。
但是最不同的在于，不像这个恐怖、残暴的星球上的其他飞船……
……它完好无损。
哈哈哈哈。
这……我想笑，又想哭，我觉得自己要精神错乱了。
我，啊，跪倒在了地上，因为腿不听使唤了，这两条废物。
我止不住颤抖，我太高兴了。
<<choice [[快站起来。|getbackup]]>> | <<choice [[休息一下吧，你应得的。|restrightthere]]>>


:: reallythatweak
真的！我一直保持坚强，只是……我一点力气都没了，好吧？
难道此刻我怂一下都不行？就这一刻？
<<choice [[不会有下一刻了。|wontbeanother]]>> | <<choice [[要是你真想的话。|reallywhatyouwant]]>>


:: submitlikeapoet
哦
这里
没有
诗
意
[[submitanddie]]


:: getbackup
对，我知道，我该，我该站起来。
我没有时间在每次遇到有人穿越半个星系来救我时都瘫倒在地，痛哭流涕。
我得稳住。
哈哈！
[[restrightthere]]


:: bepreparedtofight
我倒是“准备好”了的——我的意思是，我也不知道是不是算准备好了——不过没有发生战斗。
他们没有特别……高兴……不过没拖住我。
我感觉像是在努力避开一群游客，想好好看看蒙娜丽莎一样。
好啦，狂冲过走廊。等一下再通话。
[[delay 2m|emergefromhall]]


:: gettherefirst
行，我要试试。
现在的问题是，我能顺利穿过那满是僵尸的走廊吗？
如果我们都向一个方向走，他们会拦我吗？
<<choice [[试试看。|tryitandsee]]>> | <<choice [[准备好战斗。|bepreparedtofight]]>>


:: isthereaproblem
有个问题。确切说是一堆小问题，加在一起成了一个大问题。
[[heresthesituation]]


:: whichplate
<<if $capalive is 1 and $power is "pod">>近距警报器显示接近物体从四个降到三个。
<<else>>近距警报器显示接近物体从五个降到四个。<<endif>>
二、一、零。
因为他们不在附近了，因为他们已经到了。
他们进入了山峰。他们进入了我走过的那条又长又黑的走廊。
就在那条走廊中，那些老鼠擦过了我的腿，那时候我还不知道它们就是我！养！的老鼠。当然那是它们四个还全活着的时候的事儿。
（真……真有哪只是活的吗？身体里塞着些绿怪物到处转？）
还是那条走廊，要是我已经探索过旁边的通道就好了。
（那样的话，我还能躲在黑暗处，悄悄计划逃走。）
还是那条走廊，横在我与自由之间。
（我也真够怂的，怎么会觉得这个疯狂的寸草不生的的卫星表面居然是“自由”的象征？）
<<if $glowrods gte 1>>嘿，唔，我只是有个奇怪的想法。能告诉你吗？
<<choice [[当然，说吧。|surelayitonme]]>> | <<choice [[你最好别说。|ratheryoudidnt]]>>
<<else>>我现在除了在这里干等，还能做什么？
<<choice [[去大厅里会一会他们。|godownhall]]>> | <<choice [[放松点。|casualpose]]>><<endif>>


:: nofreakingout
我在呼吸，我在呼吸。但是在呼吸的同时，我还很怕。
[[whichplate]]


:: submitanddie
[宇航服传感器显示严重心跳过速]
[检测到局部缺血]
[检测到心肌梗塞]
[[connectionlost]]


:: heresthesituation
<<if $capalive is 1 and $power is "pod">>这个房间里面有八个外星生物。
<<else>>这个房间里面有九个外星生物。<<endif>>
“这个房间”是一个控制中心，似乎其主要目的是把飞船从宇宙中拖下来……
然后使用那些飞船上的船员尸体作为这里的寄生生命的宿主。
我不知道救援队是否有武装并能击败这些生物……
……吐槽，我都不知道常规的电击棒对这些生物有没有用……
船员们通常也就顶多配上电击棒了。
这些外星生物也注意到那艘救援船了。
好消息：他们走了，留我一个人在电脑前。
坏消息：他们冲着走廊去了。
麻烦了！
救援队在找到我之前就会被他们拦截住的。
<<choice [[你得赶在他们之前才行。|gettherefirst]]>> | <<choice [[留下，守在电脑旁。|stayguardcomputer]]>>


:: submitlikeawuss
好
好疼
哦，天
真的好疼
[[submitanddie]]


:: reallywhatyouwant
……是的。我真的想。
我只是太累了。
这些生物从我脚边的地下冒出来了。
我知道他们是怎么钻进体内的。我记得那味道。
我知道这是一种奢望……
……但我真心希望不要疼。
[[submitlikeawuss]]


:: whatsiren
这是……
哦，偶滴神啊，终于来了！
是救援船！在着陆了！
我的屏幕全亮起来了！为了所有时空里最！重！大！的消息而亮起来了！
我终于要离开这里了！
唔……除了……
<<choice [[“除了”什么？|exceptwhat]]>> | <<choice [[有什么问题？|isthereaproblem]]>>


:: goodsiren
比外星生物的尖叫更响，所以我打算说是“好报”！
[[whatsiren]]


:: glowrodgoodidea
我希望这被当成友好姿态，表示和平。
这能让他们知道我就在这里，而我正在想办法让他们的行程轻松些。
如果他们是其他坠毁飞船的落难船员的话……
那他们在这个时候看到我的示好，那一定很欣慰吧。
要说我会怎么想吗？要是有人扔给我一只荧光棒，我也多半会感激涕零的。
好啦，我要扔一根。
<<choice [[扔出去！|throwarod]]>> | <<choice [[你能看到什么东西吗？|canyousee]]>>


:: yousaidno
这个卫星。这个恐怖狂怒的卫星。
它想折磨我。
它想折磨死我。
我可以在警报器的显示器中看到，整个卫星的表面，还有这个陨石坑中的整块岩石，
……都在裂开。
以山峰为中心，岩石正在……开裂。就这样打开来。小裂隙正逐渐汇成大裂缝。
裂缝中射出光线。
给你三次机会猜，那些光有什么诡异的颜色！
哦，简直跟噩梦一样。
<<if $capalive is 1 and $power is "pod">>我想……我想还有八个小绿外星生物需要注意。
<<else>>我想……我想还有九个小绿外星生物需要注意。<<endif>>
不对。
还有上千上万个。
<<choice [[现在跑路会不会太晚？|toolatetorun]]>> | <<choice [[如果你留下来会怎样？|whatifyoustay]]>>


:: canyousee
<<silently>><<set $glowrods = $glowrods - 1>><<endsilently>>
哦，我借助荧光棒的微光能看到什么吗？
<<if $capalive is 1 and $power is "pod">>不太行。看不到太多。那四个人……还要几分钟才能到。
<<else>>不太行。看不到太多。那五个人……还要几分钟才能到。<<endif>>
（我大概说过吧，我没有运动神经。不擅长投球。换句话说，我投荧光棒的本事也很烂。）
不过，反正这几分钟除了等也没什么好做的……
哈。
野小子体内的那个小外星人发生了一些（更）诡异的事情。
<<choice [[你什么意思？|weirdratthings]]>> | <<choice [[离它远点。|staythehellaway]]>>


:: keepguardingcomputers
我就呆在这里了。他们是为我而来的。会没事的。会没……
那……那是什么？
不！
不！
<<choice [[出了什么事？|yousaidno]]>> | <<choice [[哪里不对劲了？|whatwentwrong]]>>


:: lookatwhatexactly
我可以从警报器的显示屏上看到救援人员登陆了！
我滴神啊，他们真的来了！
那艘船小小的，可能和之前那艘卡拉维尔型小飞船差不多大，甚至还要再小些。
但是大的不同点在于，和这个恐怖、残暴的星球上的其它飞船不同的是……
……它完好无损。
他们派了三名船员下来。这可能已经是大部分船员了，我猜，船上大概还有一名，顶多两名留守吧。
就像我一样，他们也发现这里的空气可供呼吸，将头盔摘了下来。谁不想图省事儿呢？
哦，偶滴神啊，我得救了！
<<choice [[你要去见他们吗？|shouldyoumeetthem]]>> | <<choice [[继续守着电脑。|keepguardingcomputers]]>>


:: stayguardcomputer
没错儿，对吧？毕竟我一路从瓦里亚号的残骸走到了这个诡异的控制中心。
我的救援人员走几百尺来救我也没什么关系啊。
我现在真心希望，他们决定开展救援行动时，做好了充分的预防措施，
就算没带武器的话，至少得带了光源和警惕心吧……
这样就不会在黑暗中被瓦里亚号来的僵尸攻击了。
嘿！……快看那个！
<<choice [[快看哪个？|lookatwhatexactly]]>> | <<choice [[你在看什么？|lookatwhatexactly2]]>>


:: lookatwhatexactly2
[[lookatwhatexactly]] 


:: exceptwhat
除！了！这是一个他们完全意料不到也根本防备不到的情况。
[[heresthesituation]]


:: surelayitonme
<<if $glowrods is 1>><<set $plural = "">><<else>><<set $plural = "">><<endif>>
好吧，我还剩下<<$glowrods>>根荧光棒<<$plural>>。
我刚刚向走廊那边瞥了一眼，那些家伙要是真带了什么灯的话，那也一定没有在用。
要在几乎伸手不见五指的黑暗中跋涉五分钟，只有尽头处的控制室有点微弱的灯光在指引他们。
我要不要折亮一支荧光棒朝他们丢过去呢？
<<choice [[要，好主意！|glowrodgoodidea]]>> | <<choice [[不，当然不要了。|glowrodbadidea]]>>


:: godownhall
什么，去半路……会会他们？
<<if $capalive is 1 and $power is "pod">>在超级黑超级封闭的走廊里面？而且还在一挑四的情况下？
<<else>>在超级黑超级封闭的走廊里面？而且还在一挑五的情况下？<<endif>>
我能不能不去啊？要知道从战术角度看，这根本就不是个好主意。
<<choice [[赌一把，有时候就需要闯一闯。|youonlyliveonce]]>> | <<choice [[好啦，那就原地待着吧。|stayincontrolroom]]>>


:: maybeverybrave
如果他们在这里再呆久点的话，那种蛮夫之勇会把他们害死的。
那种蛮夫之勇……
……该死，我希望我发掘出这种蛮勇。
[[impossibleodds]]


:: toolatetorun
确实。对我来说太晚了，我永远赶不到救援飞船了。
希望当他们发现那里一片混乱，又没看到我在空地上……
……他们会明白过来，不会呆在危险里，等着我出现。
如果他们聪明的话，现在就应该已经走了。
但是他们没有，所以……我猜他们不够聪明。
天哪，他们真的在登陆吗？他们不会打算杀到山峰吧？
对，他们真！的！不够聪明。
<<choice [[或者他们只是十分英勇。|maybeverybrave]]>> | <<choice [[他们一定真！的！想要救你。|reallysaveyou]]>>


:: casualpose
“放松”？
<<if $capalive is 1 and $power is "pod">>就像007电影里的坏人那样，恭候那四个人前来我藏在山肚子里的超级秘密巢穴？
<<else>>就像007电影里的坏人那样，恭候那五个人前来我藏在山肚子里的超级秘密巢穴？<<endif>>
（好吧，实际上……）
嗯，据我所知，这是他们的绝密老窝，并且我刚刚毁了他们的盛宴。
要是在这个剧本里我是007呢？那可是头一遭啊。
我不懂赌牌，喝马丁尼酒会犯恶心。我当特工估计很怂。
但是话说回来，邦德只是在地球上混。而我却在星际之间！
<<choice [[价值百万美元的点子！|milliondollars]]>> | <<choice [[别太臭美了。|ratheryoudidnt]]>>


:: guardyourthroat
我在努力！相信我！我可不想那玩意儿进到我身体里面！
无论控制了我这些老朋友的是什么，都把他们变强大了。
他们个头没有变，但是感觉……他们全身的肌肉都在使劲儿……效率很高的样子！
我打不过。我只能不停地踢、甩，尽量让他们离我远一些！
等等……警报响了！
<<choice [[什么意思？|whatsiren]]>> | <<choice [[好报还是坏报？|goodsiren]]>>


:: guardthecomputer
我在尽可能地把我的身体隔在他们和电脑之间。
我已经扯掉了键盘……
但我现在担心的是，他们会徒手摧毁整个系统！
无论控制了我这些老朋友的是什么，都把他们变强大了。
他们个头没有变，但是感觉……全身的肌肉都在使劲儿……效率很高的样子！
我打不过。我只能不停地踢、甩，尽量让他们离我远一些！
等等……警报响了！
<<choice [[什么意思？|whatsiren]]>> | <<choice [[好报还是坏报？|goodsiren]]>>


:: getbackup1
对，我知道，我该，我该站起来。
我没有时间在每次有人穿越半个星系来救我时都瘫倒在地，痛哭流涕。
我得稳住。
哈哈！
[[restrightthere1]]


:: restrightthere1
太神奇了，我多么强烈地想要将身体蜷回胎儿时的姿势！那样一定很舒适。
就躺在这里，躺在这个残酷星球的干燥、苍白、开裂的地面上哭泣，让哭声渗入裂缝。
裂缝……比一分钟前多了。
多了许多。
哦。
哦，糟了。
<<choice [[出什么事了？|morecracks]]>> | <<choice [[“裂缝更多了”？|morecracks2]]>>


:: whatwentwrong
全都不对劲。
本来以为好转了，谁知道情势急转直下。
[[yousaidno]]


:: throwarod
<<silently>><<set $glowrods = $glowrods - 1>><<endsilently>>
哎……我在这几天可能跟你提到过，我没有运动神经。
不擅长投球。确切的说，投什么都不擅长。
换句话说，我扔荧光棒的水平也很矬。
但重要的是心意，心意，对不对？
反正这几分钟除了等以外也没什么好做的……
……嗯。
野小子体内的那个小外星人发生了一些（更）诡异的事情。
<<choice [[你什么意思？|weirdratthings]]>> | <<choice [[离它远点。|staythehellaway]]>>


:: wrongaboutrod
是吗？行，那我就扔一根吧。
<<choice [[扔出去！|throwarod]]>> | <<choice [[你能看到什么东西吗？|canyousee]]>>


:: glowrodbadidea
说得对，我明白你的意思了。
在我了解这些人或他们的意图前，没必要浪费我有限的资源。
我一直努力保持乐观。我希望别人能将这看作示好，一个和平的信号。
这能让他们知道我就在这里，而我正在想办法让他们的行程轻松些。
如果他们是其他坠毁飞船的落难船员的话……
那他们在这个时候看到我的示好，那一定很欣慰吧。
要说我会怎么想吗？要是有人扔给我一只荧光棒，我也多半会感激涕零的。
但是先不扔也许是明智之举。
<<choice [[我说错了，你扔一根吧。|wrongaboutrod]]>> | <<choice [[等一下，放松。|casualpose]]>>


:: impossibleodds
听着，那不可能。
不是那种“电影结尾主角大显身手可以克服”的不可能……
……而是真的不！可！能！
我不可能从山峰这里跑到救援飞船那里，我一定会被那些生物攻击的。
同理，那些救援人员也不可能从救援飞船那里跑来救我，然后我们再一起跑回他们的飞船。
那些东西，那些外星寄生怪，一定会抓住我们，把我们当作他们的……傀儡的。
我要是没猜错的话，他们一定是寄生在人类身体里，然后建造了这个控制中心……
用来从空间中——天哪，还有时间中——拉取飞船。
只要有飞船坠毁，他们就会寄生在新宿主的身体中。
不过到目前为止他们所能抓到的飞船，就是坠毁在这个卫星上的这些了。
话说，如果这种外星生物可以控制宿主，然后控制这艘救援船——能飞的飞船的话，会怎么样？
那他们就可以离开这个卫星，带领着成千上万的寄生态生命前往其他星球？
<<choice [[那后果简直不堪设想。|spreadunchecked]]>> | <<choice [[宇宙种族灭绝。|spreadunchecked2]]>>


:: spreadunchecked2
[[spreadunchecked]]


:: shouldyoumeetthem
我是不是应该庆祝一下？
没准儿他们会带着香槟呢。我自己没带，想想还有点小尴尬。
等我们回去之后再请他们喝香槟吧。
好吧，是的。那现在我就空着手来欢迎救我的人吧。
现在的问题是，我能顺利穿过那满是僵尸的走廊吗？
如果我们都向一个方向走，他们会阻碍我吗？
<<choice [[试试看。|tryitandsee1]]>> | <<choice [[准备好战斗。|bepreparedtofight1]]>>



:: youonlyliveonce
说得好，此时不闯更待何时。
我只是真心希望能闯过这次，活到下次闯的时候。
不过……好吧。前有尖叫的恐怖绿色外星生物在角落吞食老鼠的尸体……
<<if $capalive is 1 and $power is "pod">>后面四个人形生物（真实身份尚未可知）在黑漆漆的走廊中慢慢接近我……
<<else>>后面五个人形生物（真实身份尚未可知）在黑漆漆的走廊中慢慢接近我……<<endif>>
我觉得无论我下一步打算怎么做，我大概都有一半生一半死的机会。
那好吧。我这就进走廊了。
祝我好运。
拜托了？
<<choice [[祝你好运。|goodluckinhallway]]>> | <<choice [[你不会有事的。|youllbefine]]>>


:: milliondollars
这样写肯定很精彩。帮我记下来吧！
“我从逃生舱中钻出来，如马蒂尼一般被使劲晃得发抖。大多数人甚至不知道阿斯顿∙马丁也造宇宙飞船。
“同样的，……大多数人也不是邦德。泰勒∙邦德。”
然后萨克斯风奏响，杜兰杜兰的音乐诉说着我是怎样一个混蛋。
切到字幕卡片：星际肇事者。
（事实是，我紧张的时候，会喋喋不休，随便乱引用。哎呀……不好意思。）
不过，反正这几分钟除了等以外也没什么好做的……
……唔。
野小子体内的那个小外星人发生了一些（更）诡异的事情。
<<choice [[什么意思？|weirdratthings]]>> | <<choice [[离它远点。|staythehellaway]]>>


:: whatifyoustay
如果我留下会怎样？
哦，天啊……要是留下的话，我就死定了。
要是跑的话，我也死定了。不论做什么，我都死定了。
不过……跑的话也不一定死得更惨。
[[impossibleodds]]


:: ratheryoudidnt
哦，天啊。我又在胡言乱语吗？我很抱歉，我不是故意的，我一紧张就爱动嘴皮子。
还有，要知道，我现在的紧张程度可能是我这辈子的紧张总数的无数倍。
（我真不知道“无数”在公制术语里是什么意思，或许是“三”的意思吧。）
吐槽。我居然在喋喋不休说自己不想喋喋不休。
我现在立刻闭嘴。我会在这里坐等接下来的五分钟度过……
直到那些人穿过走廊，突然出现在我这间屋子里……
然后我的衣服突然湿透了，由于激动或者恐惧又或者两者都是。
对不起。对不起。我要闭嘴了……
……从现在起。
唔，嘿，我知道我得闭嘴，但是，唔，我刚才注意到了……
……野小子体内的那个小外星人发生了一些（更）诡异的事情。
<<choice [[你什么意思？|weirdratthings]]>> | <<choice [[离它远点。|staythehellaway]]>>


:: whatsgoingonnow
我刚才有足够的时间把脉冲武器精确地对准这座山峰。
确认过地形图上已经瞄准好了。纬度、经度，可以发射了。
但是瓦里亚号船员……他们好像明白了我的计划，而且不喜欢这个计划！
我不知道他们怎么知道的，他们看起来没有感觉，他们不说话，他们只是……变态地……尖！叫！。
而且全都朝我过来了！
我被困住了。如果我离开电脑，我恐怕他们会关闭倒计时。
但是如果我不走，恐怕他们会把我撕成碎片，或者把那该死的绿玩意儿塞进我的喉咙！
<<choice [[保护电脑。|guardthecomputer]]>> | <<choice [[保护喉咙。|guardyourthroat]]>>


:: timejustpassed
我不知道！对你来说只是几秒钟，我猜？对我来说像是八分十分钟。
好消息是：我刚才有足够的时间把脉冲武器精确地对准这座山峰。
确认过地形图上已经瞄准好了。纬度、经度，可以发射了。
坏消息是：我那些从瓦里亚号上来的小伙伴们不喜欢这个计划！
我不知道他们怎么知道的，他们看起来没有感觉，他们不说话，他们只是……变态地……尖！叫！。
而且全都朝我过来了！
我被困住了。如果我离开电脑，我恐怕他们会关闭倒计时。
但是如果我不走，恐怕他们会把我撕成碎片，或者把那该死的绿玩意儿塞进我的喉咙！
<<choice [[保护电脑。|guardthecomputer]]>> | <<choice [[保护喉咙。|guardyourthroat]]>>


:: bepreparedtofight1
我倒是“准备好”了的——我的意思是，我也不知道是不是算准备好了——不过没有发生战斗。
他们没有特别……高兴……不过没拖住我。
我感觉像是在努力避开一群游客，想好好看看蒙娜丽莎一样。
好啦，狂冲过走廊。等一下再通话。
[[delay 2m|emergefromhall1]]


:: reallysaveyou
好吧，那他们真是太高尚了，但他们要是再在这里多停留的话，那种高尚会害死他们的。
我……我肯定是要死于这种高尚了。
[[impossibleodds]]


:: spreadunchecked
那么，我能做的事情是：
像之前那样黑进广播系统，向外面的救援飞船发送消息。
让他们火速撤离，离开这个魔窟。
<<if $capalive is 1 and $power is "pod">>我会给他们瓦里亚号的坐标，这样他们就可以去接躺在医疗舱内的阿雅船长。
药物见效，运气又好的话，她会活下来——瓦里亚号坠毁事件——唯一的幸存者。<<endif>>
然后呢？等我确定救援飞船安全撤离之后？
我就会将这座山峰的坐标作为目标输入武器系统。
然后归零整个脉冲倒计时器……
……将这座变态的山炸个粉碎。
<<choice [[那么……你呢？|butwhataboutyou]]>> | <<choice [[你能及时逃脱吗？|escapeintime]]>>


:: youllbefine
这话听起来还真是自信满满地……毕竟你离这吓死人的鬼窟那么老远呢！
嘿，唔，听着……虽然这听上去像是我在故意拖延时间——我保证这个原因只占两成——
……但野小子体内的那个小外星人发生了一些（更）诡异的事情。
<<choice [[什么意思？|weirdratthings]]>> | <<choice [[离它远点。|staythehellaway]]>>


:: holdonweapontwo
不，不，已经没有时间可以等了。
在这边，时间真的很金贵。它现在开始以我不太能理解的方式变动了。
我和你之间的连接可能会断开一秒，据我观察，你那边的一秒可能等于我这边的半个钟头左右。
谁知道这些瓦里亚僵尸在那段时间会做些什么呢？
不能等了，我得赶快行动。你支持我吗？
<<choice [[我还有别的选择吗？|doihaveachoice]]>> | <<choice [[跟我说说你的计划。|tellmeyourplan]]>>


:: reallyreprogram
我之前能够中断信号，是吧？
<<if $overridetarget is 1>>我已经黑进电脑，让瞄准系统不再将救援飞船作为……目标。<<endif>>
[[tellmeyourplan]]


:: goodluckinhallway
谢了。真的。我还从没要人为我跑半条走廊而摇旗呐喊过呢。
这一定是上了年纪的人，或者烂醉如泥的人才做的事。
（我真心希望自己能活下来，这样未来我两样都可以做。）
嘿，唔，听着……虽然这听上去像是我在故意拖延时间——我保证这个原因只占两成——
……但野小子体内的那个小外星人发生了一些（更）诡异的事情。
<<choice [[什么意思？|weirdratthings]]>> | <<choice [[离它远点。|staythehellaway]]>>


:: stayincontrolroom
好吧，要是你没意见，我就这么干了。
这里已经够怪够乱了，没必要笨头笨脑地再去多惹点。
不过，反正这几分钟除了等也没什么好做的……
……唔。
野小子体内的那个小外星人发生了一些（更）诡异的事情。
<<choice [[什么意思？|weirdratthings]]>> | <<choice [[离它远点。|staythehellaway]]>>


:: emergefromhall1
救援飞船着陆在大概一个橄榄球场的距离开外。（100码。我虽然不擅长运动，但这个还是知道的。）
舷梯已经放下来了。救援队应该能看见我。
哈哈哈哈。
这……我想笑，又想哭，我觉得自己要精神错乱了。
我，啊，跪倒在了地上，因为腿不听使唤了，这两条废物。
我止不住颤抖，我太高兴了。
<<choice [[快站起来。|getbackup1]]>> | <<choice [[休息一下吧，你应得的。|restrightthere1]]>>


:: wishyouskill
对，这样说更好。
你懂的，我真！的！不喜欢瓦里亚号全体船员同时转身盯着我的那种感觉。
令人毛骨悚然。
算了，那……
[[disappearagain]]


:: disappearagain
[通讯中断]
[时间戳无效]
[正在搜索……]
[正在重新获取信号]
[正在建立连接]
[正在接收消息]
……我消失的那段时间发生了很多可怕的事情！
千不该万不该在这个时候发生啊！
<<choice [[发生了什么？|whatsgoingonnow]]>> | <<choice [[过去多久了？|timejustpassed]]>>


:: tellmeyourplan
先叙述一下情况：
<<if $capalive is 1 and $power is "pod">>我这里有四名死而不僵的队友，三只死而不僵的老鼠，还有个我完全不知道是什么的外星绿怪物……
<<else>>我这里有五名死而不僵的队友，三只死而不僵的老鼠，还有个我完全不知道是什么的外星绿怪物……<<endif>>
全都用冰冷恐怖的绿眼睛盯着我。
眼神非常空洞！
而且全都像是希望我！的！眼睛也变成那样。
所以这么看来，在整个宇宙——字面意义的——我真没什么好损失的了。
我要是失败了，也不会变得更惨。
而且，我要是成功了……
……他们就不会再这样鬼叫了。
祝我好运吧。
<<choice [[当然，祝你好运。|wishyouluck]]>> | <<choice [[我要祝你道高一丈。|wishyouskill]]>>


:: tryitandsee1
嘿……好了……他们对此不太高兴……不过倒没拖住我。
我感觉像是在努力避开一群游客，想好好看看蒙娜丽莎一样。
好啦，狂冲过走廊。等一下再通话。
[[delay 2m|emergefromhall1]]


:: escapeintime
无处可逃。最终没法逃开。
不让那些外星生物爬进我的嘴里，开始控制我？
我避不开了。
你知道吗？我无所谓了。
[[wrappingthingsup]]



:: butwhataboutyou
那么我呢？
[[wrappingthingsup]]



:: whatweapontwo
这个山峰，这间控制室，是个武器控制中心，对吧？那个脉！冲！倒！计！时！的意义也就在于此。
我不确定这究竟是什么武器——我猜是电磁脉冲、粒子束之类的威！力！巨！大！的武器——
但很有可能，要是这个武器强大到能将一整艘星际飞船炸出天际……
……那么我需要做的就是想办法重新编程来对准这！里！的坐标发射了……
……并且它的威力足够把这座山的每一寸土地都烤得嘎嘣脆。
当然还包括那些尖叫的躲在瓦里亚号船员体内到处游荡的绿怪物了！
<<choice [[先别用武器……|holdonweapontwo]]>> | <<choice [[你真的能对它重新编程吗？|reallyreprogram]]>>


:: staythehellaway
听着，我完全不想靠近它。
相信我，要是有条件的话，我宁愿在房间的另一头架个有变焦镜头的摄像机进行拍摄……
……但我的宇航服上的相机已经罢工了。真是历史的损失。
所以我觉得我应该跟你描述一下玩意儿，尽可能地描述。不过由于我被震惊得有点失语了，不见得能形容得多好。
这个玩意儿，这种生物……在刚刚过去的几分钟内，将自身的部件拉回了自己体内。
要是你没明白我的说的：我说的是真滴！真滴这样阴森恶心。
我一直假装视而不见，因为我每次朝那里看都会觉得恶心。
<<choice [[你必须详细描述一下。|havetodescribe]]>> | <<choice [[有必要的话就休息一下。|takeaminute]]>>


:: weirdratthings
我的意思是……我真的不知道该如何向你描述这种生命体。
（现实是这件宇航服上的该死的相机罢了工，简直是历史的损失。）
这个玩意儿，这种生物……在刚刚过去的几分钟内，将自身的部件拉回了自己体内。
要是你没明白我的说的：我说的是真滴！真滴这样阴森恶心。
我一直假装视而不见，因为我每次朝那里看都会觉得恶心。
<<choice [[你必须详细描述一下。|havetodescribe]]>> | <<choice [[有必要的话就休息一下。|takeaminute]]>>


:: doihaveachoice
当！然！你当然有别的选择了。
要是你真想的话，那我就给你一个劝我别做的机会。
但事情只可能这样发展了，对不对？
无与伦比的蠢并带点点英雄气息地——或者很机智很英勇地——拯救世界？
<<choice [[那么好吧。|tellmeyourplan]]>> | <<choice [[你真的能对它重新编程吗？|reallyreprogram]]>>


:: wishyouluck
谢了。你懂的，我真！的！不喜欢瓦里亚号全体船员同时转身盯着我的那种感觉。
令人毛骨悚然。
算了，那……
[[disappearagain]]


:: mustbeanillusion1
我……我也不懂，但我不觉得是幻觉。
昨天我一直走不到山峰时，我还以为是幻觉。
但是参照之前宇航服的奇怪的时间读数，我觉得这不像是令人困惑的幻觉……
而更像是令人困惑的现实。
<<choice [[离那个外星生物远点。|clearoftheet1]]>> | <<choice [[其他的老鼠在做什么？|otherratsdoing1]]>>


:: otherratsdoing1
它们的行为看起来就像是普通的老鼠。虽然饥饿，但举止正常。
它们就像往常一样乱爬。
它们的步态，或者嗅探行为——
是的，嗅探行为是一种人类用在小白鼠身上的一种术语，我知道听起来很怪异，不过现在不是纠结这个的时候——
或者任何行为，都不会让人觉得它们没有全权控制自己。
但是考虑到那个寄生怪对野小子的肌肉和神经进行的结合……
……我很确！定！是那些外星生物在控制我的老鼠到处转。
[[aretheratsdead1]]


:: whatisitdoingnow1
就像我说的那样，它好像已经把所有触手和毛细血管拉回了自己身体。
现在这个有点光滑但几乎没什么特征的生物，就正处于原本是野小子的一团毛发和血肉之中。
但有些奇怪的是……这只外星生物看起来比老鼠要大！
感觉这玩意儿怎么也不可能装进野小子的皮里啊。
<<choice [[一定是你出现的幻觉吧。|mustbeanillusion1]]>> | <<choice [[离它远点。|clearoftheet1]]>>


:: wrappingthingsup
我刚刚过了三天地狱般的生活。
我现在只需闭上双眼，按下一个按钮，就能防止别人再重蹈我的覆辙。
考虑到这个卫星给我提供的所有选项……
这个，我觉得，这是我能想到的最好的死法了。
……好了。救援飞船升空了。
那些愤怒的，没找到宿主的外星生物转而向这座山峰而来。
这里会变得很拥挤。
他们在尖叫，他们都在尖叫。
很好。
我就要他们尖叫。
真希望能重创它们。
听着，在我走之前，我只想说……
如果没有你，我是万万不可能挺过这一切的。
所以，谢谢你。这是我的心里话。
希望未来更加光明。
这里是士官生泰勒，原属瓦里亚号，通话结束。
[连接结束]
[[delay 2s|gameover]]


:: aretheratsdead1
是的，我知道这听起来有点变……
但是我认为用来形容它们的最准确的词就是“死而不僵”。
或者这在逻辑上有点跳跃。或者尽管我管这叫僵尸，但其实本质上完全不同。
我的意思是，不管它们体内是什么……都在让它们运动，保持生命进程。
但它们还是之前的老鼠吗？还有之前的性格吗？像在瓦里亚号坠毁之前那样吗？
像在它们的眼睛变成绿色之前那样吗？
我真的不这样认为，一点也不。
因为——
哦。
哦，不。
<<choice [[怎么了？|whatthistime]]>> | <<choice [[老鼠又出了什么事？|morewithrats]]>>


:: cantbeyourplan
不是的，好吧，那不！是！我的计划。至少不是我的全！部！计划。
但听我说。
[[whatweapontwo]]


:: randombashing
嗯，不要瞎打。
好吧，不要完！全！瞎打，听我说完。
[[whatweapontwo]]


:: holdonweapon
不，不，已经没有时间可以等了。
在这边，时间真的很金贵。它现在开始以我不太能理解的方式变动了。
我和你之间的连接可能会断开一秒，据我观察，你那边的一秒可能等于我这边的半个钟头左右。
谁知道这些瓦里亚僵尸在那段时间会做些什么呢？
不能等了，我得赶快行动。你支持我吗？
<<choice [[我还有别的选择吗？|doihaveachoice]]>> | <<choice [[跟我说说你的计划。|tellmeyourplan]]>>


:: takeaminute
谢谢，但……我都不知道我是不是还能有一分钟，你懂吗？
我想要趁我的访客还没到，赶紧都说了，因为之后又是一团新的麻烦让我伤脑筋。
另外，我知道我接下来要告诉你的这件事听起来有多疯狂。
但你这几天一直陪着我来着，我相信你应该清楚疯狂的是这个卫星，而不是我。
<<if $capalive is 1 and $power is "pod">>让我跟那四个人去从头到尾解释一遍？我觉得我现在可没那么多闲工夫。
<<else>>让我跟那五个人去从头到尾解释一遍？我觉得我现在可没那那么闲工夫。<<endif>>
<<choice [[好啦，继续描述吧。|havetodescribe]]>> | <<choice [[你的访客离你还有多远？|howclosearethey]]>>


:: whatweapon
这个山峰，这间控制室，是个武器控制中心，对吧？那个脉！冲！倒！计！时！的意义也就在于此。
我不确定这究竟是什么武器——我猜是电磁脉冲、粒子束之类的威！力！巨！大！的武器——
但很有可能，要是这个武器强大到能将一整艘星际飞船炸出天际……
……那么我需要做的就是想办法重新编程来对准这！里！的坐标发射了……
……并且它的威力足够把这座山的每一寸土地都烤得嘎嘣脆。
当然还包括那些尖叫的躲在瓦里亚号船员体内到处游荡的绿怪物了！
<<choice [[别着急……|holdonweapon]]>> | <<choice [[你真的能对它重新编程吗？|reallyreprogram]]>>


:: havetodescribe
它好像有……触须，织入了野小子的身体。有动脉和肌肉还有，我说不清楚，神经树突吧……
……整个网状系统，或者说是多个网络从那个怪物身上伸出来，伸进野小子的身体系统，交织在一起。
于是这玩意儿就……缠住了我那可怜的小老鼠的神经、肌肉、器官……一切。
通过这样，这个寄生怪物，这个住在野小子体内的乘客，我描述不出来……
……就像把野小子当衣服一样穿着走动，当木偶一样提着跳舞。
但是并不笨拙。不像它不知道老鼠怎么行走和活动一样。
它的方法堪称完美。唯一不同的地方是眼睛。老鼠的眼睛泛着荧光绿。
还有……那种叫声。我可从来没听过老鼠会这样叫。我就从没有听过什么东西会发出这种声音。
<<choice [[好吧，这可……真吓人。|wellterrifying]]>> | <<choice [[它正在做什么？|whatisitdoingnow1]]>>


:: clearoftheet1
是的，我是在离它远点。但并不代表我对它不好奇。
据我所知，我应该是第一个见过这种生命体的人。
（或者，更黑暗的想法：我是第一个见过这种生命体还没死的人。）
既然没有办法录下这次经历，我就只好尽量观察了。
<<choice [[说得有理，再跟我多讲些。|tellmemore1]]>> | <<choice [[其他的老鼠在做什么？|otherratsdoing1]]>>


:: tellmemore1
我是说，我观察这些老鼠已经有几个月了。我知道正常老鼠应该是什么样子的……
而且我也知道这些老鼠正常的样子。
我知道小山子左耳后面长了个东西，因为它会不停地挠啊挠。
我知道小矮子在吃东西的时候非常警惕，脑袋会压得很低。
像这些行为，它们到现在居然还保留着。
[[otherratsdoing1]]


:: whatthistime
访客上门了。
我……我该做什么？
<<choice [[他们在威胁你吗？|threateningyou]]>> | <<choice [[你还安全吗？|areyousafe]]>>


:: morewithrats
不，不。和这些老鼠没关系。
[[whatthistime]]


:: somethingtocompute
我只是弄不清楚电脑系统。
我还不如直接用我的拳头——或者额头——随便乱敲键盘来得快呢！
……等等，就是它了！
<<choice [[“它”是什么？乱砸一气？|randombashing]]>> | <<choice [[不合适吧……|cantbeyourplan]]>>


:: alwayspackheat
我知道，我知道……我这怂样实在佩不上我肩头绣的星条旗！
动动脑子，泰勒，作死啊……快想！
……等一下。我需要一件武器，对吧？
我有武器了！
<<choice [[什么武器？|whatweapon]]>> | <<choice [[先别用武器……|holdonweapon]]>>


:: howclosearethey
快速朝走廊扫一眼……鬼都没见到一个。
他们可能五分钟也可能五秒钟就到。
他们可能正在某条侧廊里找——那条路本来应该是我的，要是我一直理智思考的话。
他们可能消失在了山峰深处，我再不会看到他们了。
不知道，不知道，我觉得……我应该和你说下这个小生物。
[[havetodescribe]]


:: wellterrifying
对头，确实吓人。
我在这身宇航服底下出热汗冷汗交织了一百次了。
我觉得每回只要听到一点声音，我都能被吓得窜出宇航服两丈高。而且，不知怎么的，我现在听到的声音可不止一点点。
<<choice [[试着冷静下来。|trytostaycalm]]>> | <<choice [[那个生物在做什么？|whatisitdoingnow1]]>>


:: submittocreature
我
不知道
会
这么痛
[[submittocreaturetwo]]


:: submittocreaturetwo
[宇航服传感器显示严重心跳过速]
[检测到局部缺血]
[检测到心肌梗塞]
[[delay 4s|connectionlost]]


:: trytostaycalm
相信我，我在尽量保持冷静。
但在眼睁睁看着外星生命体在角落里大肆践踏老鼠内脏的情况下，恐怕很难啊。
你懂吧？
[[whatisitdoingnow1]]


:: youmustgetaway1
我知道，我也想要。我也想要跑。
但我的腿真的抬不起来了。我已经饥肠辘辘地在这个卫星上到处走了三天了。
我的身体让我停下别跑了，就呆在这儿，和我的朋友们一起。
[[stoplookingateyes]]


:: havetofight
我不够强壮！我打不过！
哦，老天爷啊……就这样了，是吗？我要死了，死在这个房间里，死在瓦里亚船员的手上！
他们要用那作死的寄生小怪物感染我……
我！要！死！了！
他们怎么还尖叫个不停？！
<<choice [[肯定有解决的办法的。|mustbeasolution]]>> | <<choice [[难道你屈服了吗？！|submittocreature]]>>


:: trotterhappened
小笨脚特罗特……现在已经不是速度最慢的那个了。
而且我也不觉得现在他们中还有哪个速度慢。
[[escapeinjury]]




:: likeisaid
好吧，随着敌人把我们变成僵尸，终于轮到你说“我说了吧”。
说这话感觉不错吧？
我觉得如果我能把老朋友们击倒并利用混乱，就能出去了！
我认为我能躲过小笨脚特罗特，他是所有人里行动最迟缓的。
哎哟……
<<choice [[出了什么事？|trotterhappened]]>> | <<choice [[你成功了吗？|didyoumakeit]]>>


:: areyousafe
我……不知道……
有眼泪划过我的面颊，我也不知道为什么会这样。
感觉我的眼睛已经不受控制了，就好像沒经大脑允许就自由行动了起来。
<<choice [[告诉我你看见了什么！|whatdoyousee]]>> | <<choice [[有必要的话就休息一下。|takeaminuteifyouneed]]>>


:: threateningyou
不，没人在威胁我。没人说一个字，没人动一下。
他们就是……站在那里。而我……我抖得像筛子，我快要扛不住了。
<<choice [[我不明白。|idontgetit]]>> | <<choice [[告诉我发生了什么。|areyousafe]]>>


:: mustbeasolution
我难道还有什么办法吗？
我一突围，他们就挡我，力气还大得要命。
我一后退，他们就回到诡异的“待命”状态，等着我下一次冲出去。
我在这里能做的就是瞎鼓捣这这些电脑，可是我真的不知道该怎么鼓捣。
别误会，我真心想更积极地对付这些狱卒，但是没有武器的话怕是不太可能。
真希望我能找一个，或者，在小飞船里的时候用零部件造一个——
但我当时真的不认为在这个星球上我需要带枪。
<<choice [[有枪总是好的。|alwayspackheat]]>> | <<choice [[总得多考虑一下。|somethingtocompute]]>>


:: escapeinjury
作死啊……他们只是……他们只是冰冷地站在那里，静候着我的下一个动作……
而且他们突然间变得……好强壮……真！滴！把精力都集中在了阻止我上。
<<if $hurtshoulder is 1>>哎哟！阿戴尔直击在我受伤的肩膀上！<<endif>>
<<if $hurtankle is 1>>安冬——哦——死死抓紧了我扭伤的脚踝。<<endif>>
他们都瞄准着我受伤的位置，就好像他们知！道！我哪里受伤了一样！
我没法……我打不过这么多人！我……他们随！便！谁！都比我厉害！
<<choice [[你得跟他们打才行！|havetofight]]>> | <<choice [[打不过就跑！|nofightflee]]>>


:: fightthecreature
我……不行……
哎哟，我的妈呀！我在做！什！么！？
我刚意识到：“它是通过嘴巴钻进体内的！”
那……绿口水。那早晨可怕的味道。哦，天啊！
这种玩意儿曾企图趁我！睡！觉！时！爬进我的体内！
我得快跑！
<<choice [[我跟你说过的！|likeisaid]]>> | <<choice [[你能找到出路吗？|findanexit]]>>


:: surestaycalm
这是我从未经历过的宁静。
科尔比从野小子的残骸中拾起等候着的那个绿色小生物。
现在我明白了。它等着我，我也在等着它。
它通过嘴进入体内，那些爪印也是这样来的。
我只要放松就好。
微笑。
<<choice [[不！抗争一下啊！|fightthecreature]]>> | <<choice [[难道你屈服了吗？！|submittocreature]]>>


:: notstillyourfriends
我知道，我知道他们不是。但他们看起来真像我的朋友。
眼睛不对，我知道……但是我看着他们越久，我也就觉得他们越像。
……他们的嘴巴旁边有小小的印子。暗绿色的疤痕……像是……什么爪子的抓痕。
像是什么东西使劲儿地……爬进他们的嘴巴。
<<choice [[你快走。|youmustgetaway1]]>> | <<choice [[别再看他们的眼睛。|stoplookingateyes]]>>


:: whatsupwithzombies
我的愿望果然没有实现……
……虽然我也不知道我怎么会以为能避得开！
[[whoareyoutalkingto]]


:: cannotbepossible
是啊！我也知！道！这不可能！
<<if $capburied is 1 and $crewburied is 1>>我把他们所有人都埋了的——穿着那些制服埋的——我还在瓦里亚号的时候！
<<elseif $crewburied is 1 and $capburied is 0>>我把船员都埋了的——穿着那些制服埋的——我还在瓦里亚号的时候！
<<elseif $crewburied is 0 and $capburied is 1>>我把船长埋了的——穿着那套制服埋的——我还在瓦里亚号的时候！
<<endif>>
<<if $capburied is 1 or $crewburied is 1>>混蛋，我努力了！我做了我所知道的一切！
我尽最大努力的表达敬意，体面地安葬他们，这混蛋的卫星就是不遂人愿。<<endif>>
哦，不要啊，别这样。
<<choice [[发生了什么？|whatsupwithzombies]]>> | <<choice [[你在和谁说话？|whoareyoutalkingto]]>>


:: thisisexciting
是啊，我本来也这么想：好激动啊！
[[nametagtime]]



:: nofightflee
往哪里逃？没地方逃了。他们把我逼到了控制室里。
我打不过他们。我赢不了。
哦，老天爷啊……就这样了，是吗？我要死了，死在这个房间，死在瓦里亚船员的手上！
他们要用那作死的寄生小怪物感染我.……
我！要！死！了！
他们怎么还尖叫个不停？！
<<choice [[肯定有解决的办法的。|mustbeasolution]]>> | <<choice [[难道你屈服了吗？！|submittocreature]]>>


:: didyoumakeit
不！我失败了！
要是成功了的人不会发出“哎哟”的声音。
[[escapeinjury]]


:: findanexit
<<if $capalive is 1 and $power is "pod">>唯一的“出路”就是击退四个曾是我的队友的冷面人……
<<else>>唯一的“出路”就是击退五个曾是我的队友的冷面人……<<endif>>
……而且还得指望我能在混乱中爬出去！
我觉得我能躲过小笨脚特罗特，他是所有人里行动最迟缓的——
啊唷
<<choice [[出了什么事？|trotterhappened]]>> | <<choice [[你成功了吗？|didyoumakeit]]>>



:: stoplookingateyes
你不懂。你没在听我说。
这是自从事故以来我第一次觉得不再担心任何事、所有事。
不管接下来会发生什么，该发生的总会发生的。
所有的一切：瓦里亚号的事故，我找到这个山峰……所以这一切都是注定，让我来到这个时刻。
来到这种宁静。
<<choice [[好了，好了，冷静点。|surestaycalm]]>> | <<choice [[保持冷静。离开那里！|screwcalm]]>>



:: whatdoyousee
好吧，只是……只是没有任何话语能精确地描述我正在经历的事情……
所以我打算平铺直述，尽量不要讲到一半就发狂尖叫或者什么的。
当他们走进来时，我首先见到的是宇航服上的美国国旗。
缝在左边的肩膀上，就像我自己的一样。永远的星条旗。
那身衣服看起来有些残破，积了点灰，就好像他们穿过了地狱然后又回到了人间。
或者说没有“完全”返回人间。
他们都戴着圆形的头盔，面罩上有一层不透明的金膜。
我知道，我知道，这是为了帮助隔离紫外线……
……不过，没什么比通过一层金膜环视整个世界更像“美国”了，对吧？
<<choice [[觉得想家了？|feelinghomesick]]>> | <<choice [[这可真是令人激动呢！|thisisexciting]]>>


:: idontgetit
我很抱歉，我只是……需要点时间。我很抱歉，
[[delay 3m|whatdoyousee]]


:: nametagtime
看到这些宇航服，它们多么令人安心，多么亲切熟悉……
……是那么的无！比！亲！切！
<<if $capalive is 1 and $power is "pod">>我看着那四块名牌，缝在四套美国制服上的名牌，写着清晰可辨的正楷名字。
安冬、特罗特、阿戴尔、科尔比。
<<else>>我看着那五块名牌，缝在五套制服上的名牌，写着清晰可辨的正楷名字。
安冬、特罗特、阿戴尔、科尔比。
阿雅船长。<<endif>>
<<choice [[那根本不可能。|cannotbepossible]]>> | <<choice [[快离开那里。出去。|getoutoftherenow]]>>


:: screwcalm
不……要是之前，我肯定还会赶快逃。但现在我觉得我要回到团队中去。
我需要这只寄生生物，就像它需要我成为它的宿主一样。
[[surestaycalm]]


:: youmustgetaway
我知道，我想要。我想要跑。
但我腿真的抬不起来了。我已经饥肠辘辘地在这个卫星上到处走了三天了。
我真的不想再跑了。就呆在这儿，和我的朋友们一起。
<<choice [[那些人不再是你的朋友了。|notstillyourfriends1]]>> | <<choice [[别再看他们的眼睛。|stoplookingateyes]]>>


:: whoareyoutalkingto
科尔比~~
或者说是身上有科尔比名牌的那个。
摘下了头盔。
……哦，不，不要啊，该死的。
是她，真的是她。
我一直希望，希望他们是别的飞船的幸存者。
是些正常人，正好遇到了瓦里亚号，拿了这些宇航服来穿上防身。
可惜，不是。这真的是科尔比。
摘下头盔，真的是他们所！有！人！
跟我最后看到他们时一模一样。
除了那种空洞的，绿色的，发光的，眼睛。
<<choice [[你快逃。|youmustgetaway]]>> | <<choice [[他们不再是你的朋友了。|notstillyourfriends]]>>


:: getoutoftherenow
那你觉得，我要怎么才能离开呢？这里只有一个出口……
<<if $capalive is 1 and $power is "pod">>有四个人（或者说曾经是人的家伙）横在我和出口之间。
<<else>>有五个人（或者说曾经是人的家伙）横在我和出口之间。<<endif>>
从统计学来看我的幸存几率的话，我得说是……“完蛋了”。
（用公制术语来讲，应该是“完犊子了”。）
现在……哦。哦，不要，哦，别这样。
<<choice [[发生了什么？|whatsupwithzombies]]>> | <<choice [[你在和谁说话？|whoareyoutalkingto]]>>


:: feelinghomesick
真的，我真的好想家啊。
[[nametagtime]]


:: takeaminuteifyouneed
谢谢你。我真的，只是，真的需要点时间。我很抱歉，
[[delay 3m|whatdoyousee]]


:: notstillyourfriends1
我知道，我知！道！他们不是。但他们看起来真像我的朋友。
眼睛不对，我知道……但是我看他们越久，就越觉得他们像。
……他们的嘴巴旁边有小小的印子。暗绿色的疤痕……像是……什么爪子的抓痕。
像是什么东西使劲儿地……爬进他们的嘴巴。
[[stoplookingateyes]]


:: gameover
gameover]====]
