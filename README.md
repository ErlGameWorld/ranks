ranks
=====

    多工作进程多类型的排行榜

Build
-----

    $ rebar3 compile

Useage
-----

    startWork/1             %% 创建指定数量的排行榜工作者
    initRank/3              %% 创建新的类型排行榜
    updateScore/3           %% 更新某类型的排行榜分数
    updateInfo/2            %% 更新公共信息
    getRankInfo/5           %% 获取排行某页的信息