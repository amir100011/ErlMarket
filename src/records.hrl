%%%-------------------------------------------------------------------
%%% @author dorliv
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2019 12:19 PM
%%%-------------------------------------------------------------------
-author("dorliv").
-record(product_info,{product_name, price, entering_time, expiry_time, amount}).
-record(product, {department, product_name, price, expiry_time}).
-record(department, {department_name, department_pid}).
-include_lib("stdlib/include/qlc.hrl").