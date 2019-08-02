%%%-------------------------------------------------------------------
%%% @author dorliv
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2019 12:19 PM
%%%-------------------------------------------------------------------
-author("dorliv").
-record(departmentProduct,{department, product_name, price, expiry_time, amount}).
-record(product, {department, product_name, price, expiry_time}).
-record(department, {department_name, department_pid}).
-record(shoppinlistelement, {department_name, product_name, price, amount}).
-define(DEPARTMENT_LIST, [dairy, meat, bakery]).
-record(processesAllocationToNodes, {monitorRef,moduleName,processName}).
-include_lib("stdlib/include/qlc.hrl").