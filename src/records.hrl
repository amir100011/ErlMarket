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
-record(processesAllocationToNodes,{monitorRef,moduleName,processName}).
-define(DEPARTMENT_LIST, [dairy, meat, bakery]).
-define(PURCHASE_DEPARTMENT_NODE, 'amir@132.72.52.249').
-define(CASHIER_SERVER_NODE,  'amir@132.72.52.249').
-define(MASTER_SERVER_NODE,  'dor@132.72.54.107').
-define(INTERFACE_NODE, 'dor@132.72.54.107').
-define(NodeList, [?INTERFACE_NODE, ?MASTER_SERVER_NODE]).%, ?CASHIER_SERVER_NODE, ?MASTER_SERVER_NODE, ?INTERFACE_NODE]).
-include_lib("stdlib/include/qlc.hrl").