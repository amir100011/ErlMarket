%%%-------------------------------------------------------------------
%%% @author
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2019 14:22
%%%-------------------------------------------------------------------
-module(cashierServer).
-behavior(gen_server).
-define(LOGGER_FILE_PATH, "../Logger-CashierServer.txt").
-author("amir").
-include_lib("records.hrl").
%% API
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).
-export([start/0,callFunc/1,castFunc/1, payToMaxAndReturnTheRest/2, payToMaxAndReturnTheRest/4]).

start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, []}.

%% @doc uses to call for using the cashier sever
callFunc(MSG) ->
  try gen_server:call({global, ?MODULE}, MSG) of
    AnsFromServer -> AnsFromServer
  catch
    exit:Error -> timer:sleep(2500),
      writeToLogger(variable ," ~p is not responding becuase ~p, resending message ~n",[?MODULE, Error]),
      case interface:callFunc(isFinished) of
          false ->     Ans = callFunc(MSG),
                       Ans;
           true -> writeToLogger(variable ,"System is down, closing stray process from Module ~p ~n",[?MODULE])
      end
  end.
%% @doc interface function for using gen_server cast
castFunc(Message) ->
  try  gen_server:cast({global, ?MODULE}, Message) of
    AnsFromServer-> AnsFromServer  % usually no reply just ok or some atom
  catch
    exit:Error -> timer:sleep(2500),
      writeToLogger(variable,"~p is not responding becuase ~p, resending message ~n",[?MODULE, Error]),
      case interface:callFunc(isFinished) of
            false -> castFunc(Message);
            true -> writeToLogger(variable ,"System is down, closing stray process from Module ~p ~n",[?MODULE])
      end
  end.

%% @doc returns its own PID for watchdog monitoring
handle_call(pid, _From, State) ->
  Reply= self(),
  {reply, Reply, State}.


handle_cast(terminate, State) ->
  {stop, normal, State};

%% @doc  checks if the customer can pay or not, if not pays to the maximum available, the rest are going back to the relevant department
handle_cast({pay,ListOfProductsAndAmounts,CustomerBalance}, State) ->
  %writeToLogger("Handle Cast - CashierServer ",[ListOfProductsAndAmounts,CustomerBalance]),
  pay(ListOfProductsAndAmounts, CustomerBalance),
 {noreply, State}.

handle_info(MSG, State)->
  io:fwrite("cashierServer Department gets ~p~n",[MSG]),
  {noreply, State}.

terminate(_Reason, _State) ->
  io:fwrite("~p says bye bye ~n",[?MODULE]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @doc checks if a customer can pay for all his products (budget issues) if not spawns a new cashier to return all the products that he cannot afford
pay(ListOfProductsAndAmounts,CustomerBalance)->
  %writeToLogger("payFunction ",[ListOfProductsAndAmounts,CustomerBalance]),
  {CanPay, AmountToPay} = canPay(ListOfProductsAndAmounts,CustomerBalance),
  case CanPay of
    canPay ->
      updateErlMarketBalance(AmountToPay),
      printBoughtListToLogger(ListOfProductsAndAmounts);
    cannotPay ->
      spawn(cashierServer, payToMaxAndReturnTheRest, [ListOfProductsAndAmounts, CustomerBalance])
  end.

%% @doc pay up and leave the shop
canPay(ListOfProductsAndAmounts,CustomerBalance) ->
  SumOfAllProducts = sumOfAllProducts(ListOfProductsAndAmounts),
  if SumOfAllProducts =< CustomerBalance -> {canPay, SumOfAllProducts};
    true -> {cannotPay,0}
  end.

%% @doc checks what is the productss that the customer can afford to buy.
%% than let the customer pay for them.
%%returns the rest to the right department and update the ErlMart budget.
payToMaxAndReturnTheRest(ListOfProductsAndAmounts, CustomerBalance)->
  spawn_monitor(?MODULE, payToMaxAndReturnTheRest, [ListOfProductsAndAmounts, CustomerBalance, dict:new(), []]),
  receive
    {'DOWN', _, process, _, normal} -> done;
    {'DOWN', _, process, PID, Reason} ->
      writeToLogger(variable, "Process ~p Died because ~p , respawning a new process~n",[PID, Reason]),
      payToMaxAndReturnTheRest(ListOfProductsAndAmounts, CustomerBalance)
  end.
payToMaxAndReturnTheRest([],_CustomerBalance, Dict, ListOfProducts) ->
  case dict:is_key(amountToPay, Dict) of % when the customer didnt buy anything this will be false
    true ->  {Amount, UpdatedDict} = dict:take(amountToPay, Dict),
             updateErlMarketBalance(Amount);
    false -> UpdatedDict = Dict
  end,
  writeToLogger(variable, "Bought Products by Customer is : ~p ~n", [ListOfProducts]), % for debugging
  returnProducts(dict:fetch_keys(UpdatedDict), UpdatedDict); % return products to departments
payToMaxAndReturnTheRest([H|T],CustomerBalance, Dict, ListOfProducts) ->
  {CanPay, AmountToPay} = canPay([H],CustomerBalance),
  case CanPay of
    canPay ->
      NewCustomerBalance = CustomerBalance - AmountToPay,
      %writeToLogger("Customer:Oldbalance - ",CustomerBalance, " NewBalance - ", NewCustomerBalance),
      case dict:is_key(amountToPay, Dict) of
        false -> DictUpdated = dict:store(amountToPay, AmountToPay, Dict);
        true -> OldAmount = dict:fetch(amountToPay, Dict),
                DictUpdated = dict:store(amountToPay, AmountToPay + OldAmount, Dict)
      end,
      payToMaxAndReturnTheRest(T, NewCustomerBalance, DictUpdated, ListOfProducts ++ [H]);
    cannotPay ->
      DepartmentName = H#departmentProduct.department,
      case dict:is_key(DepartmentName, Dict) of
        false -> DictUpdated = dict:store(DepartmentName, [H], Dict);
        true ->  DictUpdated = dict:append(DepartmentName, H, Dict)
      end,
      payToMaxAndReturnTheRest(T, CustomerBalance, DictUpdated, ListOfProducts)
  end.

%% @doc been called when a customer cannot pay for all the products he bought. this function returns the remaining products to the right department.
returnProducts([],_) -> done;
returnProducts([H|T], Dict) ->
  {ListOfProduct, UpdatedDict} = dict:take(H, Dict),
  returnProductToDepartment(ListOfProduct, H),
  returnProducts(T, UpdatedDict).

returnProductToDepartment(DepartmentProducts, Department) ->
  department:castFunc(Department, {return, DepartmentProducts}).

sumOfAllProducts([H|T]) ->
  costOfSingleProduct(H) + sumOfAllProducts(T);
sumOfAllProducts([]) -> 0.

costOfSingleProduct({departmentProduct,_department,_productName,PriceForEach,_expiryDate, Amount}) ->
  Amount * PriceForEach.

updateErlMarketBalance(AmountToAdd) ->
  purchaseDepartment:castFunc({updateBalance, add, AmountToAdd}).

%%------------------WRITING TO LOGGER------------------

writeToLogger(String, List) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S,"~s~n ",[String]),
  file:close(S),
  file:write_file(?LOGGER_FILE_PATH, io_lib:format("~p.~n", [List]), [append]).

writeToLogger(variable, String, Variables) ->
  {ok, S} = file:open(?LOGGER_FILE_PATH, [append]),
  io:format(S, String, Variables),
  file:close(S).

printBoughtListToLogger(ListOfBoughtProducts) ->
  writeToLogger("Bought Products ~p ~n",[ListOfBoughtProducts]).