
-module(epush4_misc).

-export([c_r/2]).




% recursion for function list
c_r(FunList, Args) ->
  case_recursion(FunList, Args).
case_recursion(FunList, Args) ->
  Fun = fun
            (F, [N|R], Acc = #{status := ok})   -> F(F, R, apply(N, [Acc]));
            (_F, _,    Acc = #{status := done}) -> Acc#{status := ok};
            (_F, _,    Acc) -> Acc
        end,
  Fun(Fun, FunList, Args).

