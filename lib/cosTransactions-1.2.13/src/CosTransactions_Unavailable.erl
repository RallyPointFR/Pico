%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosTransactions_Unavailable
%% Source: /home/vagrant/otp-support/lib/cosTransactions/src/CosTransactions.idl
%% IC vsn: 4.3.2
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosTransactions_Unavailable').
-ic_compiled("4_3_2").


-include("CosTransactions.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_except,"IDL:omg.org/CosTransactions/Unavailable:1.0",
                   "Unavailable",[]}.

%% returns id
id() -> "IDL:omg.org/CosTransactions/Unavailable:1.0".

%% returns name
name() -> "CosTransactions_Unavailable".


