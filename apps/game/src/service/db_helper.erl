%% 自动生成，请勿编辑 
-module(db_helper).
-include("common.hrl"). 
-include("tab.hrl").    
-export([serialize/1, deserialize/2]).  
serialize(Role) ->                       
    term_to_binary(Role).               
deserialize(Role, Data) ->              
    Role0 = binary_to_term(Data),       
    #role{id = ID,platform = PLATFORM,accname = ACCNAME,name = NAME,sex = SEX,lvl = LVL,exp = EXP,t_goods = T_GOODS} = Role0, 
    Role2 = Role#role{id = ID,platform = PLATFORM,accname = ACCNAME,name = NAME,sex = SEX,lvl = LVL,exp = EXP,t_goods = T_GOODS}, 
    Role2.