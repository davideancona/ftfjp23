:- module(spec, [(trace_expression/2), (match/2)]).
:- use_module(monitor(deep_subdict)).
:- use_module(library(clpr)).
match(_event, new_et(InstId)) :- deep_subdict(_event, _{event:"init_post", class:"HashSet", res:InstId}).
match(_event, not_new_hash_et) :- not(match(_event, new_et(_))).
match(_event, modify_et(TargId)) :- deep_subdict(_event, _{event:"meth_post", targetId:TargId, name:"add", res:true}).
match(_event, modify_et(TargId)) :- deep_subdict(_event, _{event:"meth_post", targetId:TargId, name:"remove", res:true}).
match(_event, not_modify_et(TargId)) :- not(match(_event, modify_et(TargId))).
match(_event, add_et(TargId, ElemId)) :- deep_subdict(_event, _{event:"meth_post", targetId:TargId, name:"add", argIds:[ElemId], res:true}).
match(_event, not_add_et(TargId)) :- not(match(_event, add_et(TargId, _))).
match(_event, remove_et(TargId, ElemId)) :- deep_subdict(_event, _{event:"meth_post", targetId:TargId, name:"add", argIds:[ElemId], res:true}).
match(_event, meth_on_et(HashId, ElemId)) :- match(_event, modify_et(HashId)).
match(_event, meth_on_et(HashId, ElemId)) :- match(_event, modify_et(ElemId)).
match(_event, any_et) :- deep_subdict(_event, _{}).
match(_event, none_et) :- not(match(_event, any_et)).
trace_expression('Main', Main) :- (Main=(star(not_new_et)*optional(var(hashId, (new_et(var(hashId))*(app(Verify, [var(hashId)])/\Main)))))),
	(Verify=gen([hashId], (star(not_add_et(var(hash_id)))*var(elemId, (add_et(var(hashId), var(elemId))*((star(not_modify_et(var(elemId)))*remove_et(var(hashId), var(elemId)))|app(Verify, [var(hashId)]))))))).
