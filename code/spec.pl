:- module(spec, [(trace_expression/2), (match/2)]).
:- use_module(monitor(deep_subdict)).
:- use_module(library(clpr)).
match(_event, new_hash_et(HashId)) :- deep_subdict(_event, _{event:"func_post", name:"HashSet", resultId:HashId}).
match(_event, not_new_hash_et) :- not(match(_event, new_hash_et(_))).
match(_event, modify_pre_et(TargId)) :- deep_subdict(_event, _{event:"meth_pre", targetId:TargId, name:"add"}).
match(_event, modify_pre_et(TargId)) :- deep_subdict(_event, _{event:"meth_pre", targetId:TargId, name:"remove"}).
match(_event, modify_post_et) :- deep_subdict(_event, _{event:"meth_post", name:"add", res:true}).
match(_event, modify_post_et) :- deep_subdict(_event, _{event:"meth_post", name:"remove", res:true}).
match(_event, not_modify_et(TargId)) :- not(match(_event, modify_et(TargId))).
match(_event, add_et(HashId, ElemId)) :- deep_subdict(_event, _{event:"meth_post", targetId:HashId, name:"add", argIds:[ElemId], res:true}).
match(_event, not_add_et(HashId)) :- not(match(_event, add_et(HashId, _))).
match(_event, remove_et(HashId, ElemId)) :- deep_subdict(_event, _{event:"meth_post", targetId:HashId, name:"add", argIds:[ElemId], res:true}).
match(_event, modify_et(HashId, ElemId)) :- match(_event, modify_et(HashId)).
match(_event, modify_et(HashId, ElemId)) :- match(_event, modify_et(ElemId)).
match(_event, any_et) :- deep_subdict(_event, _{}).
match(_event, none_et) :- not(match(_event, any_et)).
trace_expression('Main', Main) :- (Main=(star(not_new_hash_et)*optional(var(hashId, (new_hash_et(var(hashId))*(app(VerifyHash, [var(hashId)])/\Main)))))),
	(VerifyHash=gen([hashId], (star(not_add_et(var(hash_id)))*var(elemId, (add_et(var(hashId), var(elemId))*((modify_et(var(hashId), var(elemId))>>app(VerifyHashElem, [var(hashId), var(elemId)]));1)))))),
	(VerifyHashElem=gen([hashId, elemId], ((star(not_modify_et(var(elemId)))*remove_et(var(hashId), var(elemId)))|app(VerifyHash, [var(hashId)])))).
