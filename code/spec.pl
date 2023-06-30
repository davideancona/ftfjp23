:- module(spec, [(trace_expression/2), (match/2)]).
:- use_module(monitor(deep_subdict)).
:- use_module(library(clpr)).
match(_event, new_hash_et(Hash_id)) :- deep_subdict(_event, _{event:"func_post", name:"HashSet", resultId:Hash_id}).
match(_event, not_new_hash_et) :- not(match(_event, new_hash_et(_))).
match(_event, add_et(Hash_id, Elem_id)) :- deep_subdict(_event, _{event:"func_post", targetId:Hash_id, name:"add", argIds:[Elem_id], res:true}).
match(_event, not_add_et(Hash_id)) :- not(match(_event, add_et(Hash_id, _))).
match(_event, remove_et(Hash_id, Elem_id)) :- deep_subdict(_event, _{event:"func_post", targetId:Hash_id, name:"remove", argIds:[Elem_id], res:true}).
match(_event, modify_et(Targ_id)) :- match(_event, add_et(Targ_id, _)).
match(_event, modify_et(Targ_id)) :- match(_event, remove_et(Targ_id, _)).
match(_event, not_modify_remove_et(Hash_id, Elem_id)) :- not(match(_event, modify_et(Elem_id))),
	not(match(_event, remove_et(Hash_id, Elem_id))).
match(_event, op_et(Hash_id, Elem_id)) :- deep_subdict(_event, _{targetId:Hash_id}).
match(_event, op_et(Hash_id, Elem_id)) :- deep_subdict(_event, _{targetId:Elem_id}).
match(_event, relevant_et) :- match(_event, new_hash_et(_)).
match(_event, relevant_et) :- match(_event, modify_et(_)).
match(_event, any_et) :- deep_subdict(_event, _{}).
match(_event, none_et) :- not(match(_event, any_et)).
trace_expression('Main', Main) :- (Main=((relevant_et>>clos(SafeHash));1)),
	(SafeHash=(star(not_new_hash_et)*optional(var(hash_id, (new_hash_et(var(hash_id))*(app(SafeHashTable, [var(hash_id)])/\SafeHash)))))),
	(SafeHashTable=gen([hash_id], (star(not_add_et(var(hash_id)))*optional(var(elem_id, (add_et(var(hash_id), var(elem_id))*(app(SafeHashElem, [var(hash_id), var(elem_id)])/\app(SafeHashTable, [var(hash_id)])))))))),
	(SafeHashElem=gen([hash_id, elem_id], (star(not_modify_remove_et(var(hash_id), var(elem_id)))*(remove_et(var(hash_id), var(elem_id))*1)))).
