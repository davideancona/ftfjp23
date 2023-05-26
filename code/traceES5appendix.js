'use strict';

function HashSet() {
    this.set = new Set();
}
HashSet.prototype.add = function (el) {
    var res = !this.set.has(el);
    this.set.add(el);
    return res;
}
HashSet.prototype.remove = function (el) {
    var res = this.set.has(el);
    this.set.delete(el);
    return res;
}
HashSet.prototype.contains = function (el) {
    return this.set.has(el);
}

var sset = new HashSet();
var set1 = new HashSet();
var set2 = new HashSet();
set1.add(1);
set2.add(2);
sset.add(set1);
set1.add(1);
set1.contains(1);
sset.add(set2);
sset.remove(set1);
set1.remove(1);
set2.remove(1);
//s2.remove(2);
sset.remove(set2);
set1.add(1);
set2.add(2);
