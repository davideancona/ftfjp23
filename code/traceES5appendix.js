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
var s1 = new HashSet();
var s2 = new HashSet();
s1.add(1);
s2.add(2);
sset.add(s1);
s1.contains(1);
s1.add(1);
sset.add(s2);
sset.remove(s1);
s2.remove(2);
s1.remove(1);
s2.remove(1);
sset.remove(s2);
s1.add(1);
s2.add(2);

