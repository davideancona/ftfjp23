'use strict';

class HashSet {
    constructor() {
        this.set = new Set();
    }
    add(el) {
        const res = !this.set.has(el);
        this.set.add(el);
        return res;
    }
    remove(el) {
        const res = this.set.has(el);
        this.set.delete(el);
        return res;
    }
    contains(el) {
        return this.set.has(el);
    }
}

let s = new HashSet();
let s1 = new HashSet();
let s2 = new HashSet();
s1.add(1);
s2.add(2);
s.add(s1);
s1.contains(1);
s.add(s2);
s.remove(s1);
s1.remove(1);
s2.remove(1);
s.remove(s2);
s1.add(1);
s2.add(2);
