'use strict';

const { readFileSync } = require('fs');

const lines = readFileSync(process.argv[2], 'utf8').split('\n');
// console.log(lines);
const stack = [];
for (const l of lines) {
    if (l === '') break;
    const o = JSON.parse(l);
    if (o.event === 'func_pre') {
        stack.push({ targetId: o.targetId, argIds: o.argIds });
        console.log(l);
    }
    else {
        const o2 = stack.pop();
        o.targetId = o2.targetId;
        o.argIds = o2.argIds;
        console.log(JSON.stringify(o));
    }
}
