"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.exampleEdges = exports.exampleNodes = void 0;
/* Example of a complex tree:
    '../test/test-network-illustration.png'
*/
exports.exampleNodes = [
    {
        id: 'A',
        label: 'a',
        value: 0,
        delta: 1,
        valence: 'Good',
    },
    {
        id: 'B',
        label: 'b',
        value: 0,
        delta: 1,
        valence: 'Bad',
    },
    {
        id: 'C',
        label: 'c',
        value: 0,
        delta: 1,
        valence: 'Bad',
    },
    {
        id: 'D',
        label: 'd',
        value: 0,
        delta: 1,
        valence: 'Bad',
    },
    {
        id: 'E',
        label: 'e',
        value: 0,
        delta: 1,
        valence: 'Bad',
    },
    {
        id: 'F',
        label: 'f',
        value: 0,
        delta: 1,
        valence: 'Bad',
    },
    {
        id: 'G',
        label: 'g',
        value: 0,
        delta: 1,
        valence: 'Bad',
    },
];
exports.exampleEdges = [
    // Main branch from origin // Direct and indirect route from A-->C
    {
        source: 'A',
        target: 'B',
        beta: 0.5,
    },
    {
        source: 'B',
        target: 'C',
        beta: 0.5,
    },
    // Loops
    // Self-loops
    {
        source: 'B',
        target: 'B',
        beta: 0.5,
    },
    // One-step loops (x-->y-->x)
    {
        source: 'A',
        target: 'C',
        beta: 0.5,
    },
    {
        source: 'C',
        target: 'A',
        beta: 0.5,
    },
    {
        source: 'B',
        target: 'F',
        beta: 0.5,
    },
    {
        source: 'F',
        target: 'B',
        beta: 0.5,
    },
    {
        source: 'C',
        target: 'F',
        beta: 0.5,
    },
    {
        source: 'F',
        target: 'C',
        beta: 0.5,
    },
    {
        source: 'A',
        target: 'F',
        beta: 0.5,
    },
    {
        source: 'F',
        target: 'A',
        beta: 0.5,
    },
    // n-step loops (x-->n-->y)
    {
        source: 'C',
        target: 'D',
        beta: 0.5,
    },
    {
        source: 'D',
        target: 'E',
        beta: 0.5,
    },
    {
        source: 'E',
        target: 'C',
        beta: 0.5,
    },
    {
        source: 'E',
        target: 'F',
        beta: 0.5,
    },
    {
        source: 'F',
        target: 'G',
        beta: 0.5,
    },
    {
        source: 'G',
        target: 'E',
        beta: 0.5,
    },
    {
        // (C<-->C)
        source: 'G',
        target: 'C',
        beta: 0.5,
    },
    // Duplicate edges
    {
        source: 'C',
        target: 'A',
        beta: 0.5,
    },
    {
        source: 'C',
        target: 'F',
        beta: 0.5,
    },
    {
        source: 'F',
        target: 'G',
        beta: 0.5,
    },
];
//# sourceMappingURL=testData.js.map