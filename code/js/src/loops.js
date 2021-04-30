"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.identifyAndRemoveLoops = void 0;
// Find loops from graph
const identifyAndRemoveLoops = (G, root) => {
    const queue = [root];
    const loops = [];
    // Use colors to keep track of visited nodes (white = not visited, grey = visiting, black = visited)
    const color = {};
    // Run traversal
    search(queue[0]);
    // Get successors of node
    function search(node) {
        // Color node as visiting
        color[node] = 'grey';
        const successors = G.successors(node);
        for (const successor of successors) {
            if (color[successor] === 'grey') {
                // If a loop is found, remove it from the Graph and continue
                G.removeEdge(node, successor);
                loops.push([node, successor]);
            }
            else {
                search(successor);
            }
        }
        // Once done, color node as visited
        color[node] = 'black';
    }
    return loops;
};
exports.identifyAndRemoveLoops = identifyAndRemoveLoops;
//# sourceMappingURL=loops.js.map